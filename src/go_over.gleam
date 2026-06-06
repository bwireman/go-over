import filepath
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import go_over/advisories/advisories
import go_over/config.{type Config, type Flags}
import go_over/packages
import go_over/sarif
import go_over/sources
import go_over/util/constants
import go_over/util/globals
import go_over/util/print
import go_over/util/spinner
import go_over/util/util
import go_over/warning.{type Warning}
import go_over/workspace
import gxyz/function as gfunction
import shellout
import simplifile

pub type AuditResult {
  AuditResult(
    project_root: String,
    fatal_warnings: List(Warning),
    info_warnings: List(Warning),
    outdated_failed: Bool,
    format: config.Format,
  )
}

fn prefix_label(prefix: option.Option(String), label: String) -> String {
  case prefix {
    option.Some(path) -> "[" <> path <> "] " <> label
    option.None -> label
  }
}

fn print_warnings_output(
  vulns: List(Warning),
  conf: Config,
  label: String,
  prefix: option.Option(String),
) -> Nil {
  let label = prefix_label(prefix, label)
  io.print_error(label)

  case conf.format {
    config.Minimal ->
      vulns
      |> list.map(warning.format_as_string_minimal)
      |> string.join("")
      |> io.print_error()

    config.JSON -> Nil

    config.SARIF -> Nil

    _ ->
      vulns
      |> list.map(warning.format_as_string)
      |> string.join(constants.long_ass_dashes)
      |> io.print_error()
  }
}

fn warnings_label(vulns: List(Warning), kind: String) -> String {
  "⛔ "
  <> int.to_string(list.length(vulns))
  <> " "
  <> kind
  <> "(s) FOUND!"
  <> constants.long_ass_dashes
}

fn info_label(vulns: List(Warning)) -> String {
  "ℹ️ "
  <> int.to_string(list.length(vulns))
  <> " Item(s) of Note"
  <> constants.long_ass_dashes
}

fn warnings_for_json(result: AuditResult) -> List(Warning) {
  list.append(result.info_warnings, result.fatal_warnings)
}

pub fn warnings_for_json_results(results: List(AuditResult)) -> List(Warning) {
  list.flat_map(results, warnings_for_json)
}

pub fn print_json_warnings(warnings: List(Warning)) -> Nil {
  warnings
  |> list.map(warning.format_as_json)
  |> json.preprocessed_array()
  |> json.to_string()
  |> io.print_error()
}

fn write_sarif(
  results: List(AuditResult),
  output: option.Option(String),
) -> Nil {
  let runs =
    list.map(results, fn(result) {
      #(result.project_root, warnings_for_json(result))
    })

  let content =
    sarif.to_sarif_log(runs)
    |> json.to_string()

  case output {
    option.None -> content |> io.println()
    option.Some(path) ->
      case path |> simplifile.write(content) {
        Ok(Nil) -> Nil
        Error(_) -> {
          io.println_error("could not write SARIF output to " <> path)
          shellout.exit(1)
        }
      }
  }
}

pub fn skipped_workspace_warnings(skipped: List(String)) -> List(Warning) {
  list.map(skipped, fn(path) {
    warning.info_to_warning(
      path,
      "Info: project at '"
        <> path
        <> "' was skipped (exceeds workspace_max_depth)",
    )
  })
}

pub fn print_warnings(
  vulns: List(Warning),
  conf: Config,
  prefix: option.Option(String),
) -> Nil {
  print_warnings_output(vulns, conf, warnings_label(vulns, "WARNING"), prefix)
  shellout.exit(1)
}

fn read_project_config(project_root: String) -> Config {
  let gleam_toml = filepath.join(project_root, "gleam.toml")

  case simplifile.read(gleam_toml) {
    Ok(_) -> config.read_config(gleam_toml)
    Error(_) -> config.default_config()
  }
}

pub fn audit_project(
  flags: Flags,
  project_root: String,
) -> Result(AuditResult, String) {
  let manifest_toml = filepath.join(project_root, "manifest.toml")
  let project_config = read_project_config(project_root)

  use conf <- result.try(config.merge_flags_and_config(flags, project_config))

  globals.set_project_root(project_root)
  globals.set_verbose(conf.verbose)
  globals.set_use_global_cache(conf.global)
  globals.set_force(conf.force)

  let machine_output = conf.format == config.SARIF || conf.format == config.JSON
  let spinner = case machine_output {
    True -> option.None
    False -> spinner.new_spinner("Let's do this!")
  }

  gfunction.ignore_result(
    conf.force,
    gfunction.freeze1(simplifile.delete, globals.go_over_path()),
  )

  spinner.set_text_spinner(spinner, "Reading manifest")
  let manifest_pkgs = packages.read_manifest(manifest_toml)
  let pkgs_audited =
    manifest_pkgs
    |> config.filter_dev_dependencies(conf, _)
    |> config.filter_indirect(conf, _)

  let hex_pkgs =
    list.filter(pkgs_audited, fn(p) { p.source == packages.PackageSourceHex })

  spinner.set_text_spinner(
    spinner,
    "Checking packages: " <> print.raw("vulnerable", "red"),
  )
  let all_advisories = advisories.fetch_all()
  let vulnerable_warnings =
    sources.get_vulnerable_warnings(pkgs_audited, conf, all_advisories)

  spinner.set_text_spinner(
    spinner,
    "Checking packages: " <> print.raw("retired", "yellow"),
  )
  let retired_warnings = sources.get_retired_warnings(hex_pkgs, conf)

  let #(hex_warnings, dependency_licenses) = case conf.allowed_licenses {
    [] -> #([], [])
    _ -> {
      spinner.set_text_spinner(
        spinner,
        "Checking packages: " <> print.raw("licenses", "brightmagenta"),
      )

      sources.get_hex_warnings(hex_pkgs, conf)
    }
  }

  spinner.set_text_spinner(spinner, "Filtering warnings")
  let audit_warnings =
    list.append(retired_warnings, vulnerable_warnings)
    |> list.append(hex_warnings)

  let unnecessary_warnings =
    config.unnecessary_ignore_warnings(
      conf,
      manifest_pkgs,
      audit_warnings,
      dependency_licenses,
      all_advisories,
    )

  let fatal_warnings =
    audit_warnings
    |> config.filter_package_warnings(conf, _)
    |> config.filter_severity(conf, _)

  spinner.stop_spinner(spinner)

  let outdated_failed = case conf.outdated {
    False -> False
    True -> run_deps_outdated(project_root)
  }

  let info_warnings =
    list.append(
      unnecessary_warnings,
      warning.git_deps_to_warnings(manifest_pkgs),
    )

  Ok(AuditResult(
    project_root:,
    fatal_warnings:,
    info_warnings:,
    outdated_failed:,
    format: conf.format,
  ))
}

pub fn main() {
  case config.parse_flags(shellout.arguments()) {
    Error(message) -> io.println(message)
    Ok(flags) -> run(flags)
  }
}

fn print_no_issues_success(info_count: Int) -> Nil {
  case info_count {
    0 -> print.success("✅ No warnings found!")
    _ ->
      print.success(
        "✅ No security issues found ("
        <> int.to_string(info_count)
        <> " item(s) of note)",
      )
  }
}

fn run(flags: config.Flags) -> Nil {
  let workspace_mode = option.is_some(flags.workspace_root)

  let #(results, workspace_skipped) = case flags.workspace_root {
    option.Some(scan_root) -> {
      let scan_config = read_project_config(scan_root)
      let max_depth = scan_config.workspace_max_depth

      case workspace.discover_or_error(scan_root, max_depth) {
        Ok(workspace.DiscoverResult(projects, skipped)) -> #(
          list.map(projects, fn(project_root) {
            case audit_project(flags, project_root) {
              Ok(result) -> result
              Error(e) -> {
                io.println_error(e)
                shellout.exit(1)
                util.do_panic()
              }
            }
          }),
          skipped,
        )
        Error(e) -> {
          io.println_error(e)
          shellout.exit(1)
          util.do_panic()
        }
      }
    }
    option.None -> {
      let project_root = option.unwrap(flags.single_root, ".")
      let assert Ok(result) = audit_project(flags, project_root)
      #([result], [])
    }
  }

  let skipped_warnings = skipped_workspace_warnings(workspace_skipped)
  let results = case skipped_warnings {
    [] -> results
    _ -> {
      case results {
        [first, ..rest] -> [
          AuditResult(
            ..first,
            info_warnings: list.append(skipped_warnings, first.info_warnings),
          ),
          ..rest
        ]
        [] -> results
      }
    }
  }

  case workspace_mode, flags.format {
    True, option.None ->
      case
        config.validate_workspace_formats(
          list.map(results, fn(r) { #(r.format, r.project_root) }),
        )
      {
        Error(e) -> {
          io.println_error(e)
          shellout.exit(1)
          util.do_panic()
        }
        Ok(Nil) -> Nil
      }
    _, _ -> Nil
  }

  let prefix_for = fn(result: AuditResult) {
    case workspace_mode {
      True -> option.Some(result.project_root)
      False -> option.None
    }
  }

  let display_conf = fn(result: AuditResult) {
    config.Config(..config.default_config(), format: result.format)
  }

  let output_format = case results {
    [] -> config.Minimal
    [first, ..] -> first.format
  }

  let sarif_output = output_format == config.SARIF
  let json_output = output_format == config.JSON

  case flags.sarif_output, sarif_output {
    option.Some(_), False -> {
      io.println_error("--sarif-output requires --format sarif")
      shellout.exit(1)
    }
    _, _ -> Nil
  }

  case sarif_output, json_output {
    False, False ->
      list.each(results, fn(result) {
        case result.info_warnings {
          [] -> Nil
          info ->
            print_warnings_output(
              info,
              display_conf(result),
              info_label(info),
              prefix_for(result),
            )
        }
      })
    False, True -> print_json_warnings(warnings_for_json_results(results))
    True, _ -> Nil
  }

  let any_fatal = list.any(results, fn(r) { !list.is_empty(r.fatal_warnings) })
  let any_outdated_failed = list.any(results, fn(r) { r.outdated_failed })
  let info_count =
    list.flat_map(results, fn(r) { r.info_warnings }) |> list.length

  case any_fatal, any_outdated_failed, sarif_output, json_output {
    False, False, True, _ -> {
      write_sarif(results, flags.sarif_output)
      Nil
    }
    False, False, False, _ -> {
      print_no_issues_success(info_count)
      Nil
    }
    False, True, True, _ -> {
      write_sarif(results, flags.sarif_output)
      shellout.exit(1)
    }
    False, True, False, _ -> shellout.exit(1)
    True, _, True, _ -> {
      write_sarif(results, flags.sarif_output)
      shellout.exit(1)
    }
    True, _, False, True -> shellout.exit(1)
    True, _, False, False -> {
      list.each(results, fn(result) {
        case result.fatal_warnings {
          [] -> Nil
          vulns ->
            print_warnings_output(
              vulns,
              display_conf(result),
              warnings_label(vulns, "WARNING"),
              prefix_for(result),
            )
        }
      })
      shellout.exit(1)
    }
  }
}

fn run_deps_outdated(project_root: String) -> Bool {
  print.high(
    "The --outdated flag is deprecated. Use `gleam deps outdated` instead.",
  )

  case
    shellout.command(
      run: "gleam",
      with: ["deps", "outdated"],
      in: project_root,
      opt: [
        shellout.LetBeStdout,
      ],
    )
  {
    Ok(_) -> False
    Error(_) -> True
  }
}
