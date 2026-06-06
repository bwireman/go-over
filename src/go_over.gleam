import filepath
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import go_over/config.{type Config, type Flags}
import go_over/hex/hex
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

fn print_warnings_count(vulns: List(Warning), label: String) -> List(Warning) {
  label |> io.print_error()
  vulns
}

fn print_warnings_list(
  vulns: List(Warning),
  conf: Config,
  label: String,
  prefix: option.Option(String),
) -> Nil {
  let label = prefix_label(prefix, warnings_label(vulns, label))

  case conf.format {
    config.Minimal ->
      vulns
      |> print_warnings_count(label)
      |> list.map(warning.format_as_string_minimal)
      |> string.join("")
      |> io.print_error()

    config.JSON ->
      vulns
      |> list.map(warning.format_as_json)
      |> json.preprocessed_array()
      |> json.to_string()
      |> io.print_error()

    config.SARIF -> Nil

    _ ->
      vulns
      |> print_warnings_count(label)
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
  "ℹ️  "
  <> int.to_string(list.length(vulns))
  <> " Item(s) of Note"
  <> constants.long_ass_dashes
}

fn print_info_list(
  vulns: List(Warning),
  conf: Config,
  prefix: option.Option(String),
) -> Nil {
  let label = prefix_label(prefix, info_label(vulns))

  case conf.format {
    config.Minimal ->
      vulns
      |> print_warnings_count(label)
      |> list.map(warning.format_as_string_minimal)
      |> string.join("")
      |> io.print_error()

    config.JSON ->
      vulns
      |> list.map(warning.format_as_json)
      |> json.preprocessed_array()
      |> json.to_string()
      |> io.print_error()

    config.SARIF -> Nil

    _ ->
      vulns
      |> print_warnings_count(label)
      |> list.map(warning.format_as_string)
      |> string.join(constants.long_ass_dashes)
      |> io.print_error()
  }
}

fn print_sarif(results: List(AuditResult)) -> Nil {
  let runs =
    list.map(results, fn(result) {
      #(result.project_root, result.fatal_warnings)
    })

  sarif.to_sarif_log(runs)
  |> json.to_string()
  |> io.print_error()
}

pub fn print_info(
  vulns: List(Warning),
  conf: Config,
  prefix: option.Option(String),
) -> Nil {
  print_info_list(vulns, conf, prefix)
}

pub fn print_warnings(
  vulns: List(Warning),
  conf: Config,
  prefix: option.Option(String),
) -> Nil {
  print_warnings_list(vulns, conf, "WARNING", prefix)
  shellout.exit(1)
}

pub fn audit_project(
  flags: Flags,
  project_root: String,
) -> Result(AuditResult, String) {
  let gleam_toml = filepath.join(project_root, "gleam.toml")
  let manifest_toml = filepath.join(project_root, "manifest.toml")

  let project_config = case simplifile.read(gleam_toml) {
    Ok(_) -> config.read_config(gleam_toml)
    Error(_) -> config.default_config()
  }

  use conf <- result.try(config.merge_flags_and_config(flags, project_config))

  globals.set_project_root(project_root)
  globals.set_verbose(conf.verbose)
  globals.set_use_global_cache(conf.global)
  globals.set_force(conf.force)

  let spinner = spinner.new_spinner("Let's do this!")
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
  let vulnerable_warnings = sources.get_vulnerable_warnings(pkgs_audited, conf)

  spinner.set_text_spinner(
    spinner,
    "Checking packages: " <> print.raw("retired", "yellow"),
  )
  let retired_warnings = sources.get_retired_warnings(hex_pkgs, conf)

  let hex_warnings =
    gfunction.iff(
      !list.is_empty(conf.allowed_licenses),
      fn() {
        spinner.set_text_spinner(
          spinner,
          "Checking packages: " <> print.raw("licenses", "brightmagenta"),
        )

        sources.get_hex_warnings(hex_pkgs, conf)
      },
      [],
    )

  let dependency_licenses =
    gfunction.iff(
      !list.is_empty(conf.allowed_licenses),
      fn() { list.flat_map(hex_pkgs, hex.package_licenses(conf.puller, _)) },
      [],
    )

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
    list.append(unnecessary_warnings, warning.git_deps_to_warnings(manifest_pkgs))

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

fn run(flags: config.Flags) -> Nil {
  let workspace_mode = option.is_some(flags.workspace_root)

  let results = case flags.workspace_root {
    option.Some(scan_root) ->
      case workspace.discover_or_error(scan_root) {
        Ok(project_roots) ->
          list.map(project_roots, fn(project_root) {
            case audit_project(flags, project_root) {
              Ok(result) -> result
              Error(e) -> {
                io.println_error(e)
                shellout.exit(1)
                util.do_panic()
              }
            }
          })
        Error(e) -> {
          io.println_error(e)
          shellout.exit(1)
          util.do_panic()
        }
      }
    option.None -> {
      let project_root = option.unwrap(flags.single_root, ".")
      let assert Ok(result) = audit_project(flags, project_root)
      [result]
    }
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

  let sarif_output = list.any(results, fn(r) { r.format == config.SARIF })

  case sarif_output {
    False ->
      list.each(results, fn(result) {
        case result.info_warnings {
          [] -> Nil
          info -> print_info(info, display_conf(result), prefix_for(result))
        }
      })
    True -> Nil
  }

  let any_fatal = list.any(results, fn(r) { !list.is_empty(r.fatal_warnings) })
  let any_outdated_failed = list.any(results, fn(r) { r.outdated_failed })

  case any_fatal, any_outdated_failed, sarif_output {
    False, False, True -> {
      print_sarif(results)
      Nil
    }
    False, False, False -> print.success("✅ No warnings found!")
    False, True, True -> {
      print_sarif(results)
      shellout.exit(1)
    }
    False, True, False -> shellout.exit(1)
    True, _, True -> {
      print_sarif(results)
      shellout.exit(1)
    }
    True, _, False -> {
      list.each(results, fn(result) {
        case result.fatal_warnings {
          [] -> Nil
          vulns ->
            print_warnings_list(
              vulns,
              display_conf(result),
              "WARNING",
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
