import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/string
import go_over/config.{type Config}
import go_over/hex/hex
import go_over/packages
import go_over/sources
import go_over/util/constants
import go_over/util/globals
import go_over/util/print
import go_over/util/spinner
import go_over/util/util
import go_over/warning.{type Warning}
import gxyz/function as gfunction
import shellout
import simplifile

fn print_warnings_count(vulns: List(Warning)) -> List(Warning) {
  {
    "⛔ "
    <> int.to_string(list.length(vulns))
    <> " WARNING(s) FOUND!"
    <> constants.long_ass_dashes
  }
  |> io.print_error()
  vulns
}

pub fn print_warnings(vulns: List(Warning), conf: Config) -> Nil {
  case conf.format {
    config.Minimal ->
      vulns
      |> print_warnings_count
      |> list.map(warning.format_as_string_minimal)
      |> string.join("")
      |> io.print_error()

    config.JSON ->
      vulns
      |> list.map(warning.format_as_json)
      |> json.preprocessed_array()
      |> json.to_string()
      |> io.print_error()

    _ ->
      vulns
      |> print_warnings_count
      |> list.map(warning.format_as_string)
      |> string.join(constants.long_ass_dashes)
      |> io.print_error()
  }
  shellout.exit(1)
}

pub fn main() {
  let conf = case
    config.spin_up(config.read_config("gleam.toml"), shellout.arguments())
  {
    Error(e) -> {
      io.println_error(e)
      shellout.exit(0)
      util.do_panic()
    }
    Ok(conf) -> conf
  }

  globals.set_verbose(conf.verbose)
  globals.set_use_global_cache(conf.global)
  globals.set_force(conf.force)

  let spinner = spinner.new_spinner("Let's do this!")
  gfunction.ignore_result(
    conf.force,
    gfunction.freeze1(simplifile.delete, globals.go_over_path()),
  )

  spinner.set_text_spinner(spinner, "Reading manifest")
  let manifest_pkgs = packages.read_manifest("manifest.toml")
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

  let warnings =
    audit_warnings
    |> config.filter_package_warnings(conf, _)
    |> config.filter_severity(conf, _)
    |> list.append(unnecessary_warnings)

  spinner.stop_spinner(spinner)

  let outdated_failed = case conf.outdated {
    False -> False
    True -> run_deps_outdated()
  }

  case warnings, outdated_failed {
    [], False -> print.success("✅ No warnings found!")
    [], True -> shellout.exit(1)
    vulns, _ -> print_warnings(vulns, conf)
  }
}

fn run_deps_outdated() -> Bool {
  print.warning("The --outdated flag is deprecated. Use `gleam deps outdated` instead.")

  case
    shellout.command(run: "gleam", with: ["deps", "outdated"], in: ".", opt: [
      shellout.LetBeStdout,
    ])
  {
    Ok(_) -> False
    Error(_) -> True
  }
}
