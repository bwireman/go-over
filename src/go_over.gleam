import gleam/function
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/string
import go_over/config.{type Config}
import go_over/packages
import go_over/sources
import go_over/util/constants
import go_over/util/print
import go_over/util/spinner
import go_over/warning.{type Warning}
import gxyz/function as gfunction
import gxyz/list as glist
import shellout
import simplifile

fn print_warnings_count(vulns: List(Warning)) -> Nil {
  {
    "⛔ "
    <> int.to_string(list.length(vulns))
    <> " WARNING(s) FOUND!"
    <> constants.long_ass_dashes
  }
  |> io.print_error()
}

pub fn print_warnings(vulns: List(Warning), conf: Config) -> Nil {
  case conf.format {
    config.Minimal ->
      vulns
      |> function.tap(print_warnings_count)
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
      |> function.tap(print_warnings_count)
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
      panic as "Unreachable, please create an issue in https://github.com/bwireman/go-over if you see this"
    }
    Ok(conf) -> conf
  }

  let spinner = spinner.new_spinner("Let's do this!", conf.verbose)
  gfunction.ignore_result(
    conf.force,
    gfunction.freeze1(simplifile.delete, constants.go_over_path(conf.global)),
  )

  spinner.set_text_spinner(spinner, "Reading manifest", conf.verbose)
  let pkgs =
    packages.read_manifest("manifest.toml")
    |> config.filter_dev_dependencies(conf, _)
    |> config.filter_packages(conf, _)
    |> config.filter_indirect(conf, _)

  spinner.set_text_spinner(
    spinner,
    "Checking packages: " <> print.raw("vulnerable", "red"),
    conf.verbose,
  )
  let vulnerable_warnings = sources.get_vulnerable_warnings(pkgs, conf)

  spinner.set_text_spinner(
    spinner,
    "Checking packages: " <> print.raw("retired", "yellow"),
    conf.verbose,
  )
  let retired_warnings =
    pkgs
    |> glist.reject(fn(p) { p.source == packages.PackageSourceGit })
    |> sources.get_retired_warnings(conf)

  let hex_warnings =
    gfunction.iff(
      conf.outdated || list.length(conf.allowed_licenses) > 0,
      fn() {
        let msg = case conf.outdated, list.length(conf.allowed_licenses) > 0 {
          True, True -> "outdated & licenses"
          True, False -> "outdated"
          False, True -> "licenses"
          False, False ->
            panic as "Unreachable, please create an issue in https://github.com/bwireman/go-over if you see this"
        }

        spinner.set_text_spinner(
          spinner,
          "Checking packages: " <> print.raw(msg, "brightmagenta"),
          conf.verbose,
        )

        pkgs
        |> glist.reject(fn(p) { p.source == packages.PackageSourceGit })
        |> sources.get_hex_warnings(conf)
      },
      [],
    )

  spinner.set_text_spinner(spinner, "Filtering warnings", conf.verbose)
  let warnings =
    list.append(retired_warnings, vulnerable_warnings)
    |> list.append(hex_warnings)
    |> config.filter_severity(conf, _)

  spinner.stop_spinner(spinner)
  case warnings {
    [] -> print.success("✅ No warnings found!")
    vulns -> print_warnings(vulns, conf)
  }
}
