import gleam/function
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import go_over/config.{type Config}
import go_over/packages
import go_over/sources
import go_over/util/constants
import go_over/util/print
import go_over/util/spinner
import go_over/warning.{
  type Warning, Direct, Indirect, SeverityCritical, SeverityHigh, SeverityLow,
  SeverityModerate, SeverityPackageRetiredSecurity, SeverityRejectedLicense,
  Warning, WarningReasonRetired, WarningReasonVulnerable,
}
import gxyz/function as gfunction
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

fn print_warnings(vulns: List(Warning), conf: Config) -> Nil {
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
  let retired_warnings = sources.get_retired_warnings(pkgs, conf)

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

        sources.get_hex_warnings(pkgs, conf)
      },
      [],
    )

  gfunction.iff_nil(
    conf.fake,
    gfunction.freeze2(print_warnings, example_warnings, conf),
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

const example_warnings = [
  Warning(
    None,
    "fake",
    Some("x.y.z"),
    "Retired",
    WarningReasonVulnerable,
    SeverityCritical,
    Direct,
  ),
  Warning(
    None,
    "another_fake",
    Some("1.2.3"),
    "Vulnerable",
    WarningReasonVulnerable,
    SeverityHigh,
    Direct,
  ),
  Warning(
    None,
    "and_another",
    Some("4.5.6"),
    "Vulnerable",
    WarningReasonVulnerable,
    SeverityModerate,
    Direct,
  ),
  Warning(
    None,
    "one_more",
    Some("7.8.9"),
    "Vulnerable",
    WarningReasonVulnerable,
    SeverityLow,
    Indirect,
  ),
  Warning(
    None,
    "this_one_was_retired",
    Some("10.11.12"),
    "Retired",
    WarningReasonRetired,
    SeverityPackageRetiredSecurity,
    Indirect,
  ),
  Warning(
    None,
    "rejected_license",
    None,
    "Retired",
    WarningReasonRetired,
    SeverityRejectedLicense,
    Indirect,
  ),
]
