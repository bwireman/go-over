import gleam/function
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import go_over/advisories/advisories
import go_over/config.{type Config, Config}
import go_over/packages.{type Package}
import go_over/retired/outdated
import go_over/retired/retired
import go_over/util/constants
import go_over/util/print
import go_over/util/spinner
import go_over/warning.{
  type Warning, Direct, Indirect, Retired, Vulnerable, Warning,
}
import gxyz/gxyz_function
import gxyz/gxyz_tuple
import shellout
import simplifile

fn get_vulnerable_packages(pkgs: List(Package), conf: Config) -> List(Warning) {
  advisories.check_for_advisories(pkgs, conf.force, conf.verbose, conf.global)
  |> list.map(fn(p) {
    gxyz_tuple.map2_1(p, config.filter_advisory_ids(conf, _))
  })
  |> list.filter(fn(p) { gxyz_tuple.at2_1(p) == [] })
  |> list.flat_map(gxyz_tuple.apply_from2(_, warning.adv_to_warning))
}

fn get_retired_packages(pkgs: List(Package), conf: Config) -> List(Warning) {
  pkgs
  |> list.map(fn(pkg) {
    retired.check_retired(pkg, conf.force, conf.verbose, conf.global)
    |> option.map(fn(ret) { #(pkg, ret) })
  })
  |> option.values()
  |> list.map(gxyz_tuple.apply_from2(_, warning.retired_to_warning))
}

fn get_outdated_packages(pkgs: List(Package), conf: Config) -> List(Warning) {
  pkgs
  |> list.map(fn(pkg) {
    case outdated.check_outdated(pkg, conf.force, conf.verbose, conf.global) {
      Some(ret) -> Some(#(pkg, ret))
      None -> None
    }
  })
  |> option.values()
  |> list.map(gxyz_tuple.apply_from2(_, warning.outdated_to_warning))
}

fn print_warnings_count(vulns: List(Warning)) -> Nil {
  {
    "⛔ "
    <> int.to_string(list.length(vulns))
    <> " WARNING(s) FOUND!"
    <> constants.long_ass_dashes
  }
  |> io.print_error
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
  gxyz_function.ignore_result(
    conf.force,
    gxyz_function.freeze1(
      simplifile.delete,
      constants.go_over_path(conf.global),
    ),
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
  let vulnerable_packages = get_vulnerable_packages(pkgs, conf)

  spinner.set_text_spinner(
    spinner,
    "Checking packages: " <> print.raw("retired", "yellow"),
    conf.verbose,
  )
  let retired_packages = get_retired_packages(pkgs, conf)

  spinner.set_text_spinner(
    spinner,
    "Checking packages: " <> print.raw("outdated", "brightmagenta"),
    conf.verbose,
  )
  let outdated_packages =
    gxyz_function.iff(
      conf.outdated,
      gxyz_function.freeze2(get_outdated_packages, pkgs, conf),
      [],
    )

  gxyz_function.iff_nil(
    conf.fake,
    gxyz_function.freeze2(print_warnings, example_warnings, conf),
  )

  spinner.set_text_spinner(spinner, "Filtering warnings", conf.verbose)
  let warnings =
    list.append(retired_packages, vulnerable_packages)
    |> list.append(outdated_packages)
    |> config.filter_severity(conf, _)

  spinner.stop_spinner(spinner)
  case warnings {
    [] -> print.success("✅ No warnings found!")
    vulns -> print_warnings(vulns, conf)
  }
}

const example_warnings = [
  Warning(None, "fake", "x.y.z", "Retired", Vulnerable, "Critical", Direct),
  Warning(
    None,
    "another_fake",
    "1.2.3",
    "Vulnerable",
    Vulnerable,
    "High",
    Direct,
  ),
  Warning(
    None,
    "and_another",
    "4.5.6",
    "Vulnerable",
    Vulnerable,
    "Moderate",
    Direct,
  ),
  Warning(None, "one_more", "7.8.9", "Vulnerable", Vulnerable, "LOW", Indirect),
  Warning(
    None,
    "this_one_was_retired",
    "10.11.12",
    "Retired",
    Retired,
    "Package Retired",
    Indirect,
  ),
]
