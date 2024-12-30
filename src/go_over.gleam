import gleam/function
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import go_over/advisories/advisories
import go_over/config.{type Config, Config}
import go_over/packages.{type Package}
import go_over/retired/outdated
import go_over/retired/retired
import go_over/util/constants
import go_over/util/print
import go_over/util/util.{has_flag}
import go_over/warning.{
  type Warning, Direct, Indirect, Retired, Vulnerable, Warning,
}
import gxyz/gxyz_function
import gxyz/gxyz_tuple
import shellout
import simplifile

type Flags {
  Flags(
    force: Bool,
    fake: Bool,
    outdated: Bool,
    ignore_indirect: Bool,
    format: option.Option(config.Format),
  )
}

fn merge_flags_and_config(flags: Flags, cfg: Config) -> Config {
  Config(
    dev_deps: cfg.dev_deps,
    cache: cfg.cache,
    force: flags.force,
    outdated: cfg.outdated || flags.outdated,
    ignore_indirect: cfg.ignore_indirect || flags.ignore_indirect,
    fake: flags.fake,
    format: option.unwrap(flags.format, cfg.format),
    ignore_packages: cfg.ignore_packages,
    ignore_severity: cfg.ignore_severity,
    ignore_ids: cfg.ignore_ids,
    ignore_dev_dependencies: cfg.ignore_dev_dependencies,
  )
}

fn spin_up(cfg: Config) -> Config {
  let args = shellout.arguments()

  let format =
    list.find(args, string.starts_with(_, "--format"))
    |> result.try(string.split_once(_, "="))
    |> result.map(gxyz_tuple.at2_1)
    |> result.map(config.parse_config_format)
    |> option.from_result()

  let flags =
    Flags(
      force: has_flag(args, "force"),
      outdated: has_flag(args, "outdated"),
      ignore_indirect: has_flag(args, "ignore-indirect"),
      fake: has_flag(args, "fake"),
      format: format,
    )

  merge_flags_and_config(flags, cfg)
}

fn get_vulnerable_packages(pkgs: List(Package), conf: Config) -> List(Warning) {
  advisories.check_for_advisories(pkgs, conf.force || !conf.cache)
  |> list.map(fn(p) {
    gxyz_tuple.map2_1(p, config.filter_advisory_ids(conf, _))
  })
  |> list.filter(fn(p) { gxyz_tuple.at2_1(p) == [] })
  |> list.flat_map(gxyz_tuple.apply_from2(_, warning.adv_to_warning))
}

fn get_retired_packages(pkgs: List(Package), conf: Config) -> List(Warning) {
  pkgs
  |> list.map(fn(pkg) {
    retired.check_retired(pkg, conf.force || !conf.cache)
    |> option.map(fn(ret) { #(pkg, ret) })
  })
  |> option.values()
  |> list.map(gxyz_tuple.apply_from2(_, warning.retired_to_warning))
}

fn get_outdated_packages(pkgs: List(Package), conf: Config) -> List(Warning) {
  pkgs
  |> list.map(fn(pkg) {
    case outdated.check_outdated(pkg, conf.force || !conf.cache) {
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
      |> io.print_error

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
      |> io.print_error
  }
  shellout.exit(1)
}

pub fn main() {
  let conf = spin_up(config.read_config("gleam.toml"))
  gxyz_function.ignore_result(
    !conf.cache,
    gxyz_function.freeze1(simplifile.delete, constants.go_over_path()),
  )

  let pkgs =
    packages.read_manifest("manifest.toml")
    |> config.filter_dev_dependencies(conf, _)
    |> config.filter_packages(conf, _)
    |> config.filter_indirect(conf, _)

  let vulnerable_packages = get_vulnerable_packages(pkgs, conf)
  let retired_packages = get_retired_packages(pkgs, conf)
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

  let warnings =
    list.append(retired_packages, vulnerable_packages)
    |> list.append(outdated_packages)
    |> config.filter_severity(conf, _)

  case warnings {
    [] -> print.success("All good! ✅")
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
