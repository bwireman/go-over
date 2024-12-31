import clip
import clip/flag
import clip/help
import clip/opt
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
    verbose: Bool,
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
    verbose: flags.verbose,
    format: option.unwrap(flags.format, cfg.format),
    ignore_packages: cfg.ignore_packages,
    ignore_severity: cfg.ignore_severity,
    ignore_ids: cfg.ignore_ids,
    ignore_dev_dependencies: cfg.ignore_dev_dependencies,
  )
}

fn spin_up(cfg: Config) -> Result(Config, String) {
  clip.command({
    use force <- clip.parameter
    use outdated <- clip.parameter
    use ignore_indirect <- clip.parameter
    use fake <- clip.parameter
    use verbose <- clip.parameter
    use format <- clip.parameter

    let flags =
      Flags(
        force:,
        outdated:,
        ignore_indirect:,
        fake:,
        verbose:,
        format: config.parse_config_format(format),
      )

    merge_flags_and_config(flags, cfg)
  })
  |> clip.flag(flag.help(
    flag.new("force"),
    "Force pulling new data even if the cached data is still valid",
  ))
  |> clip.flag(flag.help(
    flag.new("outdated"),
    "Additionally check if newer versions of dependencies exist",
  ))
  |> clip.flag(flag.help(
    flag.new("ignore_indirect"),
    "Ignore all warnings for indirect dependencies",
  ))
  |> clip.flag(flag.new("fake"))
  |> clip.flag(flag.help(
    flag.new("verbose"),
    "Print progress as packages are checked",
  ))
  |> clip.opt(
    opt.new("format")
    |> opt.default("")
    |> opt.help(
      "Specify the output format of any warnings, [minimal, verbose, json]",
    ),
  )
  |> clip.help(help.custom(config.help))
  |> clip.run(shellout.arguments())
}

fn get_vulnerable_packages(pkgs: List(Package), conf: Config) -> List(Warning) {
  advisories.check_for_advisories(pkgs, conf.force || !conf.cache, conf.verbose)
  |> list.map(fn(p) {
    gxyz_tuple.map2_1(p, config.filter_advisory_ids(conf, _))
  })
  |> list.filter(fn(p) { gxyz_tuple.at2_1(p) == [] })
  |> list.flat_map(gxyz_tuple.apply_from2(_, warning.adv_to_warning))
}

fn get_retired_packages(pkgs: List(Package), conf: Config) -> List(Warning) {
  pkgs
  |> list.map(fn(pkg) {
    retired.check_retired(pkg, conf.force || !conf.cache, conf.verbose)
    |> option.map(fn(ret) { #(pkg, ret) })
  })
  |> option.values()
  |> list.map(gxyz_tuple.apply_from2(_, warning.retired_to_warning))
}

fn get_outdated_packages(pkgs: List(Package), conf: Config) -> List(Warning) {
  pkgs
  |> list.map(fn(pkg) {
    case outdated.check_outdated(pkg, conf.force || !conf.cache, conf.verbose) {
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
  let conf = case spin_up(config.read_config("gleam.toml")) {
    Error(e) -> {
      io.println_error(e)
      shellout.exit(0)
      panic as "Unreachable, please create an issue in https://github.com/bwireman/go-over if you see this"
    }
    Ok(conf) -> conf
  }

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
