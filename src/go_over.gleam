import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import go_over/advisories/advisories
import go_over/config.{type Config, Config}
import go_over/packages
import go_over/retired/outdated
import go_over/retired/retired
import go_over/util/constants
import go_over/util/print
import go_over/util/util.{has_flag, iff_nil, throwaway}
import go_over/warning.{type Warning, Warning}
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

fn merge_flags_and_config(flgs: Flags, cfg: Config) -> Config {
  Config(
    cache: cfg.cache,
    force: flgs.force,
    outdated: cfg.outdated || flgs.outdated,
    ignore_indirect: cfg.ignore_indirect || flgs.ignore_indirect,
    fake: flgs.fake,
    format: option.unwrap(flgs.format, cfg.format),
    ignore_packages: cfg.ignore_packages,
    ignore_severity: cfg.ignore_severity,
    ignore_ids: cfg.ignore_ids,
  )
}

fn spin_up(cfg: Config) -> Config {
  let args = shellout.arguments()

  let format =
    list.find(args, fn(arg) { string.starts_with(arg, "--format") })
    |> result.try(fn(arg) { string.split_once(arg, "=") })
    |> result.map(fn(arg) {
      case arg {
        #(_, val) -> val
      }
    })
    |> result.map(config.parse_config_format)
    |> option.from_result

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

fn get_vulnerable_packages(
  pkgs: List(packages.Package),
  conf: Config,
) -> List(Warning) {
  advisories.check_for_advisories(pkgs, conf.force || !conf.cache)
  |> list.map(fn(p) {
    let #(pkg, adv) = p

    #(pkg, config.filter_advisory_ids(conf, adv))
  })
  |> list.filter(fn(p) {
    case p {
      #(_, []) -> False
      _ -> True
    }
  })
  |> list.flat_map(fn(p) {
    let #(pkg, adv) = p

    warning.adv_to_warning(pkg, adv)
  })
}

fn get_retired_packges(
  pkgs: List(packages.Package),
  conf: Config,
) -> List(Warning) {
  pkgs
  |> list.map(fn(pkg) {
    case retired.check_retired(pkg, conf.force || !conf.cache) {
      option.Some(ret) -> option.Some(#(pkg, ret))
      option.None -> option.None
    }
  })
  |> option.values
  |> list.map(fn(p) {
    let #(pkg, ret) = p
    warning.retired_to_warning(pkg, ret)
  })
}

fn get_outdated_packages(
  pkgs: List(packages.Package),
  conf: Config,
) -> List(Warning) {
  pkgs
  |> list.map(fn(pkg) {
    case outdated.check_outdated(pkg, conf.force || !conf.cache) {
      option.Some(ret) -> option.Some(#(pkg, ret))
      option.None -> option.None
    }
  })
  |> option.values
  |> list.map(fn(p) {
    let #(pkg, ret) = p
    warning.outdated_to_warning(pkg, ret)
  })
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
    config.Minimal -> {
      print_warnings_count(vulns)

      vulns
      |> list.map(warning.format_as_string_minimal)
      |> string.join("")
      |> io.print_error
    }

    config.JSON -> {
      vulns
      |> list.map(warning.format_as_json)
      |> json.preprocessed_array
      |> json.to_string
      |> io.print_error
    }

    _ -> {
      print_warnings_count(vulns)

      vulns
      |> list.map(warning.format_as_string)
      |> string.join(constants.long_ass_dashes)
      |> io.print_error
    }
  }
  shellout.exit(1)
}

pub fn main() {
  let conf = spin_up(config.read_config("./gleam.toml"))
  throwaway(
    !conf.cache,
    util.freeze1(simplifile.delete, constants.go_over_path()),
  )

  let pkgs =
    packages.read_manifest("./manifest.toml")
    |> config.filter_packages(conf, _)
    |> config.filter_indirect(conf, _)

  let vulnerable_packages = get_vulnerable_packages(pkgs, conf)
  let retired_packages = get_retired_packges(pkgs, conf)
  let outdated_packages = case conf.outdated {
    True -> get_outdated_packages(pkgs, conf)
    False -> []
  }

  iff_nil(
    conf.fake,
    util.freeze2(
      print_warnings,
      [
        Warning(
          option.None,
          "fake",
          "x.y.z",
          "Retired",
          warning.Vulnerable,
          "Critical",
          warning.Direct,
        ),
        Warning(
          option.None,
          "another_fake",
          "1.2.3",
          "Vulnerable",
          warning.Vulnerable,
          "High",
          warning.Direct,
        ),
        Warning(
          option.None,
          "and_another",
          "4.5.6",
          "Vulnerable",
          warning.Vulnerable,
          "Moderate",
          warning.Direct,
        ),
        Warning(
          option.None,
          "one_more",
          "7.8.9",
          "Vulnerable",
          warning.Vulnerable,
          "LOW",
          warning.Indirect,
        ),
        Warning(
          option.None,
          "this_one_was_retired",
          "10.11.12",
          "Retired",
          warning.Retired,
          "Package Retired",
          warning.Indirect,
        ),
      ],
      conf,
    ),
  )

  let warnings =
    list.append(retired_packages, vulnerable_packages)
    |> list.append(outdated_packages)
    |> config.filter_severity(conf, _)

  case warnings {
    [] -> print.success("✅ All good! ✨")
    vulns -> print_warnings(vulns, conf)
  }
}
