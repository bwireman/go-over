import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/string
import go_over/advisories/advisories
import go_over/config.{type Config}
import go_over/packages
import go_over/retired
import go_over/util/constants
import go_over/util/print
import go_over/util/util.{iffnil, throwaway}
import go_over/warning.{type Warning, Warning}
import shellout
import simplifile

type Flags {
  Flags(skip: Bool, force: Bool, fake: Bool)
}

fn spin_up() -> Flags {
  let args = shellout.arguments()
  let flags =
    Flags(
      skip: list.any(args, fn(arg) { arg == "--skip" }),
      force: list.any(args, fn(arg) { arg == "--force" }),
      fake: list.any(args, fn(arg) { arg == "--fake" }),
    )

  iffnil(flags.force && flags.skip, fn() {
    print.warning("Cannot specify both `--skip` & `--force`")
    shellout.exit(1)
  })

  flags
}

fn get_vulnerable_packages(
  pkgs: List(packages.Package),
  conf: Config,
  flags: Flags,
) -> List(Warning) {
  advisories.check_for_advisories(pkgs, !flags.skip)
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
  |> list.map(fn(p) {
    let #(pkg, adv) = p

    warning.adv_to_warning(pkg, adv)
  })
}

fn get_retired_packges(
  pkgs: List(packages.Package),
  flags: Flags,
) -> List(Warning) {
  pkgs
  |> list.map(fn(pkg) {
    case retired.check_retired(pkg, !flags.skip) {
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

fn print_warnings(vulns: List(Warning)) -> Nil {
  {
    "⛔ "
    <> int.to_string(list.length(vulns))
    <> " WARNING(s) FOUND!"
    <> constants.long_ass_dashes
  }
  |> io.print

  vulns
  |> list.map(warning.format_as_string)
  |> string.join(constants.long_ass_dashes)
  |> io.print
  shellout.exit(1)
}

pub fn main() {
  let flags = spin_up()
  let conf = config.read_config("./gleam.toml")
  iffnil(!conf.cache && flags.skip, fn() {
    print.warning("Cannot specify both `--skip` & `cache=false`")
    shellout.exit(1)
  })

  throwaway(flags.force || !conf.cache, fn() {
    simplifile.delete(constants.go_over_path())
  })

  let pkgs =
    packages.read_manifest("./manifest.toml")
    |> config.filter_packages(conf, _)

  let vulnerable_packages = get_vulnerable_packages(pkgs, conf, flags)
  let retired_packages = get_retired_packges(pkgs, flags)

  iffnil(flags.fake, fn() {
    print_warnings([
      Warning(
        "fake",
        "x.y.z",
        "Retired",
        warning.Vulnerable,
        "Critical",
        warning.Direct,
      ),
      Warning(
        "another_fake",
        "1.2.3",
        "Vulnerabe",
        warning.Vulnerable,
        "High",
        warning.Direct,
      ),
      Warning(
        "and_another",
        "4.5.6",
        "Vulnerabe",
        warning.Vulnerable,
        "Moderate",
        warning.Direct,
      ),
      Warning(
        "one_more",
        "7.8.9",
        "Vulnerabe",
        warning.Vulnerable,
        "LOW",
        warning.Indirect,
      ),
      Warning(
        "this_one_was_retired",
        "10.11.12",
        "Retired",
        warning.Retired,
        "Package Retired",
        warning.Indirect,
      ),
    ])
  })

  let warnings =
    list.append(retired_packages, vulnerable_packages)
    |> config.filter_severity(conf, _)

  case warnings {
    [] -> print.success("✅ All good! ✨")
    vulns -> print_warnings(vulns)
  }
}
