import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/string
import go_over/advisories
import go_over/constants
import go_over/packages
import go_over/print
import go_over/retired
import go_over/util.{iffnil, throwaway}
import go_over/warning.{type Warning, Warning}
import go_over/yaml
import shellout
import simplifile

type Flags {
  Flags(skip: Bool, force: Bool, fake: Bool)
}

fn spin_up() -> Flags {
  let assert Ok(_) = yaml.start()
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
  flags: Flags,
) -> List(Warning) {
  advisories.check_for_advisories(pkgs, !flags.skip)
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
  throwaway(flags.force, fn() { simplifile.delete(constants.go_over_path()) })
  iffnil(flags.fake, fn() {
    print_warnings([
      Warning(
        "Fake Retired Package",
        "0.1.2",
        "Retired",
        warning.Retired,
        "Critical",
        warning.Direct,
      ),
      Warning(
        "Fake Retired Package",
        "0.1.2",
        "Retired",
        warning.Retired,
        "High",
        warning.Direct,
      ),
      Warning(
        "Fake Retired Package",
        "0.1.2",
        "Retired",
        warning.Retired,
        "Moderate",
        warning.Direct,
      ),
      Warning(
        "Fake Vulnerable Package",
        "1.2.3",
        "Vulnerabe",
        warning.Vulnerable,
        "LOW",
        warning.Indirect,
      ),
    ])
  })

  let pkgs = packages.read_manifest("./manifest.toml")
  let vulnerable_packages = get_vulnerable_packages(pkgs, flags)
  let retired_packages = get_retired_packges(pkgs, flags)

  case list.append(retired_packages, vulnerable_packages) {
    [] -> print.success("✅ All good! ✨")
    vulns -> print_warnings(vulns)
  }
}
