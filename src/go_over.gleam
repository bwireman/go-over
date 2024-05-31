import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/string
import go_over/advisories
import go_over/constants
import go_over/packages
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
    shellout.style(
      "Cannot specify both `--skip` & `--force`\n\n",
      with: shellout.color(["red"]),
      custom: [],
    )
    |> io.print
    shellout.exit(1)
  })

  flags
}

fn get_vulnerable_packages(pkgs: List(packages.Package), flags: Flags) {
  advisories.check_for_advisories(pkgs, !flags.skip)
  |> list.map(fn(p) {
    let #(pkg, adv) = p

    warning.adv_to_warning(pkg, adv)
  })
}

fn get_retired_packges(pkgs: List(packages.Package), flags: Flags) {
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

fn print_warnings(vulns: List(Warning)) {
  let len = list.length(vulns)
  {
    "⛔ "
    <> int.to_string(len)
    <> " WARNING(s) FOUND!"
    <> constants.long_ass_dashes
  }
  |> io.print

  let warn =
    vulns
    |> list.map(fn(w) {
      shellout.style(
        warning.print(w),
        with: shellout.color(["red"]),
        custom: [],
      )
    })
    |> string.join(constants.long_ass_dashes)

  io.print_error(warn <> "\n\n")

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
        warning.Direct,
      ),
      Warning(
        "Fake Vulnerable Package",
        "1.2.3",
        "Vulnerabe",
        warning.Vulnerable,
        warning.Indirect,
      ),
    ])
  })

  let pkgs = packages.read_manifest("./manifest.toml")
  let vulnerable_packages = get_vulnerable_packages(pkgs, flags)
  let retired_packages = get_retired_packges(pkgs, flags)

  case list.append(retired_packages, vulnerable_packages) {
    [] ->
      shellout.style(
        "✅ All good! ✨\n\n",
        with: shellout.color(["brightgreen"]),
        custom: [],
      )
      |> io.print

    vulns -> print_warnings(vulns)
  }
}
