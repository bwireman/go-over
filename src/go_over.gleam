import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/string
import go_over/advisories
import go_over/constants
import go_over/packages
import go_over/retired
import go_over/util.{iff, throwaway}
import go_over/warning
import go_over/yaml
import shellout
import simplifile

pub fn main() {
  let assert Ok(_) = yaml.start()
  let args = shellout.arguments()
  let skip = list.any(args, fn(arg) { arg == "--skip" })
  let force = list.any(args, fn(arg) { arg == "--force" })
  let pkgs = packages.read_manifest("./manifest.toml")

  iff(
    force && skip,
    fn() {
      shellout.style(
        "Cannot specify both `--skip` & `--force`",
        with: shellout.color(["red"]),
        custom: [],
      )
      |> io.print
      shellout.exit(1)
    },
    Nil,
  )

  throwaway(force, fn() { simplifile.delete(constants.go_over_path()) })

  let vulnerable_packages =
    advisories.check_for_advisories(pkgs, !skip)
    |> list.map(fn(p) {
      case p {
        #(pkg, adv) -> warning.adv_to_warning(pkg, adv)
      }
    })

  let retired_packages =
    pkgs
    |> list.map(fn(pkg) {
      case retired.check_retired(pkg, !skip) {
        option.Some(ret) -> option.Some(#(pkg, ret))
        option.None -> option.None
      }
    })
    |> option.values
    |> list.map(fn(p) {
      let #(pkg, ret) = p
      warning.retired_to_warning(pkg, ret)
    })

  case list.append(retired_packages, vulnerable_packages) {
    [] ->
      shellout.style(
        "✅ All good! ✨\n\n",
        with: shellout.color(["brightgreen"]),
        custom: [],
      )
      |> io.print

    vulns -> {
      let len = list.length(vulns)
      {
        "⛔ "
        <> int.to_string(len)
        <> " WARNING(s) FOUND!\n-----------------------------------------------\n"
      }
      |> io.print

      vulns
      |> list.map(fn(w) {
        warning.print(w)
        |> shellout.style(with: shellout.color(["red"]), custom: [])
      })
      |> string.join("\n-----------------------------------------------\n")
      |> io.print

      shellout.exit(1)
    }
  }
}
