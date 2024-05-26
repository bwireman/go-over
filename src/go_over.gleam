import gleam/io
import gleam/list
import gleam/option
import gleam/string
import go_over/advisories
import go_over/packages
import go_over/retired
import go_over/warning
import shellout

pub fn main() {
  let args = shellout.arguments()
  let pull = list.any(args, fn(arg) { arg == "--skip" })
  let pkgs = packages.read_manifest("./manifest.toml")

  let vulnerable_packages =
    advisories.check_for_advisories(pkgs, !pull)
    |> list.map(fn(p) {
      case p {
        #(pkg, adv) -> warning.adv_to_warning(pkg, adv)
      }
    })

  let retired_packages =
    pkgs
    |> list.map(fn(pkg) {
      case retired.check_retired(pkg) {
        option.Some(ret) -> option.Some(#(pkg, ret))
        option.None -> option.None
      }
    })
    |> option.values
    |> list.map(fn(p) {
      case p {
        #(pkg, ret) -> warning.retired_to_warning(pkg, ret)
      }
    })

  case list.append(retired_packages, vulnerable_packages) {
    [] ->
      shellout.style(
        "All good!",
        with: shellout.color(["brightgreen"]),
        custom: [],
      )
    vulns -> {
      vulns
      |> list.map(warning.print)
      |> string.join("\n")
      |> shellout.style(with: shellout.color(["red"]), custom: [])
    }
  }
  |> io.print
}
