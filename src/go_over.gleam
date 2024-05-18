import gleam/io
import gleam/list
import gleam/string
import go_over/advisories
import shellout

pub fn main() {
  let args = shellout.arguments()
  let pull = list.any(args, fn(arg) { arg == "--skip" })

  case advisories.check_for_advisories("./manifest.toml", !pull) {
    [] ->
      shellout.style(
        "All good!",
        with: shellout.color(["brightgreen"]),
        custom: [],
      )
    vulns -> {
      let warnings =
        vulns
        |> list.map(advisories.print_adv)
        |> string.join("--- \n\n")

      shellout.style(warnings, with: shellout.color(["brightred"]), custom: [])
    }
  }
  |> io.print
}
