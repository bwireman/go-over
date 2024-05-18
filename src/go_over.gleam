import gleam/io
import gleam/list
import go_over/advisories
import shellout

pub fn main() {
  let args = shellout.arguments()
  let pull = list.any(args, fn(arg) { arg == "--skip" })

  advisories.check_for_advisories("./manifest.toml", !pull)
  |> io.debug
}
