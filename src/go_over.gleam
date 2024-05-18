import gleam/io
import go_over/advisories

pub fn main() {
  let assert Ok(_) = advisories.clone()
  advisories.read_manifest("./manifest.toml")
  |> io.debug
}
