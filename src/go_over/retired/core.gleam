import filepath
import gleam/hexpm.{type ReleaseRetirement}
import gleam/option.{Some}
import go_over/packages.{type Package}
import go_over/util/constants

pub fn path(pkg: packages.Package) -> String {
  constants.go_over_path()
  |> filepath.join("deps")
  |> filepath.join(pkg.name)
  |> filepath.join(pkg.version_raw)
}

pub fn filename(pkg) -> String {
  pkg
  |> path
  |> filepath.join("resp.json")
}

@target(javascript)
@external(javascript, "../../fetch.mjs", "do_fetch")
pub fn do_fetch(x: String) -> String

pub fn print_ret(ret: ReleaseRetirement) -> String {
  let reason = hexpm.retirement_reason_to_string(ret.reason)
  case ret.message {
    Some(msg) -> reason <> ": " <> msg
    _ -> reason
  }
}

pub fn hex_url(pkg: Package) {
  "https://hex.pm/api/packages/" <> pkg.name <> "/releases/" <> pkg.version_raw
}
