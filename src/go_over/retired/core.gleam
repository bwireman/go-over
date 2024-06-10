import filepath
@target(erlang)
import gleam/hackney
import gleam/hexpm.{type ReleaseRetirement}
@target(erlang)
import gleam/http/request
import gleam/option.{Some}
import go_over/packages.{type Package}
import go_over/util/constants
import go_over/util/util.{hard_fail}

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

pub fn print_ret(ret: ReleaseRetirement) -> String {
  let reason = hexpm.retirement_reason_to_string(ret.reason)
  case ret.message {
    Some(msg) -> reason <> ": " <> msg
    _ -> reason
  }
}

pub fn package_url(pkg: Package) {
  "https://hex.pm/api/packages/" <> pkg.name
}

pub fn release_url(pkg: Package) {
  package_url(pkg) <> "/releases/" <> pkg.version_raw
}

@target(javascript)
pub fn do_pull_hex(pkg: Package, url: String) -> String {
  url
  |> util.do_fetch()
  |> hard_fail("request to hex.pm for package: " <> pkg.name <> " failed")
}

@target(erlang)
pub fn do_pull_hex(pkg: Package, url: String) -> String {
  let resp =
    url
    |> request.to()
    |> hard_fail("request to hex.pm for package: " <> pkg.name <> " failed")
    |> request.prepend_header("accept", "application/json")
    |> hackney.send
    |> hard_fail("request to hex.pm for package: " <> pkg.name <> " failed")

  resp.body
}
