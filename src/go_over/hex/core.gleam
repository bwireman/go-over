import filepath
import gleam/hexpm.{type ReleaseRetirement}
import gleam/option.{Some}
import go_over/hex/puller
import go_over/packages.{type Package}
import go_over/util/constants
import gxyz/cli

pub fn release_path(pkg: packages.Package, global: Bool) -> String {
  constants.go_over_path(global)
  |> filepath.join("deps")
  |> filepath.join(pkg.name)
  |> filepath.join(pkg.version_raw)
}

pub fn hex_info_path(pkg: packages.Package, global: Bool) -> String {
  constants.go_over_path(global)
  |> filepath.join("hex-info")
  |> filepath.join(pkg.name)
  |> filepath.join(pkg.version_raw)
}

pub fn pkg_pull_error(pkg: packages.Package, pkg_path: String) {
  "could not store hex.pm pkg info. package: "
  <> pkg.name
  <> " at path "
  <> pkg_path
}

pub fn release_filename(pkg, global: Bool) -> String {
  pkg
  |> release_path(global)
  |> filepath.join("resp.json")
}

pub fn hex_info_filename(pkg, global: Bool) -> String {
  pkg
  |> hex_info_path(global)
  |> filepath.join("hex-info-resp.json")
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

pub fn do_pull_hex(pull: puller.Puller, pkg: Package, url: String) -> String {
  puller.run(pull, url)
  |> cli.custom_hard_fail([
    cli.FailOptMessage(
      "request to hex.pm for package: " <> pkg.name <> " failed",
    ),
    cli.FailOptEcho,
  ])
}
