import gleam/dynamic/decode
import gleam/json
import gleam/list
import go_over/hex/core
import go_over/hex/puller
import go_over/packages.{type Package}
import go_over/util/cache
import go_over/util/constants
import go_over/util/print
import gxyz/cli
import gxyz/function as gfunction
import gxyz/list as glist
import simplifile

fn pull_package_licenses(puller: puller.Puller, pkg: Package) -> Nil {
  print.progress("Fetching licenses: " <> pkg.name <> " from hex.pm")
  let pkg_path = core.package_licenses_path(pkg)
  let pkg_path_fail = core.pkg_pull_error(pkg, pkg_path)

  let _ = simplifile.delete(pkg_path)
  simplifile.create_directory_all(pkg_path)
  |> cli.hard_fail_with_msg(pkg_path_fail)

  let resp = core.do_pull_hex(puller, pkg, core.package_url(pkg))

  pkg
  |> core.package_licenses_filename()
  |> simplifile.write(resp)
  |> cli.hard_fail_with_msg(pkg_path_fail)
}

pub fn decode_licenses(data: String) -> Result(List(String), json.DecodeError) {
  let decoder = {
    use licenses <- decode.subfield(
      ["meta", "licenses"],
      decode.list(decode.string),
    )
    decode.success(licenses)
  }

  json.parse(data, decoder)
}

fn fetch_licenses(puller: puller.Puller, pkg: Package) -> List(String) {
  pkg
  |> core.package_licenses_path()
  |> cache.pull_if_not_cached(
    constants.hour,
    gfunction.freeze2(pull_package_licenses, puller, pkg),
    pkg.name <> ": package licenses",
  )

  let cached_file_name = core.package_licenses_filename(pkg)

  let resp =
    cached_file_name
    |> simplifile.read()
    |> cli.hard_fail_with_msg("failed to read " <> cached_file_name)

  cli.hard_fail_with_msg(
    decode_licenses(resp),
    "failed to parse " <> cached_file_name,
  )
}

pub type HexWarningSource {
  RejectedLicense(name: String)
}

pub fn package_licenses(puller: puller.Puller, pkg: Package) -> List(String) {
  fetch_licenses(puller, pkg)
}

pub fn rejected_license_sources(
  licenses: List(String),
  allowed_licenses: List(String),
) -> List(HexWarningSource) {
  glist.reject_contains(licenses, allowed_licenses)
  |> list.map(RejectedLicense)
}

pub fn get_hex_info(
  puller: puller.Puller,
  pkg: Package,
  allowed_licenses: List(String),
) -> List(HexWarningSource) {
  package_licenses(puller, pkg)
  |> rejected_license_sources(allowed_licenses)
}
