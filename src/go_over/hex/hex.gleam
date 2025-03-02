import gleam/dynamic.{type DecodeError, type Dynamic} as dyn
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option}
import gleam/order
import gleam/result
import gleamsver
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

pub type HexInfo {
  HexInfo(latest_stable_version: Option(String), licenses: List(String))
}

fn pull_hex_info(
  puller: puller.Puller,
  pkg: Package,
  verbose: Bool,
  global: Bool,
) -> Nil {
  print.progress(
    verbose,
    "Checking latest version: " <> pkg.name <> " From hex.pm",
  )
  let pkg_path = core.hex_info_path(pkg, global)
  let pkg_path_fail = core.pkg_pull_error(pkg, pkg_path)

  let _ = simplifile.delete(pkg_path)
  simplifile.create_directory_all(pkg_path)
  |> cli.hard_fail_with_msg(pkg_path_fail)

  let resp = core.do_pull_hex(puller, pkg, core.package_url(pkg))

  pkg
  |> core.hex_info_filename(global)
  |> simplifile.write(resp)
  |> cli.hard_fail_with_msg(pkg_path_fail)
}

pub fn decode_latest_stable_version_and_licenses(
  data: Dynamic,
) -> Result(HexInfo, List(DecodeError)) {
  let decoder = {
    use latest_stable_version <- decode.field(
      "latest_stable_version",
      decode.optional(decode.string),
    )
    use licenses <- decode.subfield(
      ["meta", "licenses"],
      decode.list(decode.string),
    )
    decode.success(HexInfo(latest_stable_version:, licenses:))
  }

  data
  |> decode.run(decoder)
  |> result.map_error(
    list.map(_, fn(og) { dyn.DecodeError(og.expected, og.found, og.path) }),
  )
}

fn pull(
  puller: puller.Puller,
  pkg: Package,
  force_pull: Bool,
  verbose: Bool,
  global: Bool,
) {
  pkg
  |> core.hex_info_path(global)
  |> cache.pull_if_not_cached(
    constants.hour,
    force_pull,
    verbose,
    gfunction.freeze4(pull_hex_info, puller, pkg, verbose, global),
    pkg.name <> ": latest stable version",
  )

  let cached_file_name = core.hex_info_filename(pkg, global)

  let resp =
    cached_file_name
    |> simplifile.read()
    |> cli.hard_fail_with_msg("failed to read " <> cached_file_name)

  cli.hard_fail_with_msg(
    json.decode(resp, decode_latest_stable_version_and_licenses),
    "failed to parse " <> cached_file_name,
  )
}

fn check_outdated(
  latest_version: String,
  pkg: Package,
  cached_file_name: String,
) {
  let latest_semver =
    gleamsver.parse(latest_version)
    |> cli.hard_fail_with_msg("failed to parse: " <> cached_file_name)

  case gleamsver.compare(latest_semver, pkg.version) {
    order.Gt -> option.Some(latest_version)
    _ -> option.None
  }
}

pub type HexWarningSource {
  RejectedLicense(name: String)
  Outdated(new_version: String)
}

pub fn get_hex_info(
  puller: puller.Puller,
  pkg: Package,
  force_pull: Bool,
  verbose: Bool,
  global: Bool,
  allowed_licenses: List(String),
) {
  let info = pull(puller, pkg, force_pull, verbose, global)
  let cached_file_name = core.hex_info_filename(pkg, global)

  let outdated =
    info.latest_stable_version
    |> option.map(check_outdated(_, pkg, cached_file_name))
    |> option.flatten()
    |> option.map(Outdated)

  let rejected_licenses =
    glist.reject_contains(info.licenses, allowed_licenses)
    |> list.map(RejectedLicense)

  case outdated {
    option.None -> rejected_licenses
    option.Some(outdated) -> [outdated, ..rejected_licenses]
  }
}
