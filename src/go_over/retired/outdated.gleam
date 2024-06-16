import gleam/dynamic.{type DecodeError, type Dynamic, DecodeError} as dyn
import gleam/function
import gleam/json
import gleam/option.{type Option}
import gleam/order
import gleamsver
import go_over/packages.{type Package}
import go_over/retired/core
import go_over/util/cache
import go_over/util/constants
import go_over/util/print
import go_over/util/util.{hard_fail}
import simplifile

fn pull_outdated(pkg: Package) -> Nil {
  print.progress("Checking latest version: " <> pkg.name <> " From hex.pm")
  let pkg_path = core.outdated_path(pkg)
  let pkg_path_fail = core.pkg_pull_error(pkg, pkg_path)

  let _ = simplifile.delete(pkg_path)
  simplifile.create_directory_all(pkg_path)
  |> hard_fail(pkg_path_fail)

  let resp = core.do_pull_hex(pkg, core.package_url(pkg))

  pkg
  |> core.outdated_filename
  |> simplifile.write(resp)
  |> hard_fail(pkg_path_fail)
}

fn decode_latest_stable_version(
  data: Dynamic,
) -> Result(Option(String), List(DecodeError)) {
  dyn.decode1(
    function.identity,
    dyn.field("latest_stable_version", dyn.optional(dyn.string)),
  )(data)
}

pub fn check_outdated(pkg: Package, force_pull: Bool) -> Option(String) {
  pkg
  |> core.outdated_path()
  |> cache.pull_if_not_cached(
    constants.hour,
    force_pull,
    util.freeze1(pull_outdated, pkg),
    pkg.name <> ": latest stable version",
  )

  let cached_file_name = core.outdated_filename(pkg)
  let resp =
    cached_file_name
    |> simplifile.read()
    |> hard_fail("failed to read " <> cached_file_name)

  {
    use latest_version <- option.map(hard_fail(
      json.decode(resp, decode_latest_stable_version),
      "failed to parse " <> cached_file_name,
    ))

    let latest_semver =
      gleamsver.parse(latest_version)
      |> hard_fail("failed to parse: " <> cached_file_name)

    case gleamsver.compare(latest_semver, pkg.version) {
      order.Gt -> option.Some(latest_version)
      _ -> option.None
    }
  }
  |> option.flatten
}
