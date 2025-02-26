import gleam/dynamic.{type DecodeError, type Dynamic} as dyn
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option}
import gleam/order
import gleam/result
import gleamsver
import go_over/packages.{type Package}
import go_over/retired/core
import go_over/util/cache
import go_over/util/constants
import go_over/util/print
import gxyz/cli
import gxyz/function as gfunction
import simplifile

fn pull_outdated(pkg: Package, verbose: Bool, global: Bool) -> Nil {
  print.progress(
    verbose,
    "Checking latest version: " <> pkg.name <> " From hex.pm",
  )
  let pkg_path = core.outdated_path(pkg, global)
  let pkg_path_fail = core.pkg_pull_error(pkg, pkg_path)

  let _ = simplifile.delete(pkg_path)
  simplifile.create_directory_all(pkg_path)
  |> cli.hard_fail_with_msg(pkg_path_fail)

  let resp = core.do_pull_hex(pkg, core.package_url(pkg))

  pkg
  |> core.outdated_filename(global)
  |> simplifile.write(resp)
  |> cli.hard_fail_with_msg(pkg_path_fail)
}

fn decode_latest_stable_version(
  data: Dynamic,
) -> Result(Option(String), List(DecodeError)) {
  let decoder = {
    use latest_stable_version <- decode.field(
      "latest_stable_version",
      decode.optional(decode.string),
    )
    decode.success(latest_stable_version)
  }

  data
  |> decode.run(decoder)
  |> result.map_error(
    list.map(_, fn(og) { dyn.DecodeError(og.expected, og.found, og.path) }),
  )
}

pub fn check_outdated(
  pkg: Package,
  force_pull: Bool,
  verbose: Bool,
  global: Bool,
) -> Option(String) {
  pkg
  |> core.outdated_path(global)
  |> cache.pull_if_not_cached(
    constants.hour,
    force_pull,
    verbose,
    gfunction.freeze3(pull_outdated, pkg, verbose, global),
    pkg.name <> ": latest stable version",
  )

  let cached_file_name = core.outdated_filename(pkg, global)
  let resp =
    cached_file_name
    |> simplifile.read()
    |> cli.hard_fail_with_msg("failed to read " <> cached_file_name)

  {
    use latest_version <- option.map(cli.hard_fail_with_msg(
      json.decode(resp, decode_latest_stable_version),
      "failed to parse " <> cached_file_name,
    ))

    let latest_semver =
      gleamsver.parse(latest_version)
      |> cli.hard_fail_with_msg("failed to parse: " <> cached_file_name)

    case gleamsver.compare(latest_semver, pkg.version) {
      order.Gt -> option.Some(latest_version)
      _ -> option.None
    }
  }
  |> option.flatten
}
