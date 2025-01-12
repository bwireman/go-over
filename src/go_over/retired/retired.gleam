import gleam/hexpm.{type ReleaseRetirement}
import gleam/json
import gleam/option.{type Option}
import go_over/packages.{type Package}
import go_over/retired/core
import go_over/util/cache
import go_over/util/constants
import go_over/util/print
import go_over/util/util.{hard_fail}
import gxyz/gxyz_function
import simplifile

fn pull_retired(pkg: Package, verbose: Bool, global: Bool) -> Nil {
  print.progress(verbose, "Checking: " <> pkg.name <> " From hex.pm")
  let pkg_path = core.release_path(pkg, global)
  let pkg_path_fail = core.pkg_pull_error(pkg, pkg_path)

  let _ = simplifile.delete(pkg_path)
  simplifile.create_directory_all(pkg_path)
  |> hard_fail(pkg_path_fail)

  let resp = core.do_pull_hex(pkg, core.release_url(pkg))

  pkg
  |> core.release_filename(global)
  |> simplifile.write(resp)
  |> hard_fail(pkg_path_fail)
}

pub fn check_retired(
  pkg: Package,
  force_pull: Bool,
  verbose: Bool,
  global: Bool,
) -> Option(ReleaseRetirement) {
  pkg
  |> core.release_path(global)
  |> cache.pull_if_not_cached(
    constants.hour,
    force_pull,
    verbose,
    gxyz_function.freeze3(pull_retired, pkg, verbose, global),
    pkg.name <> ":" <> pkg.version_raw,
  )

  let cached_file_name = core.release_filename(pkg, global)
  let resp =
    cached_file_name
    |> simplifile.read()
    |> hard_fail("failed to read " <> cached_file_name)

  let release =
    json.parse(resp, hexpm.release_decoder())
    |> hard_fail("failed to parse " <> cached_file_name)

  release.retirement
}
