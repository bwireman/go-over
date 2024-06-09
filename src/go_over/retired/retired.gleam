@target(erlang)
import gleam/hackney
import gleam/hexpm.{type ReleaseRetirement}
@target(erlang)
import gleam/http/request
import gleam/json
import gleam/option.{type Option}
import go_over/packages.{type Package}
import go_over/retired/core
import go_over/util/cache
import go_over/util/constants
import go_over/util/print
import go_over/util/util.{hard_fail}
import simplifile

@target(javascript)
pub fn do_pull_retired(pkg: Package) -> String {
  pkg
  |> core.hex_url()
  |> core.do_fetch()
  |> hard_fail("request to hex.pm for package: " <> pkg.name <> " failed")
}

@target(erlang)
fn do_pull_retired(pkg: Package) -> String {
  let resp =
    pkg
    |> core.hex_url()
    |> request.to()
    |> hard_fail("request to hex.pm for package: " <> pkg.name <> " failed")
    |> request.prepend_header("accept", "application/json")
    |> hackney.send
    |> hard_fail("request to hex.pm for package: " <> pkg.name <> " failed")

  resp.body
}

fn pull_retired(pkg: Package) -> Nil {
  print.progress("Checking: " <> pkg.name <> " From hex.pm")
  let _ = simplifile.delete(core.path(pkg))

  let pkg_path = core.path(pkg)
  let pkg_path_fail =
    "could not store hex.pm pkg info. package: "
    <> pkg.name
    <> " at path "
    <> pkg_path

  simplifile.create_directory_all(pkg_path)
  |> hard_fail(pkg_path_fail)

  let resp = do_pull_retired(pkg)

  pkg
  |> core.filename
  |> simplifile.write(resp)
  |> hard_fail(pkg_path_fail)
}

pub fn check_retired(
  pkg: Package,
  force_pull: Bool,
) -> Option(ReleaseRetirement) {
  pkg
  |> core.path()
  |> cache.pull_if_not_cached(
    constants.hour,
    force_pull,
    fn() { pull_retired(pkg) },
    pkg.name <> ":" <> pkg.version_raw,
  )

  let cached_file_name = core.filename(pkg)
  let resp =
    cached_file_name
    |> simplifile.read()
    |> hard_fail("failed to read " <> cached_file_name)

  let release =
    json.decode(resp, hexpm.decode_release)
    |> hard_fail("failed to parse " <> cached_file_name)

  release.retirement
}
