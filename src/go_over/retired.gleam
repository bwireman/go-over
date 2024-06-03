import filepath
import gleam/hackney
import gleam/hexpm.{type ReleaseRetirement}
import gleam/http/request
import gleam/json
import gleam/option.{type Option, Some}
import go_over/packages
import go_over/util/cache
import go_over/util/constants
import go_over/util/print
import go_over/util/util.{hard_fail, iffnil}
import simplifile

fn path(pkg: packages.Package) -> String {
  constants.go_over_path()
  |> filepath.join("deps")
  |> filepath.join(pkg.name)
  |> filepath.join(pkg.version_raw)
}

fn filename(pkg) -> String {
  pkg
  |> path
  |> filepath.join("resp.json")
}

fn pull_retired(pkg: packages.Package) -> Nil {
  print.progress("Checking: " <> pkg.name <> " From hex.pm")

  // Prepare a HTTP request record
  let assert Some(request) =
    request.to(
      "https://hex.pm/api/packages/"
      <> pkg.name
      <> "/releases/"
      <> pkg.version_raw,
    )
    |> hard_fail("Request to hex.pm for package: " <> pkg.name <> " failed")

  // Send the HTTP request to the server
  let assert Some(resp) =
    request
    |> request.prepend_header("accept", "application/json")
    |> hackney.send
    |> hard_fail("Request to hex.pm for package: " <> pkg.name <> " failed")

  let pkg_path = path(pkg)
  let pkg_path_fail =
    "could not store hex.pm pkg info. package: "
    <> pkg.name
    <> " at path "
    <> pkg_path

  let assert Some(_) =
    simplifile.create_directory_all(pkg_path)
    |> hard_fail(pkg_path_fail)
  let assert Some(_) =
    pkg
    |> filename
    |> simplifile.write(resp.body)
    |> hard_fail(pkg_path_fail)
  Nil
}

pub fn check_retired(
  pkg: packages.Package,
  pull: Bool,
) -> Option(ReleaseRetirement) {
  iffnil(pull, fn() {
    pkg
    |> path()
    |> cache.pull_if_not_cached(
      constants.hour,
      fn() {
        let _ = simplifile.delete(path(pkg))
        pull_retired(pkg)
      },
      pkg.name <> ":" <> pkg.version_raw,
    )
  })

  let cached_file_name = filename(pkg)
  let assert Some(resp) =
    cached_file_name
    |> simplifile.read()
    |> hard_fail("failed to read " <> cached_file_name)

  let assert Some(release) =
    json.decode(resp, hexpm.decode_release)
    |> hard_fail("failed to parse " <> cached_file_name)

  release.retirement
}

pub fn print_ret(ret: hexpm.ReleaseRetirement) -> String {
  let reason = hexpm.retirement_reason_to_string(ret.reason)
  case ret.message {
    option.Some(msg) -> reason <> ": " <> msg
    _ -> reason
  }
}
