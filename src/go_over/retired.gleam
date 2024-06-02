import filepath
import gleam/hackney
import gleam/hexpm.{type ReleaseRetirement}
import gleam/http/request
import gleam/json
import gleam/option.{type Option}
import go_over/packages
import go_over/util/cache
import go_over/util/constants
import go_over/util/print
import go_over/util/util.{iffnil}
import simplifile

fn path(pkg: packages.Package) -> String {
  constants.go_over_path()
  |> filepath.join("deps")
  |> filepath.join(pkg.name)
  |> filepath.join(pkg.version_raw)
}

fn filname(pkg) -> String {
  pkg
  |> path
  |> filepath.join("resp.json")
}

fn pull_retired(pkg: packages.Package) -> Nil {
  print.progress("Checking: " <> pkg.name <> " From hex.pm")

  // Prepare a HTTP request record
  let assert Ok(request) =
    request.to(
      "https://hex.pm/api/packages/"
      <> pkg.name
      <> "/releases/"
      <> pkg.version_raw,
    )

  // Send the HTTP request to the server
  let assert Ok(resp) =
    request
    |> request.prepend_header("accept", "application/json")
    |> hackney.send

  let assert Ok(_) = simplifile.create_directory_all(path(pkg))

  let assert Ok(_) =
    pkg
    |> filname
    |> simplifile.write(resp.body)
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

  let assert Ok(resp) =
    pkg
    |> filname
    |> simplifile.read()

  let assert Ok(release) = json.decode(resp, hexpm.decode_release)
  release.retirement
}

pub fn print_ret(ret: hexpm.ReleaseRetirement) -> String {
  let reason = hexpm.retirement_reason_to_string(ret.reason)
  case ret.message {
    option.Some(msg) -> reason <> ": " <> msg
    _ -> reason
  }
}
