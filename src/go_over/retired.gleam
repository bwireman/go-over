import gleam/hackney
import gleam/hexpm
import gleam/http/request
import gleam/json
import gleam/option
import go_over/packages

pub fn check_retired(pkg: packages.Package) {
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

  let assert Ok(release) = json.decode(resp.body, hexpm.decode_release)

  release.retirement
}

pub fn print_ret(ret: hexpm.ReleaseRetirement) -> String {
  let reason = hexpm.retirement_reason_to_string(ret.reason)
  case ret.message {
    option.Some(msg) -> reason <> ": " <> msg
    _ -> reason
  }
}
