@target(erlang)
import gleam/http/request
@target(erlang)
import gleam/httpc
import gleam/result
@target(erlang)
import gleam/string
import go_over/util/util
import simplifile

@target(erlang)
pub const default = Native

@target(erlang)
pub const default_string = "native"

@target(javascript)
pub const default = CURL

@target(javascript)
pub const default_string = "curl"

pub type Puller {
  Native
  CURL
  WGET
  HTTPIE
  Mock(result_filepath: String)
}

@target(erlang)
fn native_get(url: String) -> Result(String, #(Int, String)) {
  url
  |> request.to()
  |> result.replace_error(httpc.InvalidUtf8Response)
  |> result.try(httpc.send)
  |> result.map(fn(resp) { resp.body })
  |> result.map_error(fn(err) { #(1, string.inspect(err)) })
}

@target(javascript)
fn native_get(_: String) -> Result(String, #(Int, String)) {
  Error(#(1, "Native puller is only supported on the Erlang target"))
}

pub fn run(puller: Puller, url: String) -> Result(String, #(Int, String)) {
  case puller {
    Native -> native_get(url)

    CURL -> util.retry_cmd("curl", ["-sf", url])

    WGET -> util.retry_cmd("wget", ["-qO-", url])

    HTTPIE -> util.retry_cmd("https", ["--body", url])

    Mock(result_filepath: result_filepath) ->
      simplifile.read(result_filepath)
      |> result.replace_error(#(1, "Mock Failure"))
  }
}
