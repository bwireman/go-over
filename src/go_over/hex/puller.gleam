import gleam/result
import go_over/util/util
import simplifile

pub type Puller {
  CURL
  WGET
  HTTPIE
  Mock(result_filepath: String)
}

pub fn run(puller: Puller, url: String) -> Result(String, #(Int, String)) {
  case puller {
    CURL -> util.retry_cmd("curl", ["-sf", url])

    WGET -> util.retry_cmd("wget", ["-qO-", url])

    HTTPIE -> util.retry_cmd("https", ["--body", url])

    Mock(result_filepath: result_filepath) ->
      simplifile.read(result_filepath)
      |> result.replace_error(#(1, "Mock Failure"))
  }
}
