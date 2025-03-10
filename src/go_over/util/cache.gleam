import birl
import filepath
import gleam/int
import gleam/order
import gleam/result
import go_over/util/constants
import go_over/util/print
import gxyz/cli
import simplifile

fn cache_name(path: String) -> String {
  filepath.join(path, ".go-over-cache")
}

fn version_name(path: String) -> String {
  filepath.join(path, ".go-over-version")
}

fn file_cached(path: String, max_age_seconds: Int) -> Result(Bool, Nil) {
  path
  |> cache_name()
  |> simplifile.read()
  |> result.replace_error(Nil)
  |> result.try(int.base_parse(_, 10))
  |> result.map(fn(v) {
    let cutoff = birl.from_unix(v + max_age_seconds)

    case birl.compare(birl.utc_now(), cutoff) {
      order.Lt | order.Eq -> True
      _ -> False
    }
  })
}

pub fn pull_if_not_cached(
  path: String,
  max_age: Int,
  force_pull: Bool,
  verbose: Bool,
  pull_fn: fn() -> Nil,
  cache_message: String,
) -> Nil {
  case force_pull, file_cached(path, max_age) {
    False, Ok(True) -> {
      print.progress(verbose, "Cached: " <> cache_message)

      Nil
    }

    _, _ -> {
      pull_fn()

      let now =
        birl.utc_now()
        |> birl.to_unix()
        |> int.to_string()

      simplifile.create_directory_all(path)
      |> cli.hard_fail_with_msg("could not write cache file for " <> path)

      path
      |> cache_name()
      |> simplifile.write(now)
      |> cli.hard_fail_with_msg("could not write cache file for " <> path)

      path
      |> version_name()
      |> simplifile.write(constants.version)
      |> cli.hard_fail_with_msg("could not write cache file for " <> path)

      Nil
    }
  }
}
