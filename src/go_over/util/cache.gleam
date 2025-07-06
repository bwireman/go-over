import filepath
import gleam/order
import gleam/result
import gleam/time/calendar
import gleam/time/duration
import gleam/time/timestamp
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
  |> result.try(timestamp.parse_rfc3339)
  |> result.map(fn(v) {
    let cutoff = timestamp.add(v, duration.seconds(max_age_seconds))

    case timestamp.compare(timestamp.system_time(), cutoff) {
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
        timestamp.system_time()
        |> timestamp.to_rfc3339(calendar.utc_offset)

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
