import birl
import filepath
import gleam/int
import gleam/order
import gleam/result
import simplifile

fn cache_name(path: String) -> String {
  filepath.join(path, ".go-over-cache")
}

fn file_cached(path: String, max_age_seconds: Int) {
  path
  |> cache_name()
  |> simplifile.read()
  |> result.map_error(fn(_) { Nil })
  |> result.try(int.base_parse(_, 10))
  |> result.map(fn(v) {
    let cutoff = birl.from_unix(v + max_age_seconds)

    case birl.compare(birl.utc_now(), cutoff) {
      order.Lt | order.Eq -> True
      _ -> False
    }
  })
}

pub fn pull_if_not_cached(path: String, max_age: Int, pullfn: fn() -> Nil) {
  case file_cached(path, max_age) {
    Ok(True) -> Nil
    _ -> {
      pullfn()

      let now = birl.utc_now() |> birl.to_unix() |> int.to_string()
      let assert Ok(_) =
        path
        |> cache_name()
        |> simplifile.write(now)

      Nil
    }
  }
}