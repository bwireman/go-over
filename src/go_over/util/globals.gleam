import directories
import filepath
import global_value
import gxyz/cli
import gxyz/function
import simplifile

pub fn go_over_path(global: Bool) -> String {
  global_value.create_with_unique_name("go_over_path.global.data", fn() {
    let #(path, name) = case global {
      True -> #(
        directories.cache_dir()
          |> cli.hard_fail_with_msg("could not get cache directory"),
        "go-over",
      )
      False -> #(
        simplifile.current_directory()
          |> cli.hard_fail_with_msg("could not get current directory"),
        ".go-over",
      )
    }

    filepath.join(path, name)
  })
}

const verbose_key = "verbose.global.data"

pub fn set_verbose(verbose: Bool) -> Bool {
  global_value.create_with_unique_name(verbose_key, function.freeze(verbose))
}

pub fn verbose() -> Bool {
  global_value.create_with_unique_name(verbose_key, function.freeze(False))
}
