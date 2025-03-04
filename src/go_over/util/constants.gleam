import directories
import filepath
import gxyz/cli
import simplifile

pub const hour = 3600

pub const six_hours = 21_600

pub const version = "3.0.1"

pub const advisories_repo = "mirego/elixir-security-advisories"

pub const long_ass_dashes = "\n-----------------------------------------------\n"

pub fn go_over_path(global: Bool) -> String {
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
}
