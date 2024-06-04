import filepath
import go_over/util/util.{hard_fail}
import simplifile

pub const hour = 3600

pub const six_hours = 21_600

pub const advisories_repo = "mirego/elixir-security-advisories"

pub const long_ass_dashes = "\n-----------------------------------------------\n"

pub fn go_over_path() -> String {
  let curr =
    simplifile.current_directory()
    |> hard_fail("could not get current path")
  filepath.join(curr, ".go-over")
}
