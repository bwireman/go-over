import filepath
import simplifile

pub const hour = 3600

pub const six_hours = 21_600

pub const long_ass_dashes = "\n-----------------------------------------------\n"

pub fn go_over_path() -> String {
  let assert Ok(curr) = simplifile.current_directory()
  filepath.join(curr, ".go-over")
}
