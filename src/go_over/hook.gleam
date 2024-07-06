import filepath
import gleam/io
import gleam/set
import go_over/util/print.{format_high}
import go_over/util/util.{hard_fail}
import shellout
import simplifile

@target(erlang)
const tmpl = "gleam run -m go_over --target erlang"

@target(javascript)
const tmpl = "gleam run -m go_over --target javascript"

fn get_path() -> String {
  simplifile.current_directory()
  |> hard_fail("could not get current path")
  |> filepath.join(".git")
  |> filepath.join("hooks")
}

pub fn main() {
  format_high(
    "This command is DEPRECATED consider using 🌵 cactus instead\nhttps://hex.pm/packages/cactus",
  )
  |> io.println_error()
  let pwd = get_path()

  simplifile.create_directory_all(pwd)
  |> hard_fail("could not create hooks directory")

  let pre_commit_path = filepath.join(pwd, "pre-commit")

  util.throwaway(
    util.has_flag(shellout.arguments(), "recreate"),
    util.freeze1(simplifile.delete, pre_commit_path),
  )

  let _ = simplifile.create_file(pre_commit_path)
  let all =
    set.from_list([simplifile.Read, simplifile.Write, simplifile.Execute])

  let _ =
    simplifile.set_permissions(
      pre_commit_path,
      simplifile.FilePermissions(user: all, group: all, other: all),
    )

  let text = case util.has_flag(shellout.arguments(), "outdated") {
    False -> tmpl
    True -> tmpl <> " -- --outdated"
  }

  simplifile.append(pre_commit_path, "\n" <> text)
}
