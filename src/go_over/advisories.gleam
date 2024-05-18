import filepath
import gleam/list
import gleam/option
import gleam/result
import shellout
import simplifile
import tom

pub fn clone() {
  simplifile.current_directory()
  |> result.map(fn(curr) {
    let path = filepath.join(curr, ".go-over")
    let assert Ok(Nil) = simplifile.create_directory_all(path)

    let assert Ok(_) =
      shellout.command(
        run: "git",
        with: [
          "clone",
          "https://github.com/mirego/elixir-security-advisories.git",
          path,
        ],
        in: ".",
        opt: [],
      )

    Nil
  })
}

pub fn read_manifest(path: String) {
  let assert Ok(res) = simplifile.read(path)
  let assert Ok(manifest) = tom.parse(res)
  let assert Ok(packages) = tom.get_array(manifest, ["packages"])
  list.map(packages, fn(p) {
    case p {
      tom.InlineTable(x) -> {
        let assert Ok(name) = tom.get_string(x, ["name"])
        let assert Ok(version) = tom.get_string(x, ["version"])

        option.Some(#(name, version))
      }

      _ -> option.None
    }
  })
  |> option.values
}
