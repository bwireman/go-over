import gleam/dict
import gleam/list
import gleam/option.{Some}
import gleam/string
import gleamsver.{type SemVer}
import go_over/util/print.{warning}
import go_over/util/util.{hard_fail}
import shellout
import simplifile
import tom

pub type Package {
  Package(name: String, version: SemVer, version_raw: String, direct: Bool)
}

pub fn read_manifest(path: String) -> List(Package) {
  let res = simplifile.read(path) |> hard_fail("could not parse " <> path)
  let manifest = tom.parse(res) |> hard_fail("could not parse " <> path)
  let packages =
    tom.get_array(manifest, ["packages"])
    |> hard_fail("could not parse " <> path <> " value: packages")
  let requirements =
    tom.get_table(manifest, ["requirements"])
    |> hard_fail("could not parse " <> path <> " value: requirements")
  let required_packages = dict.keys(requirements)

  list.map(packages, fn(p) {
    case p {
      tom.InlineTable(t) -> {
        let name =
          tom.get_string(t, ["name"])
          |> hard_fail("could not parse parckage: " <> string.inspect(t))
        let ver =
          tom.get_string(t, ["version"])
          |> hard_fail("could not parse parckage: " <> string.inspect(t))
        let semver =
          gleamsver.parse(ver)
          |> hard_fail("could not parse parckage version: " <> ver)

        Some(Package(name, semver, ver, list.contains(required_packages, name)))
      }

      _ -> {
        warning("could not parse packages: incorrect type")
        shellout.exit(1)
        option.None
      }
    }
  })
  |> option.values
}
