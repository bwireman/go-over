import gleam/dict
import gleam/list
import gleam/option
import gleamsver.{type SemVer}
import simplifile
import tom

pub type Package {
  Package(name: String, version: SemVer, version_raw: String, direct: Bool)
}

pub fn read_manifest(path: String) -> List(Package) {
  let assert Ok(res) = simplifile.read(path)
  let assert Ok(manifest) = tom.parse(res)
  let assert Ok(packages) = tom.get_array(manifest, ["packages"])
  let assert Ok(requirements) = tom.get_table(manifest, ["requirements"])
  let required_packages = dict.keys(requirements)

  list.map(packages, fn(p) {
    case p {
      tom.InlineTable(x) -> {
        let assert Ok(name) = tom.get_string(x, ["name"])
        let assert Ok(ver) = tom.get_string(x, ["version"])
        let assert Ok(semver) = gleamsver.parse(ver)

        option.Some(Package(
          name,
          semver,
          ver,
          list.contains(required_packages, name),
        ))
      }

      _ -> option.None
    }
  })
  |> option.values
}
