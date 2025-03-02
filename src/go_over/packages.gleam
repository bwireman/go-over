import gleam/dict
import gleam/list
import gleam/option.{Some}
import gleam/string
import gleamsver.{type SemVer}
import go_over/util/print.{warning}
import gxyz/cli
import shellout
import simplifile
import tom

pub type Package {
  Package(name: String, version: SemVer, version_raw: String, direct: Bool)
}

pub fn read_manifest(path: String) -> List(Package) {
  let manifest =
    simplifile.read(path)
    |> cli.hard_fail_with_msg("could not parse " <> path)
    |> string.replace("\r\n", "\n")
    |> tom.parse()
    |> cli.hard_fail_with_msg("could not parse " <> path)

  let packages =
    tom.get_array(manifest, ["packages"])
    |> cli.hard_fail_with_msg("could not parse " <> path <> " value: packages")
  let requirements =
    tom.get_table(manifest, ["requirements"])
    |> cli.hard_fail_with_msg(
      "could not parse " <> path <> " value: requirements",
    )
  let required_packages = dict.keys(requirements)

  list.map(packages, fn(p) {
    case p {
      tom.InlineTable(t) -> {
        let name =
          tom.get_string(t, ["name"])
          |> cli.hard_fail_with_msg(
            "could not parse package: " <> string.inspect(t),
          )
        let ver =
          tom.get_string(t, ["version"])
          |> cli.hard_fail_with_msg(
            "could not parse package: " <> string.inspect(t),
          )
        let semver =
          gleamsver.parse(ver)
          |> cli.hard_fail_with_msg("could not parse package version: " <> ver)

        Some(Package(name, semver, ver, list.contains(required_packages, name)))
      }

      _ -> {
        warning("could not parse packages: incorrect type")
        shellout.exit(1)
        panic as "Unreachable, please create an issue in https://github.com/bwireman/go-over if you see this"
      }
    }
  })
  |> option.values()
}
