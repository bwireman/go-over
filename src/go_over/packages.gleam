import gleam/dict
import gleam/list
import gleam/option.{Some}
import gleam/string
import gleamsver.{type SemVer}
import go_over/util/print.{warning}
import go_over/util/util
import gxyz/cli
import shellout
import simplifile
import tom

pub type PackageSource {
  PackageSourceHex
  PackageSourceGit
  PackageSourceLocal
}

pub type Package {
  Package(
    name: String,
    version: SemVer,
    version_raw: String,
    direct: Bool,
    source: PackageSource,
  )
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
  let required_packages =
    tom.get_table(manifest, ["requirements"])
    |> cli.hard_fail_with_msg(
      "could not parse " <> path <> " value: requirements",
    )
    |> dict.keys()

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

        let source_raw =
          tom.get_string(t, ["source"])
          |> cli.hard_fail_with_msg(
            "could not parse package: " <> string.inspect(t),
          )

        let source = case source_raw {
          "git" -> PackageSourceGit
          "hex" -> PackageSourceHex
          "local" -> PackageSourceLocal
          _ -> util.do_panic()
        }

        Some(Package(
          name,
          semver,
          ver,
          list.contains(required_packages, name),
          source,
        ))
      }

      _ -> {
        warning("could not parse packages: incorrect type")
        shellout.exit(1)
        util.do_panic()
      }
    }
  })
  |> option.values()
}
