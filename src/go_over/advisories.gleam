import filepath
import gleam/list
import gleam/option
import go_over/comparisons
import shellout
import simplifile
import stoiridh/version.{type Version}
import tom

type YamlADV

@external(javascript, "./../yaml.mjs", "readADV")
fn parse(path: String) -> YamlADV

@external(javascript, "./../yaml.mjs", "name")
fn name(y: YamlADV) -> String

@external(javascript, "./../yaml.mjs", "first_patched_versions")
fn first_patched_versions(y: YamlADV) -> List(String)

@external(javascript, "./../yaml.mjs", "vulnerable_version_ranges")
fn vulnerable_version_ranges(y: YamlADV) -> List(String)

type ADV {
  ADV(
    name: String,
    first_patched_versions: List(String),
    vulnerable_version_ranges: List(String),
    file: String,
  )
}

fn path() -> String {
  let assert Ok(curr) = simplifile.current_directory()
  filepath.join(curr, ".go-over")
}

type Package {
  Package(name: String, version: Version)
}

fn read_manifest(path: String) {
  let assert Ok(res) = simplifile.read(path)
  let assert Ok(manifest) = tom.parse(res)
  let assert Ok(packages) = tom.get_array(manifest, ["packages"])
  list.map(packages, fn(p) {
    case p {
      tom.InlineTable(x) -> {
        let assert Ok(name) = tom.get_string(x, ["name"])
        let assert Ok(ver) = tom.get_string(x, ["version"])
        let assert Ok(semver) = version.parse(ver)

        option.Some(Package(name, semver))
      }

      _ -> option.None
    }
  })
  |> option.values
}

fn read_adv(path: String) {
  let assert Ok(contents) = simplifile.read(path)
  let parsed = parse(contents)
  ADV(
    name(parsed),
    first_patched_versions(parsed),
    vulnerable_version_ranges(parsed),
    path,
  )
}

fn read_all_adv() {
  let packages_path = filepath.join(path(), "packages")

  let assert Ok(packages) = simplifile.read_directory(packages_path)
  list.flat_map(packages, fn(dir) {
    let dir_path = filepath.join(packages_path, dir)

    let assert Ok(adv_names) = simplifile.read_directory(dir_path)
    list.map(adv_names, fn(adv_name) {
      read_adv(filepath.join(dir_path, adv_name))
    })
  })
}

fn is_vulnerable(p: Package, advs: List(ADV)) -> List(String) {
  list.map(advs, fn(adv) {
    case adv.name == p.name {
      False -> option.None
      True -> {
        case
          {
            list.any(adv.vulnerable_version_ranges, fn(vulnsemver) {
              let comp = comparisons.get_comparator(vulnsemver)
              let parsedvulnsemver = comparisons.parse(vulnsemver)

              comp(p.version, parsedvulnsemver)
            })
          }
        {
          False -> option.None
          True -> option.Some(adv.file)
        }
      }
    }
  })
  |> option.values
}

fn clone() {
  let assert Ok(Nil) = simplifile.create_directory_all(path())

  let assert Ok(_) =
    shellout.command(
      run: "git",
      with: [
        "clone",
        "https://github.com/mirego/elixir-security-advisories.git",
        path(),
      ],
      in: ".",
      opt: [],
    )

  Nil
}

pub fn check_for_advisories(manifest_path: String, pull: Bool) {
  case pull {
    True -> {
      let assert _ = simplifile.delete(path())
      clone()
    }

    False -> Nil
  }

  let packages = read_manifest(manifest_path)
  let advs = read_all_adv()

  list.flat_map(packages, fn(p) {
    case is_vulnerable(p, advs) {
      [] -> []
      vulns -> vulns
    }
  })
}

pub fn print_adv(adv_path: String) {
  let assert Ok(contents) = simplifile.read(adv_path)

  shellout.style(contents, with: shellout.display(["bold", "italic", "tubular"]), custom: [])
}
