import filepath
import gleam/list
import gleam/option
import gleam/string
import go_over/comparisons
import go_over/yaml
import shellout
import simplifile
import stoiridh/version.{type Version}
import tom

pub type ADV {
  ADV(name: String, vulnerable_version_ranges: List(String), file: String)
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
  let #(name, vulnerable_version_ranges) = yaml.parse(path)
  ADV(name, vulnerable_version_ranges, path)
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

fn is_vulnerable(p: Package, advs: List(ADV)) -> List(ADV) {
  list.map(advs, fn(adv) {
    case adv.name == p.name {
      False -> option.None
      True -> {
        case
          {
            list.any(adv.vulnerable_version_ranges, fn(vulnsemver) {
              let comp = comparisons.get_comparator(vulnsemver)

              comp(p.version)
            })
          }
        {
          False -> option.None
          True -> option.Some(adv)
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

pub fn print_adv(adv: ADV) {
  let assert Ok(contents) = simplifile.read(adv.file)

  string.append(adv.name, string.append("\n\n", contents))
}
