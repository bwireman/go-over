import filepath
import gleam/list
import gleam/option
import go_over/cache
import go_over/comparisons
import go_over/constants.{go_over_path, six_hours}
import go_over/packages
import go_over/yaml
import shellout
import simplifile

pub type ADV {
  ADV(name: String, vulnerable_version_ranges: List(String), file: String)
}

fn path() {
  go_over_path()
  |> filepath.join("mirego-elixir-security-advisories")
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

fn is_vulnerable(p: packages.Package, advs: List(ADV)) -> List(ADV) {
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
  let assert Ok(Nil) =
    path()
    |> simplifile.create_directory_all()

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

fn delete_and_clone() {
  // ? File may or may not exist
  let p = path()

  let _ = simplifile.delete(p)
  clone()
  let assert Ok(Nil) = simplifile.delete(filepath.join(p, ".git"))

  Nil
}

pub fn check_for_advisories(packages: List(packages.Package), pull: Bool) {
  case pull {
    True -> cache.pull_if_not_cached(path(), six_hours, delete_and_clone)
    False -> Nil
  }

  let advs = read_all_adv()

  list.map(packages, fn(pkg) {
    case is_vulnerable(pkg, advs) {
      [] -> option.None
      vulns -> option.Some(#(pkg, vulns))
    }
  })
  |> option.values
}
