import filepath
import gleam/list
import gleam/option
import go_over/cache
import go_over/comparisons
import go_over/constants.{go_over_path, six_hours}
import go_over/packages.{type Package}
import go_over/print
import go_over/util.{iffnil}
import go_over/yaml
import shellout
import simplifile

pub type ADV {
  ADV(
    name: String,
    severity: String,
    vulnerable_version_ranges: List(String),
    file: String,
  )
}

fn path() -> String {
  go_over_path()
  |> filepath.join("mirego-elixir-security-advisories")
}

fn read_adv(path: String) -> ADV {
  let #(name, severity, vulnerable_version_ranges) = yaml.parse(path)
  ADV(
    name: name,
    severity: severity,
    vulnerable_version_ranges: vulnerable_version_ranges,
    file: path,
  )
}

fn read_all_adv() -> List(ADV) {
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

fn clone() -> Nil {
  print.progress("Cloning mirego/elixir-security-advisories...")

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

fn delete_and_clone() -> Nil {
  // ? File may or may not exist
  let p = path()

  let _ = simplifile.delete(p)
  clone()
  let assert Ok(Nil) = simplifile.delete(filepath.join(p, ".git"))

  Nil
}

pub fn check_for_advisories(
  packages: List(packages.Package),
  pull: Bool,
) -> List(#(Package, List(ADV))) {
  iffnil(pull, fn() {
    cache.pull_if_not_cached(
      path(),
      six_hours,
      delete_and_clone,
      "mirego/elixir-security-advisories",
    )
  })

  let advs = read_all_adv()

  list.map(packages, fn(pkg) {
    case is_vulnerable(pkg, advs) {
      [] -> option.None
      vulns -> option.Some(#(pkg, vulns))
    }
  })
  |> option.values
}
