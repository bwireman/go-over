import filepath
import gleam/list
import gleam/option
import gleam/string
import go_over/comparisons
import go_over/packages
import go_over/yaml
import shellout
import simplifile

pub type ADV {
  ADV(name: String, vulnerable_version_ranges: List(String), file: String)
}

fn path() -> String {
  let assert Ok(curr) = simplifile.current_directory()
  filepath.join(curr, ".go-over")
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

pub fn check_for_advisories(packages: List(packages.Package), pull: Bool) {
  case pull {
    True -> {
      let assert Ok(Nil) = simplifile.delete(path())
      clone()
    }

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

pub fn print_adv(adv: ADV) {
  let assert Ok(contents) = simplifile.read(adv.file)

  string.append(adv.name, string.append("\n\n", contents))
}
