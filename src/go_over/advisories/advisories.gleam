import filepath
import gleam/list
import gleam/option
import gleam/result.{unwrap}
import gleam/string
import go_over/advisories/comparisons
import go_over/packages.{type Package}
import go_over/util/cache
import go_over/util/constants.{go_over_path, six_hours}
import go_over/util/print
import go_over/util/util.{iffnil}
import shellout
import simplifile

pub type Advisory {
  Advisory(
    id: String,
    name: String,
    severity: String,
    vulnerable_version_ranges: List(String),
    desciption: String,
  )
}

fn path() -> String {
  go_over_path()
  |> filepath.join("mirego-elixir-security-advisories")
}

@external(erlang, "advisory_yaml", "parse")
fn read(
  path: String,
) -> #(
  List(UtfCodepoint),
  List(UtfCodepoint),
  List(UtfCodepoint),
  List(UtfCodepoint),
  List(List(UtfCodepoint)),
)

fn read_adv(path: String) -> Advisory {
  let #(id, name, severity, desc, versions) = read(path)

  Advisory(
    id: string.from_utf_codepoints(id),
    name: string.from_utf_codepoints(name),
    severity: string.from_utf_codepoints(severity),
    vulnerable_version_ranges: list.map(versions, string.from_utf_codepoints),
    desciption: string.from_utf_codepoints(desc),
  )
}

fn read_all_adv() -> List(Advisory) {
  let packages_path = filepath.join(path(), "packages")

  let packages =
    simplifile.read_directory(packages_path)
    |> unwrap([])
  list.flat_map(packages, fn(dir) {
    let dir_path = filepath.join(packages_path, dir)

    let adv_names =
      simplifile.read_directory(dir_path)
      |> unwrap([])
    list.map(adv_names, fn(adv_name) {
      read_adv(filepath.join(dir_path, adv_name))
    })
  })
}

fn is_vulnerable(p: packages.Package, advs: List(Advisory)) -> List(Advisory) {
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

fn delete_and_clone() -> Nil {
  // ? File may or may not exist
  let p = path()

  let _ = simplifile.delete(p)
  print.progress("Cloning: " <> constants.advisories_repo <> "...")

  let assert Ok(Nil) =
    path()
    |> simplifile.create_directory_all()

  let assert Ok(_) =
    shellout.command(
      run: "git",
      with: [
        "clone",
        "https://github.com/" <> constants.advisories_repo <> ".git",
        path(),
      ],
      in: ".",
      opt: [],
    )

  [
    ".git", ".gitignore", ".github", "config", "lib", ".formatter.exs",
    ".credo.exs", "Makefile", "mix.exs", "mix.lock",
  ]
  |> list.map(filepath.join(p, _))
  |> list.each(simplifile.delete)
}

pub fn check_for_advisories(
  packages: List(packages.Package),
  pull: Bool,
) -> List(#(Package, List(Advisory))) {
  iffnil(pull, fn() {
    cache.pull_if_not_cached(
      path(),
      six_hours,
      delete_and_clone,
      constants.advisories_repo,
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
