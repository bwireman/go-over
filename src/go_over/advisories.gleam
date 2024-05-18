import filepath
import gleam/io
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

pub type ADV {
  ADV(
    name: String,
    first_patched_versions: List(String),
    vulnerable_version_ranges: List(String),
  )
}

pub fn path() -> String {
  let assert Ok(curr) = simplifile.current_directory()
  filepath.join(curr, ".go-over")
}

pub fn clone() {
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
  )
}

pub fn read_all_adv() {
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

fn is_vulnerable(p: Package, advs: List(ADV)) -> Bool {
  list.any(advs, fn(adv) {
    adv.name == p.name
    && {
      list.any(adv.vulnerable_version_ranges, fn(vulnsemver) {
        let comp = comparisons.get_comparator(vulnsemver)
        let parsedvulnsemver = comparisons.parse(vulnsemver)
        io.debug(parsedvulnsemver)
        io.debug(p.version)

        comp(p.version, parsedvulnsemver)
      })
    }
  })
}

pub fn go(path: String) {
  let packages = read_manifest(path)
  let advs = read_all_adv()

  list.any(packages, fn(p) { is_vulnerable(p, advs) })
}
