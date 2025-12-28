import filepath
import gleam/list
import gleam/option.{None, Some}
import global_value
import go_over/advisories/comparisons
import go_over/packages.{type Package}
import go_over/util/cache
import go_over/util/constants.{six_hours}
import go_over/util/globals
import go_over/util/print
import go_over/util/util
import gxyz/cli
import gxyz/function
import simplifile

pub type Advisory {
  Advisory(
    id: String,
    name: String,
    severity: String,
    vulnerable_version_ranges: List(String),
    description: String,
  )
}

fn advisories_path(global: Bool) -> String {
  global_value.create_with_unique_name("advisories.path.global.data", fn() {
    globals.go_over_path(global)
    |> filepath.join("mirego-elixir-security-advisories")
  })
}

@external(erlang, "go_over_ffi", "parse_adv")
@external(javascript, "./../../go_over_ffi.mjs", "parse_adv")
pub fn read(
  body: String,
) -> Result(#(String, String, String, String, List(String)), Nil)

fn read_adv(path: String) -> Advisory {
  let body =
    simplifile.read(path)
    |> cli.hard_fail_with_msg("could not read adv file at: " <> path)

  let #(id, name, severity, desc, versions) =
    read(body)
    |> cli.hard_fail_with_msg("could not parse advisory file: " <> path)

  Advisory(
    id: id,
    name: name,
    severity: severity,
    vulnerable_version_ranges: versions,
    description: desc,
  )
}

fn read_all_adv(global: Bool) -> List(Advisory) {
  let packages_path = filepath.join(advisories_path(global), "packages")

  let packages =
    cli.hard_fail_with_msg(
      simplifile.read_directory(packages_path),
      "could not read " <> packages_path,
    )

  list.flat_map(packages, fn(dir) {
    let dir_path = filepath.join(packages_path, dir)

    let adv_names =
      cli.hard_fail_with_msg(
        simplifile.read_directory(dir_path),
        "could not read " <> dir_path,
      )
    list.map(adv_names, fn(adv_name) {
      read_adv(filepath.join(dir_path, adv_name))
    })
  })
}

fn is_vulnerable(
  p: packages.Package,
  advisories: List(Advisory),
) -> List(Advisory) {
  list.map(advisories, fn(adv) {
    let do_check =
      adv.name == p.name
      && {
        list.any(adv.vulnerable_version_ranges, fn(vuln_semver) {
          let comp = comparisons.get_comparator(vuln_semver)

          comp(p.version)
        })
      }

    case do_check {
      False -> option.None
      True -> option.Some(adv)
    }
  })
  |> option.values()
}

fn delete_and_clone(global: Bool) -> Nil {
  let path = advisories_path(global)

  // ? File may or may not exist
  let _ = simplifile.delete(path)
  print.progress("Cloning: " <> constants.advisories_repo <> "...")

  path
  |> simplifile.create_directory_all()
  |> cli.hard_fail_with_msg("could not create directory at " <> path)

  util.retry_cmd("git", [
    "clone",
    "https://github.com/" <> constants.advisories_repo <> ".git",
    path,
  ])
  |> cli.hard_fail_with_msg("could not clone " <> constants.advisories_repo)

  [
    ".git", ".gitignore", ".github", "config", "lib", ".formatter.exs",
    ".credo.exs", "Makefile", "mix.exs", "mix.lock",
  ]
  |> list.map(filepath.join(path, _))
  |> list.each(simplifile.delete)
}

pub fn check_for_advisories(
  packages: List(packages.Package),
  force_pull: Bool,
  global: Bool,
) -> List(#(Package, List(Advisory))) {
  cache.pull_if_not_cached(
    advisories_path(global),
    six_hours,
    force_pull,
    function.freeze1(delete_and_clone, global),
    constants.advisories_repo,
  )

  let advisories = read_all_adv(global)

  list.map(packages, fn(pkg) {
    case is_vulnerable(pkg, advisories) {
      [] -> None
      vulns -> Some(#(pkg, vulns))
    }
  })
  |> option.values()
}
