import filepath
import gleam/list
import gleam/option.{None, Some}
import go_over/advisories/comparisons
import go_over/packages.{type Package}
import go_over/util/cache
import go_over/util/constants.{go_over_path, six_hours}
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

fn path(global: Bool) -> String {
  go_over_path(global)
  |> filepath.join("mirego-elixir-security-advisories")
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
  let packages_path = filepath.join(path(global), "packages")

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
    case adv.name == p.name {
      False -> option.None
      True -> {
        case
          {
            list.any(adv.vulnerable_version_ranges, fn(vuln_semver) {
              let comp = comparisons.get_comparator(vuln_semver)

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
  |> option.values()
}

fn delete_and_clone(verbose: Bool, global: Bool) -> Nil {
  let p = path(global)

  // ? File may or may not exist
  let _ = simplifile.delete(p)
  print.progress(verbose, "Cloning: " <> constants.advisories_repo <> "...")

  path(global)
  |> simplifile.create_directory_all()
  |> cli.hard_fail_with_msg("could not create directory at " <> path(global))

  util.retry_cmd("git", [
    "clone",
    "https://github.com/" <> constants.advisories_repo <> ".git",
    path(global),
  ])
  |> cli.hard_fail_with_msg("could not clone " <> constants.advisories_repo)

  [
    ".git", ".gitignore", ".github", "config", "lib", ".formatter.exs",
    ".credo.exs", "Makefile", "mix.exs", "mix.lock",
  ]
  |> list.map(filepath.join(p, _))
  |> list.each(simplifile.delete)
}

pub fn check_for_advisories(
  packages: List(packages.Package),
  force_pull: Bool,
  verbose: Bool,
  global: Bool,
) -> List(#(Package, List(Advisory))) {
  cache.pull_if_not_cached(
    path(global),
    six_hours,
    force_pull,
    verbose,
    function.freeze2(delete_and_clone, verbose, global),
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
