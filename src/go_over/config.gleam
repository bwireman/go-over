import gleam/dict
import gleam/list
import gleam/result.{unwrap}
import gleam/string
import go_over/advisories/advisories.{type Advisory}
import go_over/packages.{type Package}
import go_over/warning.{type Warning}
import simplifile
import tom.{type Toml}

pub type Format {
  Minimal
  Detailed
}

pub type Config {
  Config(
    cache: Bool,
    format: Format,
    ignore_packages: List(String),
    ignore_severity: List(String),
    ignore_ids: List(String),
  )
}

pub fn read_config(path: String) -> Config {
  let assert Ok(res) = simplifile.read(path)
  let assert Ok(gleam) = tom.parse(res)

  let go_over =
    tom.get_table(gleam, ["go-over"])
    |> unwrap(dict.new())
  let cache =
    tom.get_bool(go_over, ["cache"])
    |> unwrap(True)
  let format =
    tom.get_string(go_over, ["format"])
    |> unwrap("minimal")
    |> string.lowercase()
  let ignore =
    tom.get_table(go_over, ["ignore"])
    |> unwrap(dict.new())
  let packages =
    tom.get_array(ignore, ["packages"])
    |> unwrap([])
  let severity =
    tom.get_array(ignore, ["severity"])
    |> unwrap([])
  let ids =
    tom.get_array(ignore, ["ids"])
    |> unwrap([])

  Config(
    cache: cache,
    format: case format {
      "minimal" -> Minimal
      _ -> Detailed
    },
    ignore_packages: list.map(packages, as_string),
    ignore_severity: list.map(severity, as_string),
    ignore_ids: list.map(ids, as_string),
  )
}

pub fn filter_packages(conf: Config, pkgs: List(Package)) -> List(Package) {
  list.filter(pkgs, fn(pkg) { !list.contains(conf.ignore_packages, pkg.name) })
}

pub fn filter_advisory_ids(conf: Config, advs: List(Advisory)) -> List(Advisory) {
  list.filter(advs, fn(adv) { !list.contains(conf.ignore_ids, adv.id) })
}

pub fn filter_severity(conf: Config, warnings: List(Warning)) -> List(Warning) {
  list.filter(warnings, fn(w) {
    !list.contains(conf.ignore_severity, string.lowercase(w.severity))
  })
}

fn as_string(toml: Toml) -> String {
  let assert tom.String(s) = toml
  s
}
