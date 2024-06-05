import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result.{unwrap}
import gleam/string
import go_over/advisories/advisories.{type Advisory}
import go_over/packages.{type Package}
import go_over/util/print
import go_over/util/util.{hard_fail}
import go_over/warning.{type Warning}
import shellout
import simplifile
import tom.{type Toml}

pub type Format {
  Minimal
  Detailed
  JSON
}

pub type Config {
  Config(
    cache: Bool,
    force: Bool,
    fake: Bool,
    format: Format,
    ignore_packages: List(String),
    ignore_severity: List(String),
    ignore_ids: List(String),
  )
}

pub fn read_config(path: String) -> Config {
  let res =
    simplifile.read(path) |> hard_fail("could not read config file at " <> path)
  let gleam =
    tom.parse(res) |> hard_fail("could not read config file at " <> path)

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
    //read from flags only
    force: False,
    //read from flags only
    fake: False,
    format: parse_config_format(format),
    ignore_packages: list.map(packages, toml_as_string) |> option.values,
    ignore_severity: list.map(severity, toml_as_string) |> option.values,
    ignore_ids: list.map(ids, toml_as_string) |> option.values,
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

pub fn parse_config_format(val: String) -> Format {
  case val {
    "json" -> JSON
    "detailed" -> Detailed
    _ -> Minimal
  }
}

fn toml_as_string(toml: Toml) -> Option(String) {
  case toml {
    tom.String(s) -> Some(s)
    _ -> {
      print.warning("could not parse config value " <> string.inspect(toml))
      shellout.exit(1)
      None
    }
  }
}
