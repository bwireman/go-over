import clip
import clip/arg_info
import clip/flag
import clip/help
import clip/opt
import gleam/dict
import gleam/list
import gleam/option.{type Option, Some}
import gleam/result.{unwrap}
import gleam/string
import go_over/advisories/advisories.{type Advisory}
import go_over/packages.{type Package}
import go_over/util/print
import go_over/util/util.{hard_fail}
import go_over/warning.{type Warning}
import gxyz/gxyz_list
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
    dev_deps: List(String),
    outdated: Bool,
    ignore_indirect: Bool,
    force: Bool,
    fake: Bool,
    format: Format,
    verbose: Bool,
    local: Bool,
    ignore_packages: List(String),
    ignore_severity: List(String),
    ignore_ids: List(String),
    ignore_dev_dependencies: Bool,
  )
}

pub type Flags {
  Flags(
    force: Bool,
    fake: Bool,
    outdated: Bool,
    ignore_indirect: Bool,
    global: Bool,
    verbose: Bool,
    format: option.Option(Format),
  )
}

pub fn read_config(path: String) -> Config {
  let res =
    simplifile.read(path) |> hard_fail("could not read config file at " <> path)
  let gleam =
    tom.parse(res) |> hard_fail("could not read config file at " <> path)
  let dev_deps =
    tom.get_table(gleam, ["dev-dependencies"])
    |> unwrap(dict.new())
    |> dict.keys()

  let go_over =
    tom.get_table(gleam, ["go-over"])
    |> unwrap(dict.new())
  let ignore =
    tom.get_table(go_over, ["ignore"])
    |> unwrap(dict.new())
  let cache =
    tom.get_bool(go_over, ["cache"])
    |> unwrap(True)
  let outdated =
    tom.get_bool(go_over, ["outdated"])
    |> unwrap(False)
  let format =
    tom.get_string(go_over, ["format"])
    |> unwrap("minimal")
    |> string.lowercase()
  let local =
    tom.get_bool(go_over, ["local"])
    |> unwrap(True)

  let ignore_indirect =
    tom.get_bool(ignore, ["indirect"])
    |> result.lazy_unwrap(fn() {
      case tom.get_bool(go_over, ["ignore_indirect"]) {
        Ok(value) -> {
          print.high(
            "Warning: `go-over.ignore_indirect` is deprecated, use `go-over.ignore.indirect` instead",
          )

          value
        }

        Error(_) -> False
      }
    })

  let packages =
    tom.get_array(ignore, ["packages"])
    |> unwrap([])
  let severity =
    tom.get_array(ignore, ["severity"])
    |> unwrap([])
  let ids =
    tom.get_array(ignore, ["ids"])
    |> unwrap([])
  let ignore_dev_dependencies =
    tom.get_bool(ignore, ["dev_dependencies"])
    |> unwrap(False)

  Config(
    dev_deps: dev_deps,
    outdated: outdated,
    ignore_indirect: ignore_indirect,
    force: !cache,
    //read from flags only
    fake: False,
    //read from flags only
    verbose: False,
    local: local,
    format: parse_config_format(format) |> option.unwrap(Minimal),
    ignore_packages: list.map(packages, toml_as_string) |> option.values,
    ignore_severity: list.map(severity, toml_as_string) |> option.values,
    ignore_ids: list.map(ids, toml_as_string) |> option.values,
    ignore_dev_dependencies: ignore_dev_dependencies,
  )
}

pub fn filter_packages(conf: Config, pkgs: List(Package)) -> List(Package) {
  gxyz_list.reject(pkgs, fn(pkg) {
    list.contains(conf.ignore_packages, pkg.name)
  })
}

pub fn filter_indirect(conf: Config, pkgs: List(Package)) -> List(Package) {
  case conf.ignore_indirect {
    False -> pkgs
    True -> list.filter(pkgs, fn(pkg) { pkg.direct })
  }
}

pub fn filter_dev_dependencies(
  conf: Config,
  pkgs: List(Package),
) -> List(Package) {
  case conf.ignore_dev_dependencies {
    False -> pkgs
    True ->
      gxyz_list.reject(pkgs, fn(pkg) { list.contains(conf.dev_deps, pkg.name) })
  }
}

pub fn filter_advisory_ids(
  conf: Config,
  advisories: List(Advisory),
) -> List(Advisory) {
  gxyz_list.reject(advisories, fn(adv) {
    list.contains(conf.ignore_ids, adv.id)
  })
}

pub fn filter_severity(conf: Config, warnings: List(Warning)) -> List(Warning) {
  gxyz_list.reject(warnings, fn(w) {
    list.contains(conf.ignore_severity, string.lowercase(w.severity))
  })
}

pub fn parse_config_format(val: String) -> option.Option(Format) {
  case string.lowercase(val) {
    "json" -> option.Some(JSON)
    "detailed" -> option.Some(Detailed)
    "minimal" -> option.Some(Minimal)
    format -> {
      print.warning(
        "Invalid format '"
        <> format
        <> "' valid options are ['json', 'detailed', 'minimal'], defaulting to minimal",
      )
      option.None
    }
  }
}

fn toml_as_string(toml: Toml) -> Option(String) {
  case toml {
    tom.String(s) -> Some(s)
    _ -> {
      print.warning("could not parse config value " <> string.inspect(toml))
      shellout.exit(1)
      panic as "Unreachable, please create an issue in https://github.com/bwireman/go-over if you see this"
    }
  }
}

// ? want to head the `fake` flag but otherwise use the default help
fn help_message(args: arg_info.ArgInfo) -> String {
  arg_info.ArgInfo(
    named: args.named,
    positional: args.positional,
    flags: gxyz_list.reject(args.flags, fn(f) { f.name == "fake" }),
    subcommands: args.subcommands,
  )
  |> arg_info.help_text(
    "go_over",
    "                       ____  ____      ____ _   _____  _____
                        / __ `/ __ \\    / __ \\ | / / _ \\/ ___/
                       / /_/ / /_/ /   / /_/ / |/ /  __/ /
                       \\__, /\\____/____\\____/|___/\\___/_/
                      /____/     /_____/
"
    |> print.format_high()
      <> "🕵️‍♂️ Audit Erlang & Elixir dependencies, to make sure your gleam projects really ✨ sparkle!",
  )
  // ? strip out the pointless leading go_over in the help message
  |> string.crop(" ")
}

pub fn merge_flags_and_config(flags: Flags, cfg: Config) -> Config {
  let local = case flags.global {
    True -> False
    False -> cfg.local
  }

  Config(
    dev_deps: cfg.dev_deps,
    force: flags.force || cfg.force,
    outdated: flags.outdated || cfg.outdated,
    ignore_indirect: cfg.ignore_indirect || flags.ignore_indirect,
    fake: flags.fake,
    verbose: flags.verbose,
    local:,
    format: option.unwrap(flags.format, cfg.format),
    ignore_packages: cfg.ignore_packages,
    ignore_severity: cfg.ignore_severity,
    ignore_ids: cfg.ignore_ids,
    ignore_dev_dependencies: cfg.ignore_dev_dependencies,
  )
}

pub fn spin_up(cfg: Config, argv: List(String)) -> Result(Config, String) {
  clip.command({
    use force <- clip.parameter
    use outdated <- clip.parameter
    use ignore_indirect <- clip.parameter
    use global <- clip.parameter
    use fake <- clip.parameter
    use verbose <- clip.parameter
    use format <- clip.parameter

    merge_flags_and_config(
      Flags(
        force:,
        outdated:,
        ignore_indirect:,
        fake:,
        verbose:,
        format:,
        global: global,
      ),
      cfg,
    )
  })
  |> clip.flag(flag.help(
    flag.new("force"),
    "Force pulling new data even if the cached data is still valid",
  ))
  |> clip.flag(flag.help(
    flag.new("outdated"),
    "Additionally check if newer versions of dependencies exist",
  ))
  |> clip.flag(flag.help(
    flag.new("ignore-indirect"),
    "Ignore all warnings for indirect dependencies",
  ))
  |> clip.flag(flag.help(
    flag.new("global"),
    "cache all info in $HOME directory instead of locally",
  ))
  |> clip.flag(flag.new("fake"))
  |> clip.flag(flag.help(
    flag.new("verbose"),
    "Print progress as packages are checked",
  ))
  |> clip.opt(
    opt.new("format")
    |> opt.help(
      "Specify the output format of any warnings, [minimal, verbose, json]",
    )
    |> opt.map(parse_config_format)
    |> opt.default(option.None),
  )
  |> clip.help(help.custom(help_message))
  |> clip.run(
    argv
    |> gxyz_list.reject(string.ends_with(_, ".js"))
    |> gxyz_list.reject(string.ends_with(_, ".mjs"))
    |> gxyz_list.reject(string.ends_with(_, ".cjs")),
  )
}
