import clip
import clip/arg_info
import clip/flag
import clip/help
import clip/opt
import gleam/dict
import gleam/list
import gleam/option.{type Option, Some}
import gleam/result
import gleam/string
import go_over/advisories/advisories.{type Advisory}
import go_over/hex/puller
import go_over/packages.{type Package}
import go_over/util/constants
import go_over/util/print
import go_over/warning.{type Warning}
import gxyz/cli
import gxyz/list as glist
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
    global: Bool,
    puller: puller.Puller,
    allowed_licenses: List(String),
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
    puller: option.Option(puller.Puller),
  )
}

pub fn read_config(path: String) -> Config {
  let res =
    simplifile.read(path)
    |> cli.hard_fail_with_msg("could not read config file at " <> path)
  let gleam =
    tom.parse(res)
    |> cli.hard_fail_with_msg("could not read config file at " <> path)
  let dev_deps =
    tom.get_table(gleam, ["dev-dependencies"])
    |> result.unwrap(dict.new())
    |> dict.keys()

  let go_over =
    tom.get_table(gleam, ["go-over"])
    |> result.unwrap(dict.new())

  let cache =
    tom.get_bool(go_over, ["cache"])
    |> result.unwrap(True)
  let outdated =
    tom.get_bool(go_over, ["outdated"])
    |> result.unwrap(False)
  let format =
    tom.get_string(go_over, ["format"])
    |> result.unwrap("minimal")
    |> string.lowercase()
    |> parse_config_format()
    |> option.unwrap(Minimal)
  let puller =
    tom.get_string(go_over, ["puller"])
    |> result.unwrap("curl")
    |> string.lowercase()
    |> parse_puller()
    |> option.unwrap(puller.CURL)
  let global =
    tom.get_bool(go_over, ["global"])
    |> result.unwrap(True)
  let allowed_licenses =
    tom.get_array(go_over, ["allowed_licenses"])
    |> result.map(list.map(_, toml_as_string))
    |> result.map(option.values)
    |> result.unwrap([])

  let ignore =
    tom.get_table(go_over, ["ignore"])
    |> result.unwrap(dict.new())
  let ignore_indirect =
    tom.get_bool(ignore, ["indirect"])
    |> result.unwrap(False)
  let ignore_packages =
    tom.get_array(ignore, ["packages"])
    |> result.unwrap([])
    |> list.map(toml_as_string)
    |> option.values()
  let ignore_severity =
    tom.get_array(ignore, ["severity"])
    |> result.unwrap([])
    |> list.map(toml_as_string)
    |> option.values()
  let ignore_ids =
    tom.get_array(ignore, ["ids"])
    |> result.unwrap([])
    |> list.map(toml_as_string)
    |> option.values()
  let ignore_dev_dependencies =
    tom.get_bool(ignore, ["dev_dependencies"])
    |> result.unwrap(False)

  Config(
    dev_deps:,
    outdated:,
    ignore_indirect:,
    force: !cache,
    //read from flags only
    fake: False,
    //read from flags only
    verbose: False,
    global:,
    puller:,
    allowed_licenses:,
    format:,
    ignore_packages:,
    ignore_severity:,
    ignore_ids:,
    ignore_dev_dependencies:,
  )
}

pub fn filter_packages(conf: Config, pkgs: List(Package)) -> List(Package) {
  glist.reject_contains_tap(pkgs, fn(pkg) { pkg.name }, conf.ignore_packages)
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
    True -> glist.reject_contains_tap(pkgs, fn(pkg) { pkg.name }, conf.dev_deps)
  }
}

pub fn filter_advisory_ids(
  conf: Config,
  advisories: List(Advisory),
) -> List(Advisory) {
  glist.reject_contains_tap(advisories, fn(adv) { adv.id }, conf.ignore_ids)
}

pub fn filter_severity(conf: Config, warnings: List(Warning)) -> List(Warning) {
  glist.reject_contains_tap(
    warnings,
    fn(w) { warning.severity_as_string(w.severity) },
    conf.ignore_severity,
  )
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

fn parse_puller(name: String) -> option.Option(puller.Puller) {
  case name {
    "curl" -> Some(puller.CURL)
    "wget" -> Some(puller.WGET)
    "httpie" -> Some(puller.HTTPIE)
    _ -> {
      print.warning(
        "Invalid puller '"
        <> name
        <> "' valid options are ['curl', 'wget', 'httpie'], defaulting to curl",
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

const logo = "                       ____  ____      ____ _   _____  _____
                        / __ `/ __ \\    / __ \\ | / / _ \\/ ___/
                       / /_/ / /_/ /   / /_/ / |/ /  __/ /
                       \\__, /\\____/____\\____/|___/\\___/_/
                      /____/     /_____/"

// ? want to hide the `fake` flag but otherwise use the default help
fn help_message(args: arg_info.ArgInfo) -> String {
  arg_info.ArgInfo(
    named: args.named,
    positional: args.positional,
    flags: glist.reject(args.flags, fn(f) { f.name == "fake" }),
    subcommands: args.subcommands,
  )
  |> arg_info.help_text(
    "go_over",
    print.format_high(logo <> "\tversion: " <> constants.version <> "\n")
      <> "🕵️Audit Erlang & Elixir dependencies, to make sure your gleam projects really ✨ sparkle!",
  )
  // ? strip out the pointless leading go_over in the help message
  |> string.crop(" ")
}

pub fn merge_flags_and_config(flags: Flags, cfg: Config) -> Config {
  let global = case flags.global {
    True -> True
    False -> cfg.global
  }

  Config(
    dev_deps: cfg.dev_deps,
    force: flags.force || cfg.force,
    outdated: flags.outdated || cfg.outdated,
    ignore_indirect: cfg.ignore_indirect || flags.ignore_indirect,
    fake: flags.fake,
    verbose: flags.verbose,
    allowed_licenses: cfg.allowed_licenses,
    puller: cfg.puller,
    global:,
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
    use puller <- clip.parameter

    merge_flags_and_config(
      Flags(
        force:,
        outdated:,
        ignore_indirect:,
        fake:,
        verbose:,
        format:,
        global:,
        puller:,
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
    "Cache data globally in user's home directory for use by multiple projects",
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
  |> clip.opt(
    opt.new("puller")
    |> opt.help(
      "Specify the tool used to reach out to hex.pm, [curl, wget, httpie]",
    )
    |> opt.map(parse_puller)
    |> opt.default(option.None),
  )
  |> clip.help(help.custom(help_message))
  |> clip.run(cli.strip_js_from_argv(argv))
}
