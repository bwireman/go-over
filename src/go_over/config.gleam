import clip
import clip/arg_info
import clip/flag
import clip/help
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import go_over/advisories/advisories.{type Advisory}
import go_over/hex/puller
import go_over/packages.{type Package}
import go_over/util/constants
import go_over/util/print
import go_over/util/util
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
  SARIF
}

pub type Config {
  Config(
    dev_deps: List(String),
    outdated: Bool,
    ignore_indirect: Bool,
    force: Bool,
    format: Format,
    verbose: Bool,
    global: Bool,
    puller: puller.Puller,
    allowed_licenses: List(String),
    ignore_packages: List(String),
    ignore_severity: List(String),
    ignore_ids: List(String),
    ignore_dev_dependencies: Bool,
    workspace_max_depth: Int,
    single_root: Option(String),
    workspace_root: Option(String),
  )
}

pub type Flags {
  Flags(
    force: Bool,
    outdated: Bool,
    global: Bool,
    local: Bool,
    verbose: Bool,
    format: option.Option(Format),
    puller: option.Option(puller.Puller),
    single_root: option.Option(String),
    workspace_root: option.Option(String),
    sarif_output: option.Option(String),
  )
}

pub fn default_config() -> Config {
  Config(
    dev_deps: [],
    outdated: False,
    ignore_indirect: False,
    force: False,
    format: Minimal,
    verbose: False,
    global: True,
    puller: puller.default,
    allowed_licenses: [],
    ignore_packages: [],
    ignore_severity: [],
    ignore_ids: [],
    ignore_dev_dependencies: False,
    workspace_max_depth: 3,
    single_root: option.None,
    workspace_root: option.None,
  )
}

fn read_dev_dependency_names(gleam: dict.Dict(String, Toml)) -> List(String) {
  let hyphen =
    tom.get_table(gleam, ["dev-dependencies"])
    |> result.unwrap(dict.new())
  let underscore =
    tom.get_table(gleam, ["dev_dependencies"])
    |> result.unwrap(dict.new())

  dict.merge(hyphen, underscore) |> dict.keys()
}

pub fn read_config(path: String) -> Config {
  let res =
    simplifile.read(path)
    |> cli.hard_fail_with_msg("could not read config file at " <> path)
  let gleam =
    tom.parse(res)
    |> cli.hard_fail_with_msg("could not read config file at " <> path)
  let dev_deps = read_dev_dependency_names(gleam)

  let go_over =
    tom.get_table(gleam, ["go-over"])
    |> result.unwrap(dict.new())

  let force =
    tom.get_bool(go_over, ["force"])
    |> result.unwrap(False)
  let workspace_max_depth =
    tom.get_int(go_over, ["workspace_max_depth"])
    |> result.unwrap(3)
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
    |> result.unwrap(puller.default_string)
    |> string.lowercase()
    |> parse_puller()
    |> option.unwrap(puller.default)
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
    force:,
    verbose: False,
    global:,
    puller:,
    allowed_licenses:,
    format:,
    ignore_packages:,
    ignore_severity:,
    ignore_ids:,
    ignore_dev_dependencies:,
    workspace_max_depth:,
    single_root: option.None,
    workspace_root: option.None,
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

pub fn filter_package_warnings(
  conf: Config,
  warnings: List(Warning),
) -> List(Warning) {
  glist.reject_contains_tap(warnings, fn(w) { w.package }, conf.ignore_packages)
}

pub fn filter_severity(conf: Config, warnings: List(Warning)) -> List(Warning) {
  glist.reject_contains_tap(
    warnings,
    fn(w) { warning.severity_as_string(w.severity) },
    conf.ignore_severity,
  )
}

pub fn unnecessary_ignore_warnings(
  conf: Config,
  manifest_pkgs: List(Package),
  audit_warnings: List(Warning),
  dependency_licenses: List(String),
  all_advisories: List(Advisory),
) -> List(Warning) {
  let manifest_names = list.map(manifest_pkgs, fn(pkg) { pkg.name })
  let packages_with_warnings = list.map(audit_warnings, fn(w) { w.package })
  let severities_present =
    audit_warnings
    |> list.map(fn(w) {
      string.lowercase(warning.severity_as_string(w.severity))
    })

  let package_warnings =
    conf.ignore_packages
    |> list.flat_map(fn(name) {
      case list.contains(manifest_names, name) {
        False -> [
          warning.info_to_warning(
            name,
            "Info: package '" <> name <> "' is not a dependency",
          ),
        ]
        True ->
          case list.contains(packages_with_warnings, name) {
            True -> []
            False -> [
              warning.info_to_warning(
                name,
                "Info: package '" <> name <> "' did not match any warnings",
              ),
            ]
          }
      }
    })

  let license_warnings =
    conf.allowed_licenses
    |> list.filter(fn(license) { !list.contains(dependency_licenses, license) })
    |> list.map(fn(license) {
      warning.info_to_warning(
        license,
        "Info: license '"
          <> license
          <> "' did not match any dependency licenses",
      )
    })

  let severity_warnings =
    conf.ignore_severity
    |> list.filter(fn(sev) {
      !list.contains(severities_present, string.lowercase(sev))
    })
    |> list.map(fn(sev) {
      warning.info_to_warning(
        sev,
        "Info: severity '" <> sev <> "' did not match any warnings",
      )
    })

  let id_warnings =
    unnecessary_ignore_id_warnings(conf, manifest_names, all_advisories)

  let indirect_warnings = case conf.ignore_indirect {
    False -> []
    True ->
      case list.any(manifest_pkgs, fn(pkg) { !pkg.direct }) {
        True -> []
        False -> [
          warning.info_to_warning(
            "indirect",
            "Info: indirect=true has no effect (no indirect dependencies)",
          ),
        ]
      }
  }

  let dev_dep_warnings = case conf.ignore_dev_dependencies {
    False -> []
    True ->
      case conf.dev_deps {
        [] -> [
          warning.info_to_warning(
            "dev_dependencies",
            "Info: dev_dependencies=true has no effect (no dev-dependencies configured)",
          ),
        ]
        dev_deps ->
          case list.any(dev_deps, list.contains(manifest_names, _)) {
            True -> []
            False -> [
              warning.info_to_warning(
                "dev_dependencies",
                "Info: dev_dependencies=true has no effect (no dev-dependencies in manifest)",
              ),
            ]
          }
      }
  }

  package_warnings
  |> list.append(license_warnings)
  |> list.append(severity_warnings)
  |> list.append(id_warnings)
  |> list.append(indirect_warnings)
  |> list.append(dev_dep_warnings)
}

pub fn unnecessary_ignore_id_warnings(
  conf: Config,
  manifest_names: List(String),
  all_advisories: List(Advisory),
) -> List(Warning) {
  conf.ignore_ids
  |> list.flat_map(fn(id) {
    case list.find(all_advisories, fn(a: Advisory) { a.id == id }) {
      Error(_) -> [
        warning.info_to_warning(
          id,
          "Info: advisory id '" <> id <> "' is unknown",
        ),
      ]
      Ok(adv) ->
        case list.contains(manifest_names, adv.name) {
          True -> []
          False -> [
            warning.info_to_warning(
              id,
              "Info: advisory id '"
                <> id
                <> "' does not apply to any dependency",
            ),
          ]
        }
    }
  })
}

pub fn parse_config_format(val: String) -> option.Option(Format) {
  case string.lowercase(val) {
    "json" -> option.Some(JSON)
    "sarif" -> option.Some(SARIF)
    "detailed" -> option.Some(Detailed)
    "minimal" -> option.Some(Minimal)
    format -> {
      print.warning(
        "Invalid format '"
        <> format
        <> "' valid options are ['json', 'sarif', 'detailed', 'minimal'], defaulting to minimal",
      )
      option.None
    }
  }
}

fn parse_puller(name: String) -> option.Option(puller.Puller) {
  case name {
    "native" -> Some(puller.Native)
    "curl" -> Some(puller.CURL)
    "wget" -> Some(puller.WGET)
    "httpie" -> Some(puller.HTTPIE)
    _ -> {
      print.warning(
        "Invalid puller '"
        <> name
        <> "' valid options are ['native', 'curl', 'wget', 'httpie'], defaulting to "
        <> puller.default_string,
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
      util.do_panic()
    }
  }
}

const logo = "                       ____  ____      ____ _   _____  _____
                        / __ `/ __ \\    / __ \\ | / / _ \\/ ___/
                       / /_/ / /_/ /   / /_/ / |/ /  __/ /
                       \\__, /\\____/____\\____/|___/\\___/_/
                      /____/     /_____/"

const help_named_opts = [
  arg_info.NamedInfo(
    name: "format",
    short: option.None,
    default: option.None,
    help: option.Some(
      "Specify the output format of any warnings, [minimal, detailed, json, sarif]",
    ),
  ),
  arg_info.NamedInfo(
    name: "sarif-output",
    short: option.None,
    default: option.None,
    help: option.Some(
      "Write SARIF output to PATH instead of stdout (requires --format sarif)",
    ),
  ),
  arg_info.NamedInfo(
    name: "puller",
    short: option.None,
    default: option.None,
    help: option.Some(
      "Specify the tool used to reach out to hex.pm, [native, curl, wget, httpie]",
    ),
  ),
  arg_info.NamedInfo(
    name: "root",
    short: option.None,
    default: option.None,
    help: option.Some("Audit a single Gleam project at PATH"),
  ),
  arg_info.NamedInfo(
    name: "workspace",
    short: option.None,
    default: option.None,
    help: option.Some("Audit all Gleam projects under PATH (defaults to .)"),
  ),
]

fn help_message(args: arg_info.ArgInfo) -> String {
  arg_info.ArgInfo(
    named: list.append(help_named_opts, args.named),
    positional: args.positional,
    flags: args.flags,
    subcommands: args.subcommands,
  )
  |> arg_info.help_text(
    "go_over",
    print.format_high(logo <> "\tversion: " <> constants.version <> "\n")
      <> "🕵️ Audit Erlang & Elixir dependencies, to make sure your gleam projects really ✨ sparkle!",
  )
  // ? strip out the pointless leading go_over in the help message
  |> string.crop(" ")
}

pub fn normalize_workspace_argv(argv: List(String)) -> List(String) {
  case argv {
    ["--workspace", next, ..rest] ->
      case string.starts_with(next, "-") {
        True -> ["--workspace", ".", next, ..rest]
        False -> argv
      }
    ["--workspace", ..rest] -> ["--workspace", ".", ..rest]
    [head, ..rest] -> [head, ..normalize_workspace_argv(rest)]
    [] -> []
  }
}

pub fn merge_flags_and_config(
  flags: Flags,
  cfg: Config,
) -> Result(Config, String) {
  let invalid = case flags.global && flags.local {
    True -> Error("cannot set --local && global")
    _ -> Ok(Nil)
  }
  use _ <- result.try(invalid)

  let invalid = case flags.single_root, flags.workspace_root {
    option.Some(_), option.Some(_) -> Error("cannot set --root and --workspace")
    _, _ -> Ok(Nil)
  }
  use _ <- result.try(invalid)

  let global = case flags.global, flags.local, cfg.global {
    True, False, _ -> True
    False, True, _ -> False
    False, False, _ -> cfg.global
    True, True, _ -> util.do_panic()
  }

  Ok(Config(
    dev_deps: cfg.dev_deps,
    force: flags.force || cfg.force,
    outdated: flags.outdated || cfg.outdated,
    ignore_indirect: cfg.ignore_indirect,
    verbose: flags.verbose,
    allowed_licenses: cfg.allowed_licenses,
    puller: option.unwrap(flags.puller, cfg.puller),
    global:,
    format: option.unwrap(flags.format, cfg.format),
    ignore_packages: cfg.ignore_packages,
    ignore_severity: cfg.ignore_severity,
    ignore_ids: cfg.ignore_ids,
    ignore_dev_dependencies: cfg.ignore_dev_dependencies,
    workspace_max_depth: cfg.workspace_max_depth,
    single_root: flags.single_root,
    workspace_root: flags.workspace_root,
  ))
}

pub fn validate_workspace_formats(
  results: List(#(Format, String)),
) -> Result(Nil, String) {
  case results {
    [] -> Ok(Nil)
    [#(first, _), ..rest] -> {
      case list.any(rest, fn(r) { r.0 != first }) {
        True ->
          Error(
            "workspace projects have mismatched output formats; set format consistently or use --format",
          )
        False -> Ok(Nil)
      }
    }
  }
}

fn take_named_opts(
  argv: List(String),
) -> #(
  Option(String),
  Option(String),
  Option(String),
  Option(String),
  Option(String),
  List(String),
) {
  case argv {
    ["--format", value, ..rest] -> {
      let #(_, puller, root, workspace, sarif_output, remaining) =
        take_named_opts(rest)
      #(Some(value), puller, root, workspace, sarif_output, remaining)
    }
    ["--puller", value, ..rest] -> {
      let #(format, _, root, workspace, sarif_output, remaining) =
        take_named_opts(rest)
      #(format, Some(value), root, workspace, sarif_output, remaining)
    }
    ["--root", value, ..rest] -> {
      let #(format, puller, _, workspace, sarif_output, remaining) =
        take_named_opts(rest)
      #(format, puller, Some(value), workspace, sarif_output, remaining)
    }
    ["--workspace", value, ..rest] -> {
      let #(format, puller, root, _, sarif_output, remaining) =
        take_named_opts(rest)
      #(format, puller, root, Some(value), sarif_output, remaining)
    }
    ["--sarif-output", value, ..rest] -> {
      let #(format, puller, root, workspace, _, remaining) =
        take_named_opts(rest)
      #(format, puller, root, workspace, Some(value), remaining)
    }
    [head, ..rest] -> {
      let #(format, puller, root, workspace, sarif_output, remaining) =
        take_named_opts(rest)
      #(format, puller, root, workspace, sarif_output, [head, ..remaining])
    }
    [] -> #(None, None, None, None, None, [])
  }
}

fn clip_command() {
  clip.command({
    use force <- clip.parameter
    use outdated <- clip.parameter
    use global <- clip.parameter
    use local <- clip.parameter
    use verbose <- clip.parameter

    Flags(
      force:,
      outdated:,
      verbose:,
      format: option.None,
      global:,
      local:,
      puller: option.None,
      single_root: option.None,
      workspace_root: option.None,
      sarif_output: option.None,
    )
  })
  |> clip.flag(flag.help(
    flag.new("force"),
    "Force pulling new data even if the cached data is still valid",
  ))
  |> clip.flag(flag.help(
    flag.new("outdated"),
    "[deprecated] Use `gleam deps outdated` to check for newer dependency versions",
  ))
  |> clip.flag(flag.help(
    flag.new("global"),
    "Cache data globally in user's home directory for use by multiple projects",
  ))
  |> clip.flag(flag.help(
    flag.new("local"),
    "Cache data in the project's .go-over/ directory for use only by this project",
  ))
  |> clip.flag(flag.help(
    flag.new("verbose"),
    "Print progress as packages are checked",
  ))
  |> clip.help(help.custom(help_message))
}

pub fn parse_flags(argv: List(String)) -> Result(Flags, String) {
  let argv = normalize_workspace_argv(argv)
  let #(format, puller, single_root, workspace_root, sarif_output, argv) =
    take_named_opts(argv)

  use base <- result.try(
    clip_command()
    |> clip.run(cli.strip_js_from_argv(argv)),
  )

  Ok(
    Flags(
      ..base,
      format: option.map(format, parse_config_format) |> option.flatten,
      puller: option.map(puller, parse_puller) |> option.flatten,
      single_root:,
      workspace_root:,
      sarif_output:,
    ),
  )
}

pub fn spin_up(cfg: Config, argv: List(String)) -> Result(Config, String) {
  use flags <- result.try(parse_flags(argv))
  merge_flags_and_config(flags, cfg)
}
