import gleam/list
import gleam/option.{None}
import gleamsver.{parse}
import go_over/advisories/advisories.{Advisory}
import go_over/config.{
  filter_advisory_ids, filter_dev_dependencies, filter_package_warnings,
  filter_packages, filter_severity, read_config, unnecessary_ignore_id_warnings,
  unnecessary_ignore_warnings,
}
import go_over/hex/puller
import go_over/packages.{Package}
import go_over/warning.{Warning}
import go_over_test

fn empty_conf() {
  config.read_config("test/testdata/gleam/empty.toml")
}

pub fn test_read_config(p: String) {
  p
  |> read_config()
  |> go_over_test.birdie_snap("conf_test@" <> p)
}

pub fn test_spin_up(name: String, argv: List(String)) {
  let assert Ok(conf) =
    empty_conf()
    |> config.spin_up(argv)

  go_over_test.birdie_snap_with_input(conf, argv, "spin_up_test@" <> name)
}

pub fn read_config_test() {
  let empty = test_read_config("test/testdata/gleam/empty.toml")
  assert !empty.force
  assert empty.format == config.Minimal
  assert empty.puller == puller.default
  assert empty.ignore_packages == []
  assert empty.ignore_severity == []
  assert empty.ignore_ids == []
  assert !empty.ignore_indirect

  let basic = test_read_config("test/testdata/gleam/basic.toml")
  assert basic.force
  assert basic.puller == puller.HTTPIE
  assert basic.format == config.Detailed
  assert basic.ignore_packages == ["a"]
  assert basic.ignore_severity == ["b"]
  assert basic.ignore_ids == ["c"]
  assert basic.ignore_indirect

  let partial = test_read_config("test/testdata/gleam/partial.toml")
  assert !partial.force
  assert partial.puller == puller.WGET
  assert partial.format == config.Minimal
  assert partial.ignore_packages == ["a", "b", "c"]
  assert partial.ignore_severity == []
  assert partial.ignore_ids == []
  assert !partial.ignore_indirect

  let indirect_new = test_read_config("test/testdata/gleam/indirect_new.toml")
  assert indirect_new.ignore_indirect
}

pub fn filter_dev_deps_test() {
  let full = test_read_config("test/testdata/gleam/full.toml")
  let assert Ok(v) = parse("1.1.1")
  let a = Package("a", v, "", False, packages.PackageSourceHex)
  let b = Package("b", v, "", False, packages.PackageSourceHex)
  let c = Package("c", v, "", False, packages.PackageSourceHex)

  assert filter_dev_dependencies(full, []) == []
  assert filter_dev_dependencies(full, [a]) == [a]
  assert filter_dev_dependencies(full, [a, b]) == [a, b]
  assert filter_dev_dependencies(full, [b, c]) == [b]
  assert filter_dev_dependencies(full, [a, b, c]) == [a, b]
}

pub fn filter_packages_test() {
  let full = test_read_config("test/testdata/gleam/full.toml")
  let assert Ok(v) = parse("1.1.1")
  let a = Package("a", v, "", False, packages.PackageSourceHex)
  let b = Package("b", v, "", False, packages.PackageSourceHex)
  let c = Package("c", v, "", False, packages.PackageSourceHex)

  assert filter_packages(full, []) == []
  assert filter_packages(full, [a]) == []
  assert filter_packages(full, [a, b]) == []
  assert filter_packages(full, [b, c]) == [c]
  assert filter_packages(full, [a, b, c]) == [c]
}

pub fn filter_advisory_ids_test() {
  let full = test_read_config("test/testdata/gleam/full.toml")
  let a = Advisory("a", "", "", [], "")
  let b = Advisory("b", "", "", [], "")
  let c = Advisory("c", "", "", [], "")

  assert filter_advisory_ids(full, []) == []
  assert filter_advisory_ids(full, [a]) == []
  assert filter_advisory_ids(full, [a, b]) == []
  assert filter_advisory_ids(full, [b, c]) == [c]
  assert filter_advisory_ids(full, [a, b, c]) == [c]
}

pub fn filter_severity_test() {
  let full = test_read_config("test/testdata/gleam/full.toml")
  let a =
    Warning(
      None,
      "",
      None,
      "",
      warning.WarningReasonVulnerable,
      warning.SeverityCritical,
      warning.DirectDep,
    )
  let b =
    Warning(
      None,
      "",
      None,
      "",
      warning.WarningReasonVulnerable,
      warning.SeverityHigh,
      warning.DirectDep,
    )
  let c =
    Warning(
      None,
      "",
      None,
      "",
      warning.WarningReasonVulnerable,
      warning.SeverityModerate,
      warning.DirectDep,
    )
  let aa =
    Warning(
      None,
      "",
      None,
      "",
      warning.WarningReasonVulnerable,
      warning.SeverityCritical,
      warning.DirectDep,
    )
  let bb =
    Warning(
      None,
      "",
      None,
      "",
      warning.WarningReasonVulnerable,
      warning.SeverityHigh,
      warning.DirectDep,
    )
  let cc =
    Warning(
      None,
      "",
      None,
      "",
      warning.WarningReasonVulnerable,
      warning.SeverityPackageRetiredSecurity,
      warning.DirectDep,
    )

  assert filter_severity(full, []) == []
  assert filter_severity(full, [a]) == []
  assert filter_severity(full, [a, b]) == []
  assert filter_severity(full, [b, c]) == [c]
  assert filter_severity(full, [a, b, c]) == [c]

  assert filter_severity(full, []) == []
  assert filter_severity(full, [aa]) == []
  assert filter_severity(full, [aa, bb]) == []
  assert filter_severity(full, [bb, cc]) == [cc]
  assert filter_severity(full, [aa, bb, cc]) == [cc]
}

pub fn spin_up_test() {
  let conf = test_spin_up("empty", [])
  assert !conf.force
  assert !conf.outdated
  assert !conf.ignore_indirect
  assert !conf.verbose
  assert conf.format == config.Minimal
  assert conf.puller == puller.default

  let conf = test_spin_up("force", ["--force"])
  assert conf.force

  let conf = test_spin_up("verbose", ["--verbose"])
  assert conf.verbose
}

pub fn spin_up_format_test() {
  let conf = test_spin_up("format=minimal", ["--format", "minimal"])
  assert conf.format == config.Minimal

  let conf = test_spin_up("format=json", ["--format", "json"])
  assert conf.format == config.JSON

  let conf = test_spin_up("format=detailed", ["--format", "detailed"])
  assert conf.format == config.Detailed
}

const empty_flags = config.Flags(
  force: False,
  outdated: False,
  verbose: False,
  format: option.None,
  global: False,
  local: False,
  puller: option.None,
  single_root: option.None,
  workspace_root: option.None,
  sarif_output: option.None,
)

pub fn merge_flags_and_config_flags_only_test() {
  assert config.merge_flags_and_config(empty_flags, empty_conf())
    == Ok(empty_conf())

  assert config.merge_flags_and_config(
      config.Flags(..empty_flags, outdated: True),
      empty_conf(),
    )
    == Ok(config.Config(..empty_conf(), outdated: True))

  assert config.merge_flags_and_config(
      config.Flags(..empty_flags, force: True),
      empty_conf(),
    )
    == Ok(config.Config(..empty_conf(), force: True))

  assert config.merge_flags_and_config(
      config.Flags(..empty_flags, global: True),
      empty_conf(),
    )
    == Ok(config.Config(..empty_conf(), global: True))

  assert config.merge_flags_and_config(
      config.Flags(..empty_flags, local: True),
      empty_conf(),
    )
    == Ok(config.Config(..empty_conf(), global: False))

  let assert Error(_) =
    config.merge_flags_and_config(
      config.Flags(..empty_flags, local: True, global: True),
      empty_conf(),
    )

  assert config.merge_flags_and_config(
      config.Flags(..empty_flags, format: option.Some(config.JSON)),
      empty_conf(),
    )
    == Ok(config.Config(..empty_conf(), format: config.JSON))

  assert config.merge_flags_and_config(
      config.Flags(..empty_flags, format: option.Some(config.Detailed)),
      empty_conf(),
    )
    == Ok(config.Config(..empty_conf(), format: config.Detailed))
}

pub fn merge_flags_and_config_conf_only_test() {
  assert config.merge_flags_and_config(
      empty_flags,
      config.Config(..empty_conf(), outdated: True),
    )
    == Ok(config.Config(..empty_conf(), outdated: True))

  assert config.merge_flags_and_config(
      empty_flags,
      config.Config(..empty_conf(), ignore_indirect: True),
    )
    == Ok(config.Config(..empty_conf(), ignore_indirect: True))

  assert config.merge_flags_and_config(
      empty_flags,
      config.Config(..empty_conf(), force: True),
    )
    == Ok(config.Config(..empty_conf(), force: True))

  assert config.merge_flags_and_config(
      empty_flags,
      config.Config(..empty_conf(), global: True),
    )
    == Ok(config.Config(..empty_conf(), global: True))

  assert config.merge_flags_and_config(
      empty_flags,
      config.Config(..empty_conf(), format: config.JSON),
    )
    == Ok(config.Config(..empty_conf(), format: config.JSON))

  assert config.merge_flags_and_config(
      empty_flags,
      config.Config(..empty_conf(), format: config.Detailed),
    )
    == Ok(config.Config(..empty_conf(), format: config.Detailed))
}

pub fn unnecessary_ignore_warnings_test() {
  let assert Ok(v) = parse("1.1.1")
  let a = Package("a", v, "", True, packages.PackageSourceHex)
  let b = Package("b", v, "", False, packages.PackageSourceHex)
  let manifest = [a, b]

  let conf =
    config.Config(
      ..empty_conf(),
      ignore_packages: ["a", "b", "missing"],
      ignore_severity: ["critical", "missing-severity"],
      ignore_indirect: True,
    )

  let audit_warning =
    Warning(
      None,
      "a",
      None,
      "",
      warning.WarningReasonVulnerable,
      warning.SeverityCritical,
      warning.DirectDep,
    )

  assert unnecessary_ignore_warnings(conf, manifest, [audit_warning], [], [])
    == [
      warning.info_to_warning(
        "b",
        "Info: package 'b' did not match any warnings",
      ),
      warning.info_to_warning(
        "missing",
        "Info: package 'missing' is not a dependency",
      ),
      warning.info_to_warning(
        "missing-severity",
        "Info: severity 'missing-severity' did not match any warnings",
      ),
    ]
}

pub fn unnecessary_ignore_ignored_package_with_warning_test() {
  let assert Ok(v) = parse("1.1.1")
  let a = Package("a", v, "", True, packages.PackageSourceHex)
  let conf = config.Config(..empty_conf(), ignore_packages: ["a"])

  let audit_warning =
    Warning(
      None,
      "a",
      None,
      "",
      warning.WarningReasonVulnerable,
      warning.SeverityCritical,
      warning.DirectDep,
    )

  assert unnecessary_ignore_warnings(conf, [a], [audit_warning], [], []) == []
}

pub fn validate_workspace_formats_test() {
  let assert Error(msg) =
    config.validate_workspace_formats([
      #(config.Minimal, "a"),
      #(config.JSON, "b"),
    ])

  assert msg
    == "workspace projects have mismatched output formats; set format consistently or use --format"

  assert config.validate_workspace_formats([
      #(config.Minimal, "a"),
      #(config.Minimal, "b"),
    ])
    == Ok(Nil)
}

pub fn unnecessary_ignore_license_warnings_test() {
  let conf =
    config.Config(..empty_conf(), allowed_licenses: [
      "MIT",
      "Apache-2.0",
      "WTFPL",
    ])

  assert unnecessary_ignore_warnings(conf, [], [], ["MIT", "Apache-2.0"], [])
    == [
      warning.info_to_warning(
        "WTFPL",
        "Info: license 'WTFPL' did not match any dependency licenses",
      ),
    ]
}

pub fn filter_package_warnings_test() {
  let full = test_read_config("test/testdata/gleam/full.toml")
  let warning_a =
    Warning(
      None,
      "a",
      None,
      "",
      warning.WarningReasonVulnerable,
      warning.SeverityCritical,
      warning.DirectDep,
    )
  let warning_c =
    Warning(
      None,
      "c",
      None,
      "",
      warning.WarningReasonVulnerable,
      warning.SeverityCritical,
      warning.DirectDep,
    )

  assert filter_package_warnings(full, [warning_a, warning_c]) == [warning_c]
}

pub fn unnecessary_ignore_indirect_test() {
  let assert Ok(v) = parse("1.1.1")
  let direct = Package("a", v, "", True, packages.PackageSourceHex)
  let conf = config.Config(..empty_conf(), ignore_indirect: True)

  assert unnecessary_ignore_warnings(conf, [direct], [], [], [])
    == [
      warning.info_to_warning(
        "indirect",
        "Info: indirect=true has no effect (no indirect dependencies)",
      ),
    ]
}

pub fn unnecessary_ignore_dev_dependencies_test() {
  let assert Ok(v) = parse("1.1.1")
  let pkg = Package("a", v, "", True, packages.PackageSourceHex)
  let no_dev_deps = config.Config(..empty_conf(), ignore_dev_dependencies: True)
  let missing_dev_deps =
    config.Config(
      ..empty_conf(),
      dev_deps: ["not-in-manifest"],
      ignore_dev_dependencies: True,
    )

  assert unnecessary_ignore_warnings(no_dev_deps, [pkg], [], [], [])
    == [
      warning.info_to_warning(
        "dev_dependencies",
        "Info: dev_dependencies=true has no effect (no dev-dependencies configured)",
      ),
    ]

  assert unnecessary_ignore_warnings(missing_dev_deps, [pkg], [], [], [])
    == [
      warning.info_to_warning(
        "dev_dependencies",
        "Info: dev_dependencies=true has no effect (no dev-dependencies in manifest)",
      ),
    ]
}

pub fn unnecessary_ignore_id_warnings_test() {
  let conf = config.Config(..empty_conf(), ignore_ids: ["known", "unknown"])
  let advisories = [
    Advisory("known", "present", "", [], ""),
    Advisory("other", "missing", "", [], ""),
  ]

  assert unnecessary_ignore_id_warnings(conf, ["present"], advisories)
    == [
      warning.info_to_warning(
        "unknown",
        "Info: advisory id 'unknown' is unknown",
      ),
    ]

  let wrong_package = config.Config(..empty_conf(), ignore_ids: ["other"])

  assert unnecessary_ignore_id_warnings(wrong_package, ["present"], advisories)
    == [
      warning.info_to_warning(
        "other",
        "Info: advisory id 'other' does not apply to any dependency",
      ),
    ]
}

pub fn merge_flags_and_config_both_test() {
  assert config.merge_flags_and_config(
      config.Flags(..empty_flags, format: option.Some(config.JSON)),
      config.Config(..empty_conf(), format: config.Minimal),
    )
    == Ok(config.Config(..empty_conf(), format: config.JSON))

  assert config.merge_flags_and_config(
      config.Flags(..empty_flags, format: option.Some(config.Detailed)),
      config.Config(..empty_conf(), format: config.Minimal),
    )
    == Ok(config.Config(..empty_conf(), format: config.Detailed))

  assert config.merge_flags_and_config(
      config.Flags(..empty_flags, global: False),
      config.Config(..empty_conf(), global: False),
    )
    == Ok(config.Config(..empty_conf(), global: False))

  assert config.merge_flags_and_config(
      config.Flags(..empty_flags, global: True),
      config.Config(..empty_conf(), global: True),
    )
    == Ok(config.Config(..empty_conf(), global: True))

  assert config.merge_flags_and_config(
      config.Flags(..empty_flags, global: True),
      config.Config(..empty_conf(), global: False),
    )
    == Ok(config.Config(..empty_conf(), global: True))

  assert config.merge_flags_and_config(
      config.Flags(..empty_flags, global: False),
      config.Config(..empty_conf(), global: True),
    )
    == Ok(config.Config(..empty_conf(), global: True))

  assert config.merge_flags_and_config(
      config.Flags(..empty_flags, local: True),
      config.Config(..empty_conf(), global: True),
    )
    == Ok(config.Config(..empty_conf(), global: False))
}

pub fn read_dev_dependencies_underscore_test() {
  let conf = read_config("test/testdata/gleam/dev_dependencies_underscore.toml")
  assert list.length(conf.dev_deps) == 2
  assert list.contains(conf.dev_deps, "gleeunit")
  assert list.contains(conf.dev_deps, "birdie")
}

pub fn read_workspace_max_depth_test() {
  let conf = read_config("test/testdata/gleam/workspace_max_depth.toml")
  assert conf.workspace_max_depth == 5
}

pub fn normalize_workspace_argv_test() {
  assert config.normalize_workspace_argv(["--workspace"])
    == ["--workspace", "."]
  assert config.normalize_workspace_argv(["--workspace", "backend"])
    == ["--workspace", "backend"]
  assert config.normalize_workspace_argv(["--workspace", "--local"])
    == ["--workspace", ".", "--local"]
  assert config.normalize_workspace_argv(["--workspace", "--format", "json"])
    == ["--workspace", ".", "--format", "json"]
}

pub fn merge_flags_format_overrides_workspace_project_test() {
  let json_project = config.Config(..empty_conf(), format: config.JSON)
  let minimal_project = config.Config(..empty_conf(), format: config.Minimal)
  let cli_sarif = config.Flags(..empty_flags, format: option.Some(config.SARIF))

  assert config.merge_flags_and_config(cli_sarif, json_project)
    == Ok(config.Config(..json_project, format: config.SARIF))
  assert config.merge_flags_and_config(cli_sarif, minimal_project)
    == Ok(config.Config(..minimal_project, format: config.SARIF))
}

pub fn spin_up_root_test() {
  let assert Ok(conf) =
    empty_conf()
    |> config.spin_up(["--root", "backend"])

  assert conf.single_root == option.Some("backend")
  assert conf.workspace_root == option.None
}

pub fn spin_up_workspace_test() {
  let assert Ok(conf) =
    empty_conf()
    |> config.spin_up(["--workspace", "monorepo"])

  assert conf.workspace_root == option.Some("monorepo")
  assert conf.single_root == option.None
}

pub fn parse_sarif_output_test() {
  let assert Ok(flags) =
    config.parse_flags(["--format", "sarif", "--sarif-output", "out.sarif"])

  assert flags.sarif_output == option.Some("out.sarif")
  assert flags.format == option.Some(config.SARIF)
}

pub fn merge_root_and_workspace_error_test() {
  let assert Error(msg) =
    config.merge_flags_and_config(
      config.Flags(
        ..empty_flags,
        single_root: option.Some("a"),
        workspace_root: option.Some("b"),
      ),
      empty_conf(),
    )

  assert msg == "cannot set --root and --workspace"
}

pub fn default_workspace_max_depth_test() {
  assert empty_conf().workspace_max_depth == 3
}

pub fn validate_workspace_formats_empty_test() {
  assert config.validate_workspace_formats([]) == Ok(Nil)
}

pub fn validate_workspace_formats_single_test() {
  assert config.validate_workspace_formats([#(config.JSON, "only")]) == Ok(Nil)
}

pub fn validate_workspace_formats_three_matching_test() {
  assert config.validate_workspace_formats([
      #(config.Detailed, "a"),
      #(config.Detailed, "b"),
      #(config.Detailed, "c"),
    ])
    == Ok(Nil)
}

pub fn validate_workspace_formats_detailed_mismatch_test() {
  let assert Error(msg) =
    config.validate_workspace_formats([
      #(config.Detailed, "a"),
      #(config.Minimal, "b"),
    ])

  assert msg
    == "workspace projects have mismatched output formats; set format consistently or use --format"
}

pub fn normalize_workspace_argv_noop_test() {
  assert config.normalize_workspace_argv(["--root", "backend"])
    == ["--root", "backend"]
  assert config.normalize_workspace_argv([]) == []
}

pub fn parse_flags_workspace_default_path_test() {
  let assert Ok(flags) = config.parse_flags(["--workspace"])

  assert flags.workspace_root == option.Some(".")
  assert flags.single_root == option.None
}

pub fn parse_flags_workspace_with_path_test() {
  let assert Ok(flags) = config.parse_flags(["--workspace", "monorepo"])

  assert flags.workspace_root == option.Some("monorepo")
}

pub fn spin_up_workspace_default_path_test() {
  let assert Ok(conf) =
    empty_conf()
    |> config.spin_up(["--workspace"])

  assert conf.workspace_root == option.Some(".")
  assert conf.single_root == option.None
}
