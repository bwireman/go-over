import gleam/option.{None}
import gleamsver.{parse}
import go_over/advisories/advisories.{Advisory}
import go_over/config.{
  filter_advisory_ids, filter_dev_dependencies, filter_packages, filter_severity,
  read_config,
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
  assert empty.puller == puller.default()
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
  assert conf.puller == puller.default()

  let conf = test_spin_up("force", ["--force"])
  assert conf.force

  let conf = test_spin_up("outdated", ["--outdated"])
  assert conf.outdated

  let conf = test_spin_up("ignore_indirect", ["--ignore-indirect"])
  assert conf.ignore_indirect

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

pub fn merge_flags_and_config_test() {
  let empty_conf = empty_conf()
  let empty_flags =
    config.Flags(
      force: False,
      outdated: False,
      ignore_indirect: False,
      verbose: False,
      format: option.None,
      global: False,
      puller: option.None,
    )

  assert config.merge_flags_and_config(empty_flags, empty_conf) == empty_conf

  // FLAG
  assert config.merge_flags_and_config(
      config.Flags(..empty_flags, outdated: True),
      empty_conf,
    )
    == config.Config(..empty_conf, outdated: True)

  assert config.merge_flags_and_config(
      config.Flags(..empty_flags, ignore_indirect: True),
      empty_conf,
    )
    == config.Config(..empty_conf, ignore_indirect: True)

  assert config.merge_flags_and_config(
      config.Flags(..empty_flags, force: True),
      empty_conf,
    )
    == config.Config(..empty_conf, force: True)

  assert config.merge_flags_and_config(
      config.Flags(..empty_flags, global: True),
      empty_conf,
    )
    == config.Config(..empty_conf, global: True)

  assert config.merge_flags_and_config(
      config.Flags(..empty_flags, format: option.Some(config.JSON)),
      empty_conf,
    )
    == config.Config(..empty_conf, format: config.JSON)

  assert config.merge_flags_and_config(
      config.Flags(..empty_flags, format: option.Some(config.Detailed)),
      empty_conf,
    )
    == config.Config(..empty_conf, format: config.Detailed)

  // CONF
  assert config.merge_flags_and_config(
      empty_flags,
      config.Config(..empty_conf, outdated: True),
    )
    == config.Config(..empty_conf, outdated: True)

  assert config.merge_flags_and_config(
      empty_flags,
      config.Config(..empty_conf, ignore_indirect: True),
    )
    == config.Config(..empty_conf, ignore_indirect: True)

  assert config.merge_flags_and_config(
      empty_flags,
      config.Config(..empty_conf, force: True),
    )
    == config.Config(..empty_conf, force: True)

  assert config.merge_flags_and_config(
      empty_flags,
      config.Config(..empty_conf, global: True),
    )
    == config.Config(..empty_conf, global: True)

  assert config.merge_flags_and_config(
      empty_flags,
      config.Config(..empty_conf, format: config.JSON),
    )
    == config.Config(..empty_conf, format: config.JSON)

  assert config.merge_flags_and_config(
      empty_flags,
      config.Config(..empty_conf, format: config.Detailed),
    )
    == config.Config(..empty_conf, format: config.Detailed)

  // BOTH
  assert config.merge_flags_and_config(
      config.Flags(..empty_flags, format: option.Some(config.JSON)),
      config.Config(..empty_conf, format: config.Minimal),
    )
    == config.Config(..empty_conf, format: config.JSON)

  assert config.merge_flags_and_config(
      config.Flags(..empty_flags, format: option.Some(config.Detailed)),
      config.Config(..empty_conf, format: config.Minimal),
    )
    == config.Config(..empty_conf, format: config.Detailed)

  assert config.merge_flags_and_config(
      config.Flags(..empty_flags, global: False),
      config.Config(..empty_conf, global: False),
    )
    == config.Config(..empty_conf, global: False)

  assert config.merge_flags_and_config(
      config.Flags(..empty_flags, global: True),
      config.Config(..empty_conf, global: True),
    )
    == config.Config(..empty_conf, global: True)

  assert config.merge_flags_and_config(
      config.Flags(..empty_flags, global: True),
      config.Config(..empty_conf, global: False),
    )
    == config.Config(..empty_conf, global: True)

  assert config.merge_flags_and_config(
      config.Flags(..empty_flags, global: False),
      config.Config(..empty_conf, global: True),
    )
    == config.Config(..empty_conf, global: True)
}
