import gleam/option.{None}
import gleamsver.{parse}
import gleeunit/should
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
  empty_conf()
  |> config.spin_up(argv)
  |> should.be_ok()
  |> go_over_test.birdie_snap_with_input(argv, "spin_up_test@" <> name)
}

pub fn read_config_test() {
  let empty = test_read_config("test/testdata/gleam/empty.toml")
  should.be_false(empty.force)
  should.equal(empty.format, config.Minimal)
  should.equal(empty.puller, puller.CURL)
  should.equal(empty.ignore_packages, [])
  should.equal(empty.ignore_severity, [])
  should.equal(empty.ignore_ids, [])
  should.be_false(empty.ignore_indirect)

  let basic = test_read_config("test/testdata/gleam/basic.toml")
  should.be_true(basic.force)
  should.equal(basic.puller, puller.HTTPIE)
  should.equal(basic.format, config.Detailed)
  should.equal(basic.ignore_packages, ["a"])
  should.equal(basic.ignore_severity, ["b"])
  should.equal(basic.ignore_ids, ["c"])
  should.be_true(basic.ignore_indirect)

  let partial = test_read_config("test/testdata/gleam/partial.toml")
  should.be_false(partial.force)
  should.equal(partial.puller, puller.WGET)
  should.equal(partial.format, config.Minimal)
  should.equal(partial.ignore_packages, ["a", "b", "c"])
  should.equal(partial.ignore_severity, [])
  should.equal(partial.ignore_ids, [])
  should.be_false(partial.ignore_indirect)

  let indirect_new = test_read_config("test/testdata/gleam/indirect_new.toml")
  should.be_true(indirect_new.ignore_indirect)
}

pub fn filter_dev_deps_test() {
  let full = test_read_config("test/testdata/gleam/full.toml")
  let assert Ok(v) = parse("1.1.1")
  let a = Package("a", v, "", False)
  let b = Package("b", v, "", False)
  let c = Package("c", v, "", False)

  should.equal(filter_dev_dependencies(full, []), [])
  should.equal(filter_dev_dependencies(full, [a]), [a])
  should.equal(filter_dev_dependencies(full, [a, b]), [a, b])
  should.equal(filter_dev_dependencies(full, [b, c]), [b])
  should.equal(filter_dev_dependencies(full, [a, b, c]), [a, b])
}

pub fn filter_packages_test() {
  let full = test_read_config("test/testdata/gleam/full.toml")
  let assert Ok(v) = parse("1.1.1")
  let a = Package("a", v, "", False)
  let b = Package("b", v, "", False)
  let c = Package("c", v, "", False)

  should.equal(filter_packages(full, []), [])
  should.equal(filter_packages(full, [a]), [])
  should.equal(filter_packages(full, [a, b]), [])
  should.equal(filter_packages(full, [b, c]), [c])
  should.equal(filter_packages(full, [a, b, c]), [c])
}

pub fn filter_advisory_ids_test() {
  let full = test_read_config("test/testdata/gleam/full.toml")
  let a = Advisory("a", "", "", [], "")
  let b = Advisory("b", "", "", [], "")
  let c = Advisory("c", "", "", [], "")

  should.equal(filter_advisory_ids(full, []), [])
  should.equal(filter_advisory_ids(full, [a]), [])
  should.equal(filter_advisory_ids(full, [a, b]), [])
  should.equal(filter_advisory_ids(full, [b, c]), [c])
  should.equal(filter_advisory_ids(full, [a, b, c]), [c])
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
      warning.Direct,
    )
  let b =
    Warning(
      None,
      "",
      None,
      "",
      warning.WarningReasonVulnerable,
      warning.SeverityHigh,
      warning.Direct,
    )
  let c =
    Warning(
      None,
      "",
      None,
      "",
      warning.WarningReasonVulnerable,
      warning.SeverityModerate,
      warning.Direct,
    )
  let aa =
    Warning(
      None,
      "",
      None,
      "",
      warning.WarningReasonVulnerable,
      warning.SeverityCritical,
      warning.Direct,
    )
  let bb =
    Warning(
      None,
      "",
      None,
      "",
      warning.WarningReasonVulnerable,
      warning.SeverityHigh,
      warning.Direct,
    )
  let cc =
    Warning(
      None,
      "",
      None,
      "",
      warning.WarningReasonVulnerable,
      warning.SeverityPackageRetiredSecurity,
      warning.Direct,
    )

  should.equal(filter_severity(full, []), [])
  should.equal(filter_severity(full, [a]), [])
  should.equal(filter_severity(full, [a, b]), [])
  should.equal(filter_severity(full, [b, c]), [c])
  should.equal(filter_severity(full, [a, b, c]), [c])

  should.equal(filter_severity(full, []), [])
  should.equal(filter_severity(full, [aa]), [])
  should.equal(filter_severity(full, [aa, bb]), [])
  should.equal(filter_severity(full, [bb, cc]), [cc])
  should.equal(filter_severity(full, [aa, bb, cc]), [cc])
}

pub fn spin_up_test() {
  let conf = test_spin_up("empty", [])
  should.be_false(conf.force)
  should.be_false(conf.outdated)
  should.be_false(conf.ignore_indirect)
  should.be_false(conf.fake)
  should.be_false(conf.verbose)
  should.equal(conf.format, config.Minimal)
  should.equal(conf.puller, puller.CURL)

  let conf = test_spin_up("force", ["--force"])
  should.be_true(conf.force)

  let conf = test_spin_up("outdated", ["--outdated"])
  should.be_true(conf.outdated)

  let conf = test_spin_up("ignore_indirect", ["--ignore-indirect"])
  should.be_true(conf.ignore_indirect)

  let conf = test_spin_up("fake", ["--fake"])
  should.be_true(conf.fake)

  let conf = test_spin_up("verbose", ["--verbose"])
  should.be_true(conf.verbose)
}

pub fn spin_up_format_test() {
  let conf = test_spin_up("format=minimal", ["--format", "minimal"])
  should.equal(conf.format, config.Minimal)

  let conf = test_spin_up("format=json", ["--format", "json"])
  should.equal(conf.format, config.JSON)

  let conf = test_spin_up("format=detailed", ["--format", "detailed"])
  should.equal(conf.format, config.Detailed)
}

pub fn merge_flags_and_config_test() {
  let empty_conf = empty_conf()
  let empty_flags =
    config.Flags(
      force: False,
      fake: False,
      outdated: False,
      ignore_indirect: False,
      verbose: False,
      format: option.None,
      global: False,
    )

  config.merge_flags_and_config(empty_flags, empty_conf)
  |> should.equal(empty_conf)

  // FLAG
  config.merge_flags_and_config(
    config.Flags(..empty_flags, outdated: True),
    empty_conf,
  )
  |> should.equal(config.Config(..empty_conf, outdated: True))

  config.merge_flags_and_config(
    config.Flags(..empty_flags, ignore_indirect: True),
    empty_conf,
  )
  |> should.equal(config.Config(..empty_conf, ignore_indirect: True))

  config.merge_flags_and_config(
    config.Flags(..empty_flags, force: True),
    empty_conf,
  )
  |> should.equal(config.Config(..empty_conf, force: True))

  config.merge_flags_and_config(
    config.Flags(..empty_flags, global: True),
    empty_conf,
  )
  |> should.equal(config.Config(..empty_conf, global: True))

  config.merge_flags_and_config(
    config.Flags(..empty_flags, format: option.Some(config.JSON)),
    empty_conf,
  )
  |> should.equal(config.Config(..empty_conf, format: config.JSON))

  config.merge_flags_and_config(
    config.Flags(..empty_flags, format: option.Some(config.Detailed)),
    empty_conf,
  )
  |> should.equal(config.Config(..empty_conf, format: config.Detailed))

  // CONF
  config.merge_flags_and_config(
    empty_flags,
    config.Config(..empty_conf, outdated: True),
  )
  |> should.equal(config.Config(..empty_conf, outdated: True))

  config.merge_flags_and_config(
    empty_flags,
    config.Config(..empty_conf, ignore_indirect: True),
  )
  |> should.equal(config.Config(..empty_conf, ignore_indirect: True))

  config.merge_flags_and_config(
    empty_flags,
    config.Config(..empty_conf, force: True),
  )
  |> should.equal(config.Config(..empty_conf, force: True))

  config.merge_flags_and_config(
    empty_flags,
    config.Config(..empty_conf, global: True),
  )
  |> should.equal(config.Config(..empty_conf, global: True))

  config.merge_flags_and_config(
    empty_flags,
    config.Config(..empty_conf, format: config.JSON),
  )
  |> should.equal(config.Config(..empty_conf, format: config.JSON))

  config.merge_flags_and_config(
    empty_flags,
    config.Config(..empty_conf, format: config.Detailed),
  )
  |> should.equal(config.Config(..empty_conf, format: config.Detailed))

  // BOTH
  config.merge_flags_and_config(
    config.Flags(..empty_flags, format: option.Some(config.JSON)),
    config.Config(..empty_conf, format: config.Minimal),
  )
  |> should.equal(config.Config(..empty_conf, format: config.JSON))

  config.merge_flags_and_config(
    config.Flags(..empty_flags, format: option.Some(config.Detailed)),
    config.Config(..empty_conf, format: config.Minimal),
  )
  |> should.equal(config.Config(..empty_conf, format: config.Detailed))

  config.merge_flags_and_config(
    config.Flags(..empty_flags, global: False),
    config.Config(..empty_conf, global: False),
  )
  |> should.equal(config.Config(..empty_conf, global: False))

  config.merge_flags_and_config(
    config.Flags(..empty_flags, global: True),
    config.Config(..empty_conf, global: True),
  )
  |> should.equal(config.Config(..empty_conf, global: True))

  config.merge_flags_and_config(
    config.Flags(..empty_flags, global: True),
    config.Config(..empty_conf, global: False),
  )
  |> should.equal(config.Config(..empty_conf, global: True))

  config.merge_flags_and_config(
    config.Flags(..empty_flags, global: False),
    config.Config(..empty_conf, global: True),
  )
  |> should.equal(config.Config(..empty_conf, global: True))
}
