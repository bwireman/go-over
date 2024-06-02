import gleam/option.{None}
import gleamsver.{parse}
import gleeunit/should
import go_over/advisories/advisories.{Advisory}
import go_over/config.{
  filter_advisory_ids, filter_packages, filter_severity, read_config,
}
import go_over/packages.{Package}
import go_over/warning.{Warning}

pub fn read_config_test() {
  let empty = read_config("test/testdata/gleam/empty.toml")
  should.be_true(empty.cache)
  should.equal(empty.ignore_packages, [])
  should.equal(empty.ignore_severity, [])
  should.equal(empty.ignore_ids, [])

  let empty = read_config("test/testdata/gleam/basic.toml")
  should.be_false(empty.cache)
  should.equal(empty.ignore_packages, ["a"])
  should.equal(empty.ignore_severity, ["b"])
  should.equal(empty.ignore_ids, ["c"])

  let empty = read_config("test/testdata/gleam/partial.toml")
  should.be_true(empty.cache)
  should.equal(empty.ignore_packages, ["a", "b", "c"])
  should.equal(empty.ignore_severity, [])
  should.equal(empty.ignore_ids, [])
}

pub fn filter_packages_test() {
  let full = read_config("test/testdata/gleam/full.toml")
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
  let full = read_config("test/testdata/gleam/full.toml")
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
  let full = read_config("test/testdata/gleam/full.toml")
  let a = Warning(None, "", "", "", warning.Vulnerable, "a", warning.Direct)
  let b = Warning(None, "", "", "", warning.Vulnerable, "b", warning.Direct)
  let c = Warning(None, "", "", "", warning.Vulnerable, "c", warning.Direct)
  let aa = Warning(None, "", "", "", warning.Vulnerable, "A", warning.Direct)
  let bb = Warning(None, "", "", "", warning.Vulnerable, "B", warning.Direct)
  let cc = Warning(None, "", "", "", warning.Vulnerable, "C", warning.Direct)

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
