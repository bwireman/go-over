import gleeunit/should
import go_over/packages.{read_manifest}

pub fn read_manifest_test() {
  let assert [] = read_manifest("test/testdata/empty.toml")

  let assert [a] = read_manifest("test/testdata/a.toml")
  should.equal(a.name, "a")
  should.equal(a.version_raw, "0.8.1")
  should.be_true(a.direct)

  let assert [_, b] = read_manifest("test/testdata/b.toml")
  should.equal(b.name, "b")
  should.equal(b.version_raw, "2.2.123")
  should.be_false(b.direct)
}
