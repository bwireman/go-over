import birdie
import gleeunit/should
import go_over/packages.{read_manifest}
import pprint

fn test_read_manifest(p: String) {
  let body = read_manifest(p)

  body
  |> pprint.format()
  |> birdie.snap("Manifest test: " <> p)

  body
}

pub fn read_manifest_test() {
  let assert [] = test_read_manifest("test/testdata/manifest/empty.toml")

  let assert [a] = test_read_manifest("test/testdata/manifest/a.toml")
  should.equal(a.name, "a")
  should.equal(a.version_raw, "0.8.1")
  should.be_true(a.direct)

  let assert [_, b] = test_read_manifest("test/testdata/manifest/b.toml")
  should.equal(b.name, "b")
  should.equal(b.version_raw, "2.2.123")
  should.be_false(b.direct)
}
