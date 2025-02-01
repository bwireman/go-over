import gleeunit/should
import go_over/packages.{read_manifest}
import go_over_test

fn test_read_manifest(p: String) {
  read_manifest(p)
  |> go_over_test.birdie_snap("Manifest test: " <> p)
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

  let assert [_, dos] = test_read_manifest("test/testdata/manifest/dos.toml")
  should.equal(dos.name, "dos")
  should.equal(dos.version_raw, "2.2.123")
  should.be_false(dos.direct)
}
