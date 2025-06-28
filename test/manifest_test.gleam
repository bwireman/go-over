import gleeunit/should
import go_over/packages.{read_manifest}
import go_over_test

fn test_read_manifest(path: String) {
  path
  |> read_manifest()
  |> go_over_test.birdie_snap("manifest_test@" <> path)
}

pub fn read_manifest_test() {
  let assert [] = test_read_manifest("test/testdata/manifest/empty.toml")

  let assert [a] = test_read_manifest("test/testdata/manifest/a.toml")
  should.equal(a.name, "a")
  should.equal(a.version_raw, "0.8.1")
  should.be_true(a.direct)
  should.equal(a.source, packages.PackageSourceHex)

  let assert [_, b] = test_read_manifest("test/testdata/manifest/b.toml")
  should.equal(b.name, "b")
  should.equal(b.version_raw, "2.2.123")
  should.be_false(b.direct)
  should.equal(b.source, packages.PackageSourceHex)

  let assert [_, dos] = test_read_manifest("test/testdata/manifest/dos.toml")
  should.equal(dos.name, "dos")
  should.equal(dos.version_raw, "2.2.123")
  should.be_false(dos.direct)
  should.equal(dos.source, packages.PackageSourceHex)

  let assert [_, _, git] = test_read_manifest("test/testdata/manifest/git.toml")
  should.equal(git.name, "c")
  should.equal(git.version_raw, "0.1.0")
  should.be_false(git.direct)
  should.equal(git.source, packages.PackageSourceGit)

  let assert [_, _, local] =
    test_read_manifest("test/testdata/manifest/local.toml")
  should.equal(local.name, "c")
  should.equal(local.version_raw, "0.1.0")
  should.be_false(local.direct)
  should.equal(local.source, packages.PackageSourceLocal)
}
