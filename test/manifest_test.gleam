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
  assert a.name == "a"
  assert a.version_raw == "0.8.1"
  assert a.direct
  assert a.source == packages.PackageSourceHex

  let assert [_, b] = test_read_manifest("test/testdata/manifest/b.toml")
  assert b.name == "b"
  assert b.version_raw == "2.2.123"
  assert !b.direct
  assert b.source == packages.PackageSourceHex

  let assert [_, dos] = test_read_manifest("test/testdata/manifest/dos.toml")
  assert dos.name == "dos"
  assert dos.version_raw == "2.2.123"
  assert !dos.direct
  assert dos.source == packages.PackageSourceHex

  let assert [_, _, git] = test_read_manifest("test/testdata/manifest/git.toml")
  assert git.name == "c"
  assert git.version_raw == "0.1.0"
  assert !git.direct
  assert git.source == packages.PackageSourceGit

  let assert [_, _, local] =
    test_read_manifest("test/testdata/manifest/local.toml")
  assert local.name == "c"
  assert local.version_raw == "0.1.0"
  assert !local.direct
  assert local.source == packages.PackageSourceLocal
}
