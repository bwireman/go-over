import gleeunit/should
import go_over/hex/hex
import go_over_test
import simplifile

fn parse(path: String) {
  let input =
    simplifile.read(path)
    |> should.be_ok()

  input
  |> hex.decode_latest_stable_version_and_licenses
  |> go_over_test.birdie_snap_with_input(
    input,
    "decode_latest_stable_version_and_licenses@" <> path,
  )
}

pub fn decode_latest_stable_version_and_licenses_test() {
  parse("test/testdata/hex/empty_licenses.json")
  |> should.be_ok()
  parse("test/testdata/hex/full.json")
  |> should.be_ok()
  parse("test/testdata/hex/multi_license.json")
  |> should.be_ok()
  parse("test/testdata/hex/no_license.json")
  |> should.be_error()
  parse("test/testdata/hex/no_meta.json")
  |> should.be_error()
  parse("test/testdata/hex/no_version.json")
  |> should.be_error()
  parse("test/testdata/hex/version_null.json")
  |> should.be_ok()
}
