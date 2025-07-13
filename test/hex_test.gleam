import go_over/hex/hex
import go_over_test
import simplifile

fn parse(path: String) {
  let assert Ok(input) = simplifile.read(path)

  input
  |> hex.decode_latest_stable_version_and_licenses
  |> go_over_test.birdie_snap_with_input(
    input,
    "decode_latest_stable_version_and_licenses@" <> path,
  )
}

pub fn decode_latest_stable_version_and_licenses_test() {
  let assert Ok(_) = parse("test/testdata/hex/empty_licenses.json")
  let assert Ok(_) = parse("test/testdata/hex/full.json")
  let assert Ok(_) = parse("test/testdata/hex/multi_license.json")
  let assert Error(_) = parse("test/testdata/hex/no_license.json")
  let assert Error(_) = parse("test/testdata/hex/no_meta.json")
  let assert Error(_) = parse("test/testdata/hex/no_version.json")
  let assert Ok(_) = parse("test/testdata/hex/version_null.json")
}
