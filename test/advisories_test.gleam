import go_over/advisories/advisories.{check_for_advisories, read}
import go_over/packages.{read_manifest}
import go_over_test
import simplifile

pub fn check_for_advisories_test() {
  let assert [#(pkg, [adv1, adv2])] =
    read_manifest("test/testdata/manifest/known_vulnerable.toml")
    |> check_for_advisories(False, False, True)

  assert pkg.name == "phoenix"
  assert adv1.name == "phoenix"
  assert adv2.name == "phoenix"
}

pub fn read_adv_test() {
  let assert Ok(body) = simplifile.read("test/testdata/advisories/blank.yaml")
  assert Error(Nil) == read(body)

  let assert Ok(body) =
    simplifile.read("test/testdata/advisories/missing_id.yaml")
  assert Error(Nil) == read(body)

  let assert Ok(body) =
    simplifile.read("test/testdata/advisories/missing_package.yaml")
  assert Error(Nil) == read(body)

  let assert Ok(body) =
    simplifile.read("test/testdata/advisories/missing_title.yaml")
  assert Error(Nil) == read(body)

  let assert Ok(body) =
    simplifile.read("test/testdata/advisories/missing_severity.yaml")
  assert Error(Nil) == read(body)

  let assert Ok(body) =
    simplifile.read("test/testdata/advisories/missing_versions.yaml")
  assert Error(Nil) == read(body)

  let assert Ok(body) =
    simplifile.read("test/testdata/advisories/non_list_versions.yaml")
  assert Error(Nil) == read(body)

  let assert Ok(body) =
    simplifile.read("test/testdata/advisories/not-even-yaml.txt")
  assert Error(Nil) == read(body)

  let assert Ok(body) =
    simplifile.read("test/testdata/advisories/not_string_id.yaml")
  assert Error(Nil) == read(body)

  let assert Ok(body) =
    simplifile.read("test/testdata/advisories/not_string_package.yaml")
  assert Error(Nil) == read(body)

  let assert Ok(body) =
    simplifile.read("test/testdata/advisories/not_string_severity.yaml")
  assert Error(Nil) == read(body)

  let assert Ok(body) =
    simplifile.read("test/testdata/advisories/not_string_title.yaml")
  assert Error(Nil) == read(body)

  let assert Ok(body) =
    simplifile.read("test/testdata/advisories/not_string_versions.yaml")
  assert Error(Nil) == read(body)

  let assert Ok(body) = simplifile.read("test/testdata/advisories/all.yaml")
  let assert Ok(parsed) = read(body)

  go_over_test.birdie_snap_with_input(
    parsed,
    body,
    "advisories_test@test/testdata/advisories/all.yaml",
  )
}
