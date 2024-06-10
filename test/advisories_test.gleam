import gleeunit/should
import go_over/advisories/advisories.{check_for_advisories, read}
import go_over/packages.{read_manifest}
import simplifile

pub fn check_for_advisories_test() {
  let assert [#(pkg, [adv1, adv2])] =
    read_manifest("test/testdata/manifest/known_vulnerable.toml")
    |> check_for_advisories(False)

  should.equal(pkg.name, "phoenix")
  should.equal(adv1.name, "phoenix")
  should.equal(adv2.name, "phoenix")
}

pub fn read_adv_test() {
  let assert Ok(body) = simplifile.read("test/testdata/advisories/blank.yaml")
  read(body)
  |> should.be_error

  let assert Ok(body) =
    simplifile.read("test/testdata/advisories/missing_id.yaml")
  read(body)
  |> should.be_error

  let assert Ok(body) =
    simplifile.read("test/testdata/advisories/missing_package.yaml")
  read(body)
  |> should.be_error

  let assert Ok(body) =
    simplifile.read("test/testdata/advisories/missing_title.yaml")
  read(body)
  |> should.be_error

  let assert Ok(body) =
    simplifile.read("test/testdata/advisories/missing_severity.yaml")
  read(body)
  |> should.be_error

  let assert Ok(body) =
    simplifile.read("test/testdata/advisories/missing_versions.yaml")
  read(body)
  |> should.be_error

  let assert Ok(body) =
    simplifile.read("test/testdata/advisories/non_list_versions.yaml")
  read(body)
  |> should.be_error

  let assert Ok(body) =
    simplifile.read("test/testdata/advisories/not-even-yaml.txt")
  read(body)
  |> should.be_error

  let assert Ok(body) = simplifile.read("test/testdata/advisories/all.yaml")
  read(body)
  |> should.be_ok
}
