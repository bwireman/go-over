import gleeunit/should
import go_over/advisories/advisories.{check_for_advisories}
import go_over/packages.{read_manifest}

pub fn check_for_advisories_test() {
  let assert [#(pkg, [adv1, adv2])] =
    read_manifest("test/testdata/manifest/known_vulnerable.toml")
    |> check_for_advisories(False)

  should.equal(pkg.name, "phoenix")
  should.equal(adv1.name, "phoenix")
  should.equal(adv2.name, "phoenix")
}
