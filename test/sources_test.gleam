import gleam/option.{None, Some}
import gleamsver
import gleeunit/should
import go_over/config.{Config, Minimal}
import go_over/hex/puller
import go_over/packages
import go_over/sources
import go_over/warning.{Indirect, Outdated, RejectedLicense, Retired, Warning}

const conf = Config(
  dev_deps: [],
  outdated: True,
  ignore_indirect: False,
  force: True,
  fake: False,
  format: Minimal,
  verbose: True,
  global: False,
  allowed_licenses: ["MIT"],
  ignore_packages: [],
  ignore_severity: [],
  ignore_ids: [],
  ignore_dev_dependencies: False,
)

const pkgs = [
  packages.Package("name", gleamsver.SemVer(1, 1, 1, "", ""), "1.1.1", False),
]

pub fn get_retired_warnings_test() {
  puller.Mock("test/testdata/hex/retired/retired.json")
  |> sources.get_retired_warnings(pkgs, conf)
  |> should.equal([
    Warning(
      None,
      "name",
      Some("1.1.1"),
      "security: example",
      Retired,
      "package-retired (security)",
      Indirect,
    ),
  ])

  puller.Mock("test/testdata/hex/retired/not_retired.json")
  |> sources.get_retired_warnings(pkgs, conf)
  |> should.equal([])
}

pub fn get_rejected_license_test() {
  puller.Mock("test/testdata/hex/rejected_licenses/bad_license.json")
  |> sources.get_hex_warnings(pkgs, conf)
  |> should.equal([
    Warning(
      None,
      "name",
      None,
      "Rejected License found: closed-source",
      RejectedLicense("closed-source"),
      "rejected-license",
      Indirect,
    ),
  ])

  puller.Mock("test/testdata/hex/rejected_licenses/good_license.json")
  |> sources.get_hex_warnings(pkgs, conf)
  |> should.equal([])
}

pub fn get_outdated_test() {
  puller.Mock("test/testdata/hex/outdated/outdated.json")
  |> sources.get_hex_warnings(pkgs, conf)
  |> should.equal([
    Warning(
      None,
      "name",
      Some("1.1.1"),
      "New Version: '1.2.3' exists",
      Outdated,
      "package-outdated",
      Indirect,
    ),
  ])

  puller.Mock("test/testdata/hex/outdated/up_to_date.json")
  |> sources.get_hex_warnings(pkgs, conf)
  |> should.equal([])
}
