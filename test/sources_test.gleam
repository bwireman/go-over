import gleam/option.{None, Some}
import gleamsver
import gleeunit/should
import go_over/config.{Config, Minimal}
import go_over/hex/puller.{Mock}
import go_over/packages
import go_over/sources
import go_over/warning.{
  IndirectDep, Warning, WarningReasonOutdated, WarningReasonRejectedLicense,
  WarningReasonRetired,
}

const conf = Config(
  dev_deps: [],
  outdated: True,
  ignore_indirect: False,
  force: True,
  format: Minimal,
  verbose: True,
  global: False,
  puller: puller.CURL,
  allowed_licenses: ["MIT"],
  ignore_packages: [],
  ignore_severity: [],
  ignore_ids: [],
  ignore_dev_dependencies: False,
)

const pkgs = [
  packages.Package(
    "name",
    gleamsver.SemVer(1, 1, 1, "", ""),
    "1.1.1",
    False,
    packages.PackageSourceHex,
  ),
]

pub fn get_retired_warnings_test() {
  sources.get_retired_warnings(
    pkgs,
    Config(..conf, puller: Mock("test/testdata/hex/retired/retired.json")),
  )
  |> should.equal([
    Warning(
      None,
      "name",
      Some("1.1.1"),
      "security: example",
      WarningReasonRetired,
      warning.SeverityPackageRetiredSecurity,
      IndirectDep,
    ),
  ])

  sources.get_retired_warnings(
    pkgs,
    Config(..conf, puller: Mock("test/testdata/hex/retired/not_retired.json")),
  )
  |> should.equal([])
}

pub fn get_rejected_license_test() {
  sources.get_hex_warnings(
    pkgs,
    Config(
      ..conf,
      puller: Mock("test/testdata/hex/rejected_licenses/bad_license.json"),
    ),
  )
  |> should.equal([
    Warning(
      None,
      "name",
      None,
      "Rejected License found: closed-source",
      WarningReasonRejectedLicense("closed-source"),
      warning.SeverityRejectedLicense,
      IndirectDep,
    ),
  ])

  sources.get_hex_warnings(
    pkgs,
    Config(
      ..conf,
      puller: Mock("test/testdata/hex/rejected_licenses/good_license.json"),
    ),
  )
  |> should.equal([])
}

pub fn get_outdated_test() {
  sources.get_hex_warnings(
    pkgs,
    Config(..conf, puller: Mock("test/testdata/hex/outdated/outdated.json")),
  )
  |> should.equal([
    Warning(
      None,
      "name",
      Some("1.1.1"),
      "New Version: '1.2.3' exists",
      WarningReasonOutdated,
      warning.SeverityPackageOutdated,
      IndirectDep,
    ),
  ])

  sources.get_hex_warnings(
    pkgs,
    Config(..conf, puller: Mock("test/testdata/hex/outdated/up_to_date.json")),
  )
  |> should.equal([])
}
