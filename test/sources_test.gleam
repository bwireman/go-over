import gleam/option.{None, Some}
import gleamsver
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
  assert sources.get_retired_warnings(
      pkgs,
      Config(..conf, puller: Mock("test/testdata/hex/retired/retired.json")),
    )
    == [
      Warning(
        None,
        "name",
        Some("1.1.1"),
        "security: example",
        WarningReasonRetired,
        warning.SeverityPackageRetiredSecurity,
        IndirectDep,
      ),
    ]

  assert sources.get_retired_warnings(
      pkgs,
      Config(..conf, puller: Mock("test/testdata/hex/retired/not_retired.json")),
    )
    == []
}

pub fn get_rejected_license_test() {
  assert sources.get_hex_warnings(
      pkgs,
      Config(
        ..conf,
        puller: Mock("test/testdata/hex/rejected_licenses/bad_license.json"),
      ),
    )
    == [
      Warning(
        None,
        "name",
        None,
        "Rejected License found: closed-source",
        WarningReasonRejectedLicense("closed-source"),
        warning.SeverityRejectedLicense,
        IndirectDep,
      ),
    ]

  assert sources.get_hex_warnings(
      pkgs,
      Config(
        ..conf,
        puller: Mock("test/testdata/hex/rejected_licenses/good_license.json"),
      ),
    )
    == []
}

pub fn get_outdated_test() {
  assert sources.get_hex_warnings(
      pkgs,
      Config(..conf, puller: Mock("test/testdata/hex/outdated/outdated.json")),
    )
    == [
      Warning(
        None,
        "name",
        Some("1.1.1"),
        "New Version: '1.2.3' exists",
        WarningReasonOutdated,
        warning.SeverityPackageOutdated,
        IndirectDep,
      ),
    ]

  assert sources.get_hex_warnings(
      pkgs,
      Config(..conf, puller: Mock("test/testdata/hex/outdated/up_to_date.json")),
    )
    == []
}
