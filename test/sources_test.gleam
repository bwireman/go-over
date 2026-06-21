import gleam/option.{None, Some}
import gleamsver
import go_over/advisories/advisories.{Advisory}
import go_over/config.{Config, Minimal}
import go_over/hex/puller.{Mock}
import go_over/packages
import go_over/sources
import go_over/warning.{
  IndirectDep, SeverityLow, Warning, WarningReasonRejectedLicense,
  WarningReasonRetired, WarningReasonVulnerable,
}

const conf = Config(
  dev_deps: [],
  outdated: False,
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
  workspace_max_depth: 3,
  single_root: option.None,
  workspace_root: option.None,
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
  let #(warnings, _licenses) =
    sources.get_hex_warnings(
      pkgs,
      Config(
        ..conf,
        puller: Mock("test/testdata/hex/rejected_licenses/bad_license.json"),
      ),
    )

  assert warnings
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

  let #(warnings, _licenses) =
    sources.get_hex_warnings(
      pkgs,
      Config(
        ..conf,
        puller: Mock("test/testdata/hex/rejected_licenses/good_license.json"),
      ),
    )

  assert warnings == []
}

pub fn get_vulnerable_warnings_test() {
  let mirego_advisories = [
    Advisory(
      id: "GHSA-test",
      name: "name",
      severity: "low",
      vulnerable_version_ranges: ["<= 1.1.1"],
      description: "test advisory",
    ),
  ]

  let assert [warning] =
    sources.get_vulnerable_warnings(pkgs, conf, mirego_advisories)

  assert warning.advisory_id == Some("GHSA-test")
  assert warning.reason == "test advisory"
  assert warning.severity == SeverityLow
  assert warning.warning_reason_code == WarningReasonVulnerable
}
