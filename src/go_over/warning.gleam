import gleam/hexpm.{type ReleaseRetirement}
import gleam/json.{type Json, object, string}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import go_over/advisories/advisories.{type Advisory}
import go_over/hex/core
import go_over/packages.{type Package}
import go_over/util/print

pub type WarningReasonCode {
  WarningReasonRetired
  WarningReasonVulnerable
  WarningReasonOutdated
  WarningReasonRejectedLicense(name: String)
}

fn warning_reason_code_as_string(w: WarningReasonCode) -> String {
  case w {
    WarningReasonRetired -> "Retired"
    WarningReasonVulnerable -> "Vulnerable"
    WarningReasonOutdated -> "Outdated"
    WarningReasonRejectedLicense(name) -> "Rejected License (" <> name <> ")"
  }
}

pub type Severity {
  SeverityPackageRetiredInvalid
  SeverityPackageRetiredSecurity
  SeverityPackageRetiredDeprecated
  SeverityPackageRetiredRenamed
  SeverityPackageRetiredOtherReason(reason: String)
  SeverityPackageOutdated
  SeverityRejectedLicense
  SeverityCritical
  SeverityHigh
  SeverityLow
  SeverityModerate
  SeverityUnknown(info: String)
}

pub fn severity_as_string(s: Severity) -> String {
  case s {
    SeverityPackageRetiredInvalid -> "package-retired:invalid"
    SeverityPackageRetiredSecurity -> "package-retired:security"
    SeverityPackageRetiredDeprecated -> "package-retired:deprecated"
    SeverityPackageRetiredRenamed -> "package-retired:renamed"
    SeverityPackageRetiredOtherReason(reason) ->
      "package-retired:" <> string.lowercase(reason)
    SeverityPackageOutdated -> "package-outdated"
    SeverityRejectedLicense -> "rejected-license"
    SeverityCritical -> "critical"
    SeverityHigh -> "high"
    SeverityLow -> "low"
    SeverityModerate -> "moderate"
    SeverityUnknown(value) ->
      string.join(["unknown", string.lowercase(value)], "-")
  }
}

pub fn string_to_severity(s: String) -> Severity {
  case string.lowercase(s) {
    "package-retired:invalid" -> SeverityPackageRetiredInvalid
    "package-retired:security" -> SeverityPackageRetiredSecurity
    "package-retired:deprecated" -> SeverityPackageRetiredDeprecated
    "package-retired:renamed" -> SeverityPackageRetiredRenamed
    "package-retired:" <> v -> SeverityPackageRetiredOtherReason(v)
    "package-outdated" -> SeverityPackageOutdated
    "rejected-license" -> SeverityRejectedLicense
    "critical" -> SeverityCritical
    "high" -> SeverityHigh
    "low" -> SeverityLow
    "moderate" -> SeverityModerate
    "unknown-" <> v -> SeverityUnknown(v)
    v -> SeverityUnknown(v)
  }
}

pub type Dep {
  Direct
  Indirect
}

fn dep_code_as_string(d: Dep) -> String {
  case d {
    Direct -> "Direct"
    Indirect -> "Indirect"
  }
}

fn dep_code_from_bool(d: Bool) -> Dep {
  case d {
    True -> Direct
    False -> Indirect
  }
}

pub type Warning {
  Warning(
    advisory_id: Option(String),
    package: String,
    version: Option(String),
    reason: String,
    warning_reason_code: WarningReasonCode,
    severity: Severity,
    dep: Dep,
  )
}

pub fn adv_to_warning(pkg: Package, advisories: List(Advisory)) -> List(Warning) {
  list.map(advisories, fn(adv) {
    Warning(
      Some(adv.id),
      pkg.name,
      Some(pkg.version_raw),
      adv.description,
      WarningReasonVulnerable,
      string_to_severity(adv.severity),
      dep_code_from_bool(pkg.direct),
    )
  })
}

pub fn retired_to_warning(pkg: Package, ret: ReleaseRetirement) -> Warning {
  let sev = case ret.reason {
    hexpm.Deprecated -> SeverityPackageRetiredDeprecated
    hexpm.Invalid -> SeverityPackageRetiredInvalid
    hexpm.Renamed -> SeverityPackageRetiredRenamed
    hexpm.Security -> SeverityPackageRetiredSecurity
    hexpm.OtherReason ->
      SeverityPackageRetiredOtherReason(option.unwrap(ret.message, "Unknown"))
  }

  Warning(
    None,
    pkg.name,
    Some(pkg.version_raw),
    core.print_ret(ret),
    WarningReasonRetired,
    sev,
    dep_code_from_bool(pkg.direct),
  )
}

pub fn outdated_to_warning(pkg: Package, new_version: String) -> Warning {
  Warning(
    None,
    pkg.name,
    Some(pkg.version_raw),
    "New Version: '" <> new_version <> "' exists",
    WarningReasonOutdated,
    SeverityPackageOutdated,
    dep_code_from_bool(pkg.direct),
  )
}

pub fn rejected_license_to_warning(pkg: Package, license: String) -> Warning {
  Warning(
    None,
    pkg.name,
    None,
    "Rejected License found: " <> license,
    WarningReasonRejectedLicense(license),
    SeverityRejectedLicense,
    dep_code_from_bool(pkg.direct),
  )
}

pub fn format_as_string(w: Warning) -> String {
  [
    "ID: " <> option.unwrap(w.advisory_id, "null"),
    "Package: " <> w.package,
    "Version: " <> option.unwrap(w.version, "null"),
    "WarningReason: " <> warning_reason_code_as_string(w.warning_reason_code),
    "Dependency Type: " <> dep_code_as_string(w.dep),
    "Severity: " <> severity_as_string(w.severity),
    "Reason: " <> w.reason,
  ]
  |> string.join("\n")
  |> color(w, _)
}

pub fn format_as_string_minimal(w: Warning) -> String {
  case w.version {
    option.Some(version) ->
      color(
        w,
        w.package <> "-" <> version <> ": " <> severity_as_string(w.severity),
      )

    option.None -> color(w, w.package <> ": " <> severity_as_string(w.severity))
  }
}

pub fn format_as_json(w: Warning) -> Json {
  object([
    #("id", json.nullable(w.advisory_id, string)),
    #("package", string(w.package)),
    #("version", json.nullable(w.version, string)),
    #(
      "warning_reason",
      string(warning_reason_code_as_string(w.warning_reason_code)),
    ),
    #("dependency_type", string(dep_code_as_string(w.dep))),
    #("severity", string(severity_as_string(w.severity))),
    #("reason", string(w.reason)),
  ])
}

fn color(w: Warning, str: String) {
  case w.severity {
    SeverityCritical | SeverityPackageRetiredSecurity ->
      print.format_critical(str)
    SeverityHigh | SeverityRejectedLicense -> print.format_high(str)
    SeverityModerate
    | SeverityPackageRetiredRenamed
    | SeverityPackageRetiredDeprecated
    | SeverityPackageOutdated -> print.format_moderate(str)
    SeverityLow | SeverityPackageRetiredInvalid -> print.format_low(str)
    SeverityUnknown(_) | SeverityPackageRetiredOtherReason(_) ->
      print.format_warning(str)
  }
}
