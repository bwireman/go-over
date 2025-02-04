import gleam/hexpm.{type ReleaseRetirement}
import gleam/json.{type Json, object, string}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import go_over/advisories/advisories.{type Advisory}
import go_over/packages.{type Package}
import go_over/retired/core
import go_over/util/print

pub type WarningReasonCode {
  Retired
  Vulnerable
  Outdated
  RejectedLicense(name: String)
}

fn warning_reason_code_as_string(w: WarningReasonCode) -> String {
  case w {
    Retired -> "Retired"
    Vulnerable -> "Vulnerable"
    Outdated -> "Outdated"
    RejectedLicense(name) -> "Rejected License (" <> name <> ")"
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
    severity: String,
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
      Vulnerable,
      string.lowercase(adv.severity),
      dep_code_from_bool(pkg.direct),
    )
  })
}

pub fn retired_to_warning(pkg: Package, ret: ReleaseRetirement) -> Warning {
  Warning(
    None,
    pkg.name,
    Some(pkg.version_raw),
    core.print_ret(ret),
    Retired,
    "package-retired (" <> hexpm.retirement_reason_to_string(ret.reason) <> ")",
    dep_code_from_bool(pkg.direct),
  )
}

pub fn outdated_to_warning(pkg: Package, new_version: String) -> Warning {
  Warning(
    None,
    pkg.name,
    Some(pkg.version_raw),
    new_version <> " exists",
    Outdated,
    "package-outdated",
    dep_code_from_bool(pkg.direct),
  )
}

pub fn rejected_license_to_warning(pkg: Package, license: String) -> Warning {
  Warning(
    None,
    pkg.name,
    None,
    "Rejected License found: " <> license,
    Outdated,
    "rejected-license",
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
    "Severity: " <> string.lowercase(w.severity),
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
        w.package <> "-" <> version <> ": " <> string.lowercase(w.severity),
      )

    option.None -> color(w, w.package <> ": " <> string.lowercase(w.severity))
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
    #("severity", string(string.lowercase(w.severity))),
    #("reason", string(w.reason)),
  ])
}

fn color(w: Warning, str: String) {
  case string.lowercase(w.severity) {
    "critical" -> print.format_critical(str)
    "high" | "rejected-license" -> print.format_high(str)
    "moderate" | "package-outdated" -> print.format_moderate(str)
    "low" -> print.format_low(str)
    _ -> print.format_warning(str)
  }
}
