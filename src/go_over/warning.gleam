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
}

fn warning_reason_code_as_string(w: WarningReasonCode) -> String {
  case w {
    Retired -> "Retired"
    Vulnerable -> "Vulnerable"
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

pub type Warning {
  Warning(
    advisory_id: Option(String),
    package: String,
    version: String,
    reason: String,
    warning_reason_code: WarningReasonCode,
    severity: String,
    dep: Dep,
  )
}

pub fn adv_to_warning(pkg: Package, advs: List(Advisory)) -> List(Warning) {
  list.map(advs, fn(adv) {
    Warning(
      Some(adv.id),
      pkg.name,
      pkg.version_raw,
      adv.description,
      Vulnerable,
      string.lowercase(adv.severity),
      Direct,
    )
  })
}

pub fn retired_to_warning(pkg: Package, ret: ReleaseRetirement) -> Warning {
  Warning(
    None,
    pkg.name,
    pkg.version_raw,
    core.print_ret(ret),
    Retired,
    "package-retired",
    Direct,
  )
}

pub fn format_as_string(w: Warning) -> String {
  [
    "ID: " <> option.unwrap(w.advisory_id, "null"),
    "Package: " <> w.package,
    "Version: " <> w.version,
    "WarningReason: " <> warning_reason_code_as_string(w.warning_reason_code),
    "Dependency Type: " <> dep_code_as_string(w.dep),
    "Severity: " <> string.lowercase(w.severity),
    "Reason: " <> w.reason,
  ]
  |> string.join("\n")
  |> color(w, _)
}

pub fn format_as_string_minimal(w: Warning) -> String {
  color(
    w,
    w.package <> "-" <> w.version <> ": " <> string.lowercase(w.severity),
  )
}

pub fn format_as_json(w: Warning) -> Json {
  object([
    #("id", json.nullable(w.advisory_id, string)),
    #("package", string(w.package)),
    #("version", string(w.version)),
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
    "high" -> print.format_high(str)
    "moderate" -> print.format_moderate(str)
    "low" -> print.format_low(str)
    _ -> print.format_warning(str)
  }
}
