import gleam/hexpm.{type ReleaseRetirement}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import go_over/advisories/advisories.{type Advisory}
import go_over/packages.{type Package}
import go_over/retired
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
      adv.severity,
      Direct,
    )
  })
}

pub fn retired_to_warning(pkg: Package, ret: ReleaseRetirement) -> Warning {
  Warning(
    None,
    pkg.name,
    pkg.version_raw,
    retired.print_ret(ret),
    Retired,
    "package-retired",
    Direct,
  )
}

pub fn format_as_string(w: Warning) -> String {
  let str =
    [
      "ID: " <> option.unwrap(w.advisory_id, "N/A"),
      "Package: " <> w.package,
      "Version: " <> w.version,
      "WarningReason: " <> warning_reason_code_as_string(w.warning_reason_code),
      "Dependency Type: " <> dep_code_as_string(w.dep),
      "Severity: " <> string.lowercase(w.severity),
      "Reason: " <> w.reason,
    ]
    |> string.join("\n")

  case string.lowercase(w.severity) {
    "critical" -> print.format_critical(str)
    "high" -> print.format_high(str)
    "moderate" -> print.format_moderate(str)
    "low" -> print.format_low(str)
    _ -> print.format_warning(str)
  }
}
