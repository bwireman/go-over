import gleam/hexpm.{type ReleaseRetirement}
import gleam/list
import gleam/string
import go_over/advisories.{type ADV}
import go_over/packages.{type Package}
import go_over/retired

pub type WarningReasonCode {
  Retired
  Vulnerable
}

fn warning_reason_code_as_string(w: WarningReasonCode) {
  case w {
    Retired -> "Retired"
    Vulnerable -> "Vulnerable"
  }
}

pub type Dep {
  Direct
  Indirect(of: String)
}

fn dep_code_as_string(d: Dep) {
  case d {
    Direct -> "Direct"
    Indirect(p) -> "Indirect dependency of " <> p
  }
}

pub type Warning {
  Warning(
    package: String,
    version: String,
    reason: String,
    warning_reason_code: WarningReasonCode,
    dep: Dep,
  )
}

pub fn adv_to_warning(pkg: Package, adv: List(ADV)) {
  Warning(
    pkg.name,
    pkg.version_raw,
    list.map(adv, advisories.print_adv)
      |> string.join("\n"),
    Vulnerable,
    Direct,
  )
}

pub fn retired_to_warning(pkg: Package, ret: ReleaseRetirement) {
  Warning(pkg.name, pkg.version_raw, retired.print_ret(ret), Retired, Direct)
}

pub fn print(w: Warning) {
  [
    "PACKAGE: ",
    w.package,
    "VERSION: ",
    w.version,
    "REASON: ",
    w.reason,
    "warningReasonCode: ",
    warning_reason_code_as_string(w.warning_reason_code),
    "Dep: ",
    dep_code_as_string(w.dep),
  ]
  |> string.join("\n")
}
