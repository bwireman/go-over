import gleam/hexpm.{type ReleaseRetirement}
import gleam/list
import gleam/string
import go_over/advisories.{type ADV}
import go_over/packages.{type Package}
import go_over/retired
import simplifile

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
  Indirect
}

fn dep_code_as_string(d: Dep) {
  case d {
    Direct -> "Direct"
    Indirect -> "Indirect dependency"
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

fn print_adv(adv: ADV) {
  let assert Ok(contents) = simplifile.read(adv.file)

  "\n" <> contents
}

pub fn adv_to_warning(pkg: Package, adv: List(ADV)) {
  Warning(
    pkg.name,
    pkg.version_raw,
    adv
      |> list.map(print_adv)
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
    "Package: " <> w.package,
    "Version: " <> w.version,
    "WarningReason: " <> warning_reason_code_as_string(w.warning_reason_code),
    "Dependency Type: " <> dep_code_as_string(w.dep),
    "Reason: " <> w.reason,
  ]
  |> string.join("\n")
}
