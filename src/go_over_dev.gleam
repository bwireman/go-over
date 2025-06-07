import gleam/io
import gleam/option.{None, Some}
import go_over
import go_over/config
import go_over/warning.{
  Direct, Indirect, SeverityCritical, SeverityHigh, SeverityLow,
  SeverityModerate, SeverityPackageRetiredSecurity, SeverityRejectedLicense,
  Warning, WarningReasonRetired, WarningReasonVulnerable,
}
import gxyz/function as gfunction
import shellout

const example_warnings = [
  Warning(
    None,
    "fake",
    Some("x.y.z"),
    "Retired",
    WarningReasonVulnerable,
    SeverityCritical,
    Direct,
  ),
  Warning(
    None,
    "another_fake",
    Some("1.2.3"),
    "Vulnerable",
    WarningReasonVulnerable,
    SeverityHigh,
    Direct,
  ),
  Warning(
    None,
    "and_another",
    Some("4.5.6"),
    "Vulnerable",
    WarningReasonVulnerable,
    SeverityModerate,
    Direct,
  ),
  Warning(
    None,
    "one_more",
    Some("7.8.9"),
    "Vulnerable",
    WarningReasonVulnerable,
    SeverityLow,
    Indirect,
  ),
  Warning(
    None,
    "this_one_was_retired",
    Some("10.11.12"),
    "Retired",
    WarningReasonRetired,
    SeverityPackageRetiredSecurity,
    Indirect,
  ),
  Warning(
    None,
    "rejected_license",
    None,
    "Retired",
    WarningReasonRetired,
    SeverityRejectedLicense,
    Indirect,
  ),
]

pub fn main() {
  let conf = case
    config.spin_up(config.read_config("gleam.toml"), shellout.arguments())
  {
    Error(e) -> {
      io.println_error(e)
      shellout.exit(0)
      panic as "Unreachable, please create an issue in https://github.com/bwireman/go-over if you see this"
    }
    Ok(conf) -> conf
  }

  gfunction.iff_nil(
    True,
    gfunction.freeze2(go_over.print_warnings, example_warnings, conf),
  )
}
