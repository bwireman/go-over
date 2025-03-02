import gleam/string
import gleam/hexpm
import gleam/json
import gleam/list
import gleam/option
import gleamsver.{type SemVer, SemVer}
import go_over/advisories/advisories.{type Advisory, Advisory}
import go_over/packages.{type Package, Package}
import go_over/warning.{type Warning}
import go_over_test

@external(javascript, "./test_ffi.mjs", "pprint")
fn pprint_json(j: String) -> String {
  j
  |> string.replace("{", "\n{\n")
  |> string.replace("[", "\n[\n")
  |> string.replace(",", ",\n")
}

pub fn to_warning_format(name: String, input: a, warning: Warning) {
  go_over_test.birdie_snap_with_input(warning, input, "warning@" <> name)
  go_over_test.birdie_snap_with_input(
    warning.format_as_json(warning)
      |> json.to_string()
      |> pprint_json(),
    input,
    "warning_format_as_json@" <> name,
  )
  go_over_test.birdie_snap_with_input(
    warning.format_as_string(warning),
    input,
    "warning_format_as_string@" <> name,
  )
  go_over_test.birdie_snap_with_input(
    warning.format_as_string_minimal(warning),
    input,
    "warning_format_as_string_minimal@" <> name,
  )
}

const example_package = Package(
  name: "package for warning tests",
  version: SemVer(1, 2, 3, "pre", "build"),
  version_raw: "pre1.2.3-build",
  direct: True,
)

pub fn adv_to_warning_test() {
  let example_adv =
    Advisory(
      id: "ghsa-example",
      name: "example-advisory",
      severity: "example",
      vulnerable_version_ranges: ["1.1.1", "2.2.2"],
      description: "it's like bad",
    )

  warning.adv_to_warning(example_package, [example_adv])
  |> list.map(to_warning_format(
    "adv_to_warning",
    #(example_package, example_adv),
    _,
  ))
}

// SOMETHING IS WRONG HERE
pub fn retired_to_warning_test() {
  // with message
  {
    let retirement_other =
      hexpm.ReleaseRetirement(
        hexpm.OtherReason,
        option.Some("It's an example man"),
      )

    to_warning_format(
      "retired_to_warning_other",
      #(example_package, retirement_other),
      warning.retired_to_warning(example_package, retirement_other),
    )

    let retirement_invalid =
      hexpm.ReleaseRetirement(hexpm.Invalid, option.Some("It's an example man"))

    to_warning_format(
      "retired_to_warning_invalid",
      #(example_package, retirement_invalid),
      warning.retired_to_warning(example_package, retirement_invalid),
    )

    let retirement_security =
      hexpm.ReleaseRetirement(
        hexpm.Security,
        option.Some("It's an example man"),
      )

    to_warning_format(
      "retired_to_warning_security",
      #(example_package, retirement_security),
      warning.retired_to_warning(example_package, retirement_security),
    )

    let retirement_deprecated =
      hexpm.ReleaseRetirement(
        hexpm.Deprecated,
        option.Some("It's an example man"),
      )

    to_warning_format(
      "retired_to_warning_deprecated",
      #(example_package, retirement_deprecated),
      warning.retired_to_warning(example_package, retirement_deprecated),
    )

    let retirement_renamed =
      hexpm.ReleaseRetirement(hexpm.Renamed, option.Some("It's an example man"))

    to_warning_format(
      "retired_to_warning_renamed",
      #(example_package, retirement_renamed),
      warning.retired_to_warning(example_package, retirement_renamed),
    )
  }

  // no message
  {
    let retirement_other =
      hexpm.ReleaseRetirement(hexpm.OtherReason, option.None)

    to_warning_format(
      "retired_to_warning_other_none",
      #(example_package, retirement_other),
      warning.retired_to_warning(example_package, retirement_other),
    )

    let retirement_invalid = hexpm.ReleaseRetirement(hexpm.Invalid, option.None)

    to_warning_format(
      "retired_to_warning_invalid_none",
      #(example_package, retirement_invalid),
      warning.retired_to_warning(example_package, retirement_invalid),
    )

    let retirement_security =
      hexpm.ReleaseRetirement(hexpm.Security, option.None)

    to_warning_format(
      "retired_to_warning_security_none",
      #(example_package, retirement_security),
      warning.retired_to_warning(example_package, retirement_security),
    )

    let retirement_deprecated =
      hexpm.ReleaseRetirement(hexpm.Deprecated, option.None)

    to_warning_format(
      "retired_to_warning_deprecated_none",
      #(example_package, retirement_deprecated),
      warning.retired_to_warning(example_package, retirement_deprecated),
    )

    let retirement_renamed = hexpm.ReleaseRetirement(hexpm.Renamed, option.None)

    to_warning_format(
      "retired_to_warning_renamed_none",
      #(example_package, retirement_renamed),
      warning.retired_to_warning(example_package, retirement_renamed),
    )
  }
}

pub fn outdated_to_warning_test() {
  let ver = "1.2.3"

  to_warning_format(
    "outdated_to_warning",
    #(example_package, ver),
    warning.outdated_to_warning(example_package, ver),
  )
}

pub fn rejected_license_to_warning_test() {
  let license = "closed-source-crap"

  to_warning_format(
    "rejected_license_to_warning",
    #(example_package, license),
    warning.rejected_license_to_warning(example_package, license),
  )
}
