import gleam/json
import gleam/list
import gleam/string
import gleamsver
import go_over
import go_over/advisories/advisories
import go_over/config
import go_over/packages
import go_over/sarif
import go_over/warning
import go_over_test

pub fn skipped_workspace_warnings_test() {
  let warnings =
    go_over.skipped_workspace_warnings([
      "test/testdata/workspace_depth/too_deep/nested/nested/project",
    ])

  assert list.length(warnings) == 1
  assert warning.is_info(
    list.first(warnings)
    |> fn(r) {
      case r {
        Ok(w) -> w
        Error(_) -> panic as "expected warning"
      }
    },
  )
}

pub fn warnings_for_json_results_test() {
  let info = warning.info_to_warning("x", "Info: example")

  let fatal = warning.info_to_warning("y", "fatal placeholder")

  let results = [
    go_over.AuditResult(
      project_root: "a",
      fatal_warnings: [fatal],
      info_warnings: [info],
      outdated_failed: False,
      format: config.JSON,
    ),
  ]

  assert list.length(go_over.warnings_for_json_results(results)) == 2
}

pub fn validate_workspace_formats_integration_test() {
  let assert Error(_) =
    config.validate_workspace_formats([
      #(config.Minimal, "a"),
      #(config.JSON, "b"),
    ])

  assert config.validate_workspace_formats([
      #(config.SARIF, "a"),
      #(config.SARIF, "b"),
    ])
    == Ok(Nil)
}

pub fn sarif_log_includes_info_test() {
  let info =
    warning.info_to_warning(
      "missing-package",
      "Info: package 'missing-package' is not a dependency",
    )

  let fatal =
    warning.adv_to_warning(
      packages.Package(
        "pkg",
        gleamsver.SemVer(1, 0, 0, "", ""),
        "1.0.0",
        True,
        packages.PackageSourceHex,
      ),
      [
        advisories.Advisory("GHSA-test", "pkg", "high", [], "example"),
      ],
    )
    |> list.first
    |> fn(r) {
      case r {
        Ok(w) -> w
        Error(_) -> panic as "expected warning"
      }
    }

  let sarif_json =
    sarif.to_sarif_log([#("backend", [info, fatal])])
    |> json.to_string()

  assert string.contains(sarif_json, "\"level\"")
  assert string.contains(sarif_json, "note")
  assert string.contains(sarif_json, "error")
  assert string.contains(sarif_json, "4.0.0")

  go_over_test.birdie_snap(sarif_json, "integration@sarif_log_includes_info")
}
