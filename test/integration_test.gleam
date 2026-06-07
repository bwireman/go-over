import gleam/json
import gleam/list
import gleam/option
import gleam/string
import gleamsver
import go_over
import go_over/advisories/advisories
import go_over/config
import go_over/packages
import go_over/sarif
import go_over/warning
import go_over/workspace
import go_over_test

const workspace = "test/testdata/workspace"

const workspace_depth = "test/testdata/workspace_depth"

const workspace_root_project = "test/testdata/workspace_root_project"

const audit_flags = config.Flags(
  force: False,
  outdated: False,
  verbose: False,
  format: option.None,
  global: False,
  local: True,
  puller: option.None,
  single_root: option.None,
  workspace_root: option.None,
  sarif_output: option.None,
)

pub fn skipped_workspace_warnings_test() {
  let path = "test/testdata/workspace_depth/too_deep/nested/nested/project"
  let warnings = go_over.skipped_workspace_warnings([path])

  assert list.length(warnings) == 1
  let assert Ok(w) = list.first(warnings)
  assert warning.is_info(w)
  assert string.contains(w.reason, path)
  assert string.contains(w.reason, "exceeds workspace_max_depth")
}

pub fn audit_workspace_fixture_test() {
  let assert Ok(result) =
    go_over.audit_project(audit_flags, workspace <> "/app_a")

  assert result.project_root == workspace <> "/app_a"
  assert result.fatal_warnings == []
  assert result.outdated_failed == False
}

pub fn audit_all_discovered_workspace_projects_test() {
  let workspace.DiscoverResult(projects, skipped) =
    workspace.discover(workspace, 3)

  assert skipped == []

  let results =
    list.map(projects, fn(project_root) {
      let assert Ok(result) = go_over.audit_project(audit_flags, project_root)
      result
    })

  assert list.length(results) == 2
  assert list.all(results, fn(r) { r.fatal_warnings == [] })
}

pub fn workspace_sarif_multiple_runs_test() {
  let info = warning.info_to_warning("skipped", "Info: project was skipped")

  let runs = [
    #(workspace <> "/app_a", [info]),
    #(workspace <> "/app_b", []),
  ]

  let sarif_json = sarif.to_sarif_log(runs) |> json.to_string()

  assert list.length(runs) == 2
  assert string.contains(sarif_json, workspace <> "/app_a/manifest.toml")
  assert string.contains(sarif_json, "\"results\":[]")
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

pub fn skipped_workspace_warnings_multiple_test() {
  let skipped = [
    workspace_depth <> "/at_max/nested/project",
    workspace_depth <> "/too_deep/nested/nested/project",
  ]

  let warnings = go_over.skipped_workspace_warnings(skipped)

  assert list.length(warnings) == 2
  assert list.all(warnings, warning.is_info)
  assert list.all(warnings, fn(w) {
    string.contains(w.reason, "exceeds workspace_max_depth")
  })
}

pub fn discover_depth_skipped_warnings_integration_test() {
  let workspace.DiscoverResult(projects, skipped) =
    workspace.discover(workspace_depth, 1)

  assert projects == [workspace_depth <> "/shallow"]
  assert list.length(skipped) == 2

  let warnings = go_over.skipped_workspace_warnings(skipped)

  assert list.length(warnings) == list.length(skipped)
  let assert Ok(first) = list.first(warnings)
  assert string.contains(
    first.reason,
    list.first(skipped)
      |> fn(r) {
        case r {
          Ok(s) -> s
          Error(_) -> ""
        }
      },
  )
}

pub fn audit_workspace_app_b_test() {
  let assert Ok(result) =
    go_over.audit_project(audit_flags, workspace <> "/app_b")

  assert result.project_root == workspace <> "/app_b"
  assert result.fatal_warnings == []
}

pub fn audit_workspace_root_project_test() {
  let assert Ok(result) =
    go_over.audit_project(audit_flags, workspace_root_project)

  assert result.project_root == workspace_root_project
  assert result.fatal_warnings == []
}

pub fn warnings_for_json_results_multiple_test() {
  let info_a = warning.info_to_warning("a", "Info: first")
  let info_b = warning.info_to_warning("b", "Info: second")
  let fatal = warning.info_to_warning("c", "fatal placeholder")

  let results = [
    go_over.AuditResult(
      project_root: "a",
      fatal_warnings: [fatal],
      info_warnings: [info_a],
      outdated_failed: False,
      format: config.JSON,
    ),
    go_over.AuditResult(
      project_root: "b",
      fatal_warnings: [],
      info_warnings: [info_b],
      outdated_failed: False,
      format: config.JSON,
    ),
  ]

  let warnings = go_over.warnings_for_json_results(results)

  assert list.length(warnings) == 3
  assert list.any(warnings, fn(w) { w.reason == "Info: first" })
  assert list.any(warnings, fn(w) { w.reason == "Info: second" })
  assert list.any(warnings, fn(w) { w.reason == "fatal placeholder" })
}

pub fn skipped_workspace_warning_message_test() {
  let path = "path/to/project"
  let assert Ok(w) = go_over.skipped_workspace_warnings([path]) |> list.first

  assert w.reason
    == "Info: project at '"
    <> path
    <> "' was skipped (exceeds workspace_max_depth)"
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
