import gleam/list
import gleam/order
import gleam/string
import go_over/workspace

const workspace = "test/testdata/workspace"

const workspace_depth = "test/testdata/workspace_depth"

const workspace_root_project = "test/testdata/workspace_root_project"

const workspace_partial = "test/testdata/workspace_partial"

const workspace_skip = "test/testdata/workspace_skip"

const workspace_depth_zero = "test/testdata/workspace_depth_zero"

const workspace_multi_level = "test/testdata/workspace_multi_level"

pub fn discover_workspace_test() {
  let workspace.DiscoverResult(projects, skipped) =
    workspace.discover(workspace, 3)

  assert projects
    == [
      workspace <> "/app_a",
      workspace <> "/app_b",
    ]
  assert skipped == []
}

pub fn discover_empty_test() {
  let workspace.DiscoverResult(projects, skipped) =
    workspace.discover("test/testdata/gleam", 3)

  assert projects == []
  assert skipped == []
}

pub fn discover_max_depth_test() {
  let workspace.DiscoverResult(projects, skipped) =
    workspace.discover(workspace_depth, 3)

  assert projects
    == [
      workspace_depth <> "/at_max/nested/project",
      workspace_depth <> "/shallow",
    ]

  assert skipped == [workspace_depth <> "/too_deep/nested/nested/project"]
}

pub fn discover_or_error_empty_test() {
  let assert Error(msg) = workspace.discover_or_error("test/testdata/gleam", 3)

  assert msg == "no gleam projects found under test/testdata/gleam"
}

pub fn discover_or_error_success_test() {
  let assert Ok(workspace.DiscoverResult(projects, skipped)) =
    workspace.discover_or_error(workspace, 3)

  assert list.length(projects) == 2
  assert skipped == []
}

pub fn discover_root_is_project_test() {
  let workspace.DiscoverResult(projects, skipped) =
    workspace.discover(workspace_root_project, 3)

  assert projects == [workspace_root_project]
  assert skipped == []
}

pub fn discover_partial_projects_test() {
  let workspace.DiscoverResult(projects, skipped) =
    workspace.discover(workspace_partial, 3)

  assert projects == [workspace_partial <> "/complete"]
  assert skipped == []
}

pub fn discover_skips_ignored_dirs_test() {
  let workspace.DiscoverResult(projects, skipped) =
    workspace.discover(workspace_skip, 3)

  assert projects == [workspace_skip <> "/visible"]
  assert skipped == []
}

pub fn discover_sorts_projects_test() {
  let workspace.DiscoverResult(projects, _) = workspace.discover(workspace, 3)

  assert projects
    == list.sort(projects, fn(a, b) {
      case string.compare(a, b) {
        order.Eq -> order.Eq
        order.Lt -> order.Lt
        order.Gt -> order.Gt
      }
    })
}

pub fn discover_max_depth_zero_test() {
  let workspace.DiscoverResult(projects, skipped) =
    workspace.discover(workspace_depth_zero, 0)

  assert projects == []
  assert skipped == [workspace_depth_zero <> "/container/project"]
}

pub fn discover_max_depth_one_test() {
  let workspace.DiscoverResult(projects, skipped) =
    workspace.discover(workspace_depth, 1)

  assert projects == [workspace_depth <> "/shallow"]
  assert skipped
    == [
      workspace_depth <> "/at_max/nested/project",
      workspace_depth <> "/too_deep/nested/nested/project",
    ]
}

pub fn discover_nonexistent_path_test() {
  let workspace.DiscoverResult(projects, skipped) =
    workspace.discover("test/testdata/does_not_exist", 3)

  assert projects == []
  assert skipped == []
}

pub fn discover_scan_root_is_project_ignores_max_depth_test() {
  let workspace.DiscoverResult(projects, skipped) =
    workspace.discover(workspace_root_project, 0)

  assert projects == [workspace_root_project]
  assert skipped == []
}

pub fn discover_or_error_root_is_project_test() {
  let assert Ok(workspace.DiscoverResult(projects, skipped)) =
    workspace.discover_or_error(workspace_root_project, 3)

  assert projects == [workspace_root_project]
  assert skipped == []
}

pub fn discover_root_does_not_descend_test() {
  let workspace.DiscoverResult(projects, skipped) =
    workspace.discover(workspace_root_project, 3)

  assert projects == [workspace_root_project]
  assert skipped == []
  assert !list.contains(
    projects,
    workspace_root_project <> "/packages/nested_app",
  )
}

pub fn discover_multi_level_test() {
  let workspace.DiscoverResult(projects, skipped) =
    workspace.discover(workspace_multi_level, 3)

  assert projects
    == [
      workspace_multi_level <> "/libs/common",
      workspace_multi_level <> "/services/api",
      workspace_multi_level <> "/services/worker",
    ]
  assert skipped == []
}

pub fn discover_partial_only_gleam_test() {
  let workspace.DiscoverResult(projects, skipped) =
    workspace.discover(workspace_partial <> "/only_gleam", 3)

  assert projects == []
  assert skipped == []
}

pub fn discover_partial_only_manifest_test() {
  let workspace.DiscoverResult(projects, skipped) =
    workspace.discover(workspace_partial <> "/only_manifest", 3)

  assert projects == []
  assert skipped == []
}

pub fn discover_skipped_sorted_test() {
  let workspace.DiscoverResult(projects, skipped) =
    workspace.discover(workspace_depth, 1)

  assert projects == [workspace_depth <> "/shallow"]
  assert skipped
    == [
      workspace_depth <> "/at_max/nested/project",
      workspace_depth <> "/too_deep/nested/nested/project",
    ]
  assert skipped
    == list.sort(skipped, fn(a, b) {
      case string.compare(a, b) {
        order.Eq -> order.Eq
        order.Lt -> order.Lt
        order.Gt -> order.Gt
      }
    })
}

pub fn discover_skips_node_modules_test() {
  let workspace.DiscoverResult(projects, skipped) =
    workspace.discover(workspace_skip, 3)

  assert projects == [workspace_skip <> "/visible"]
  assert skipped == []
  assert !list.contains(projects, workspace_skip <> "/node_modules/hidden")
}

pub fn discover_skips_go_over_dir_test() {
  let workspace.DiscoverResult(projects, skipped) =
    workspace.discover(workspace_skip, 3)

  assert projects == [workspace_skip <> "/visible"]
  assert skipped == []
  assert !list.contains(projects, workspace_skip <> "/.go-over/hidden")
}

pub fn discover_max_depth_two_test() {
  let workspace.DiscoverResult(projects, skipped) =
    workspace.discover(workspace_depth, 2)

  assert projects == [workspace_depth <> "/shallow"]
  assert skipped
    == [
      workspace_depth <> "/at_max/nested/project",
      workspace_depth <> "/too_deep/nested/nested/project",
    ]
}

pub fn discover_or_error_nonexistent_test() {
  let assert Error(msg) =
    workspace.discover_or_error("test/testdata/does_not_exist", 3)

  assert msg == "no gleam projects found under test/testdata/does_not_exist"
}

pub fn discover_or_error_depth_zero_empty_test() {
  let assert Error(msg) = workspace.discover_or_error(workspace_depth_zero, 0)

  assert msg == "no gleam projects found under " <> workspace_depth_zero
}

pub fn discover_or_error_skipped_only_test() {
  let assert Error(msg) = workspace.discover_or_error(workspace_depth, 0)

  assert msg == "no gleam projects found under " <> workspace_depth
}
