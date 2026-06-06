import go_over/workspace

pub fn discover_workspace_test() {
  let workspace.DiscoverResult(projects, skipped) =
    workspace.discover("test/testdata/workspace", 3)

  assert projects
    == [
      "test/testdata/workspace/app_a",
      "test/testdata/workspace/app_b",
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
    workspace.discover("test/testdata/workspace_depth", 3)

  assert projects
    == [
      "test/testdata/workspace_depth/at_max/nested/project",
      "test/testdata/workspace_depth/shallow",
    ]

  assert skipped
    == ["test/testdata/workspace_depth/too_deep/nested/nested/project"]
}
