import go_over/workspace

pub fn discover_workspace_test() {
  let projects = workspace.discover("test/testdata/workspace")

  assert projects
    == [
      "test/testdata/workspace/app_a",
      "test/testdata/workspace/app_b",
    ]
}

pub fn discover_empty_test() {
  assert workspace.discover("test/testdata/gleam") == []
}

pub fn discover_max_depth_test() {
  let projects = workspace.discover("test/testdata/workspace_depth")

  assert projects
    == [
      "test/testdata/workspace_depth/at_max/nested/project",
      "test/testdata/workspace_depth/shallow",
    ]
}
