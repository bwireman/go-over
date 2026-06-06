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
