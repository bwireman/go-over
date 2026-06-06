import filepath
import gleam/list
import gleam/order
import gleam/string
import simplifile

const skip_dirs = ["build", "deps", "node_modules", ".go-over", ".git"]

const max_depth = 3

fn is_project_dir(dir: String) -> Bool {
  let gleam_toml = filepath.join(dir, "gleam.toml")
  let manifest = filepath.join(dir, "manifest.toml")

  case simplifile.is_file(gleam_toml), simplifile.is_file(manifest) {
    Ok(True), Ok(True) -> True
    _, _ -> False
  }
}

fn should_skip(name: String) -> Bool {
  string.starts_with(name, ".") || list.contains(skip_dirs, name)
}

fn do_discover(dir: String, depth: Int) -> List(String) {
  case is_project_dir(dir) {
    True -> [dir]
    False ->
      case depth >= max_depth {
        True -> []
        False ->
          case simplifile.read_directory(dir) {
            Ok(names) ->
              names
              |> list.filter(fn(name) { !should_skip(name) })
              |> list.flat_map(fn(name) {
                do_discover(filepath.join(dir, name), depth + 1)
              })
            Error(_) -> []
          }
      }
  }
}

pub fn discover(scan_root: String) -> List(String) {
  do_discover(scan_root, 0)
  |> list.sort(fn(a, b) {
    case string.compare(a, b) {
      order.Eq -> order.Eq
      order.Lt -> order.Lt
      order.Gt -> order.Gt
    }
  })
}

pub fn discover_or_error(scan_root: String) -> Result(List(String), String) {
  case discover(scan_root) {
    [] -> Error("no gleam projects found under " <> scan_root)
    projects -> Ok(projects)
  }
}
