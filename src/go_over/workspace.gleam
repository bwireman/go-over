import filepath
import gleam/list
import gleam/order
import gleam/string
import simplifile

const skip_dirs = ["build", "deps", "node_modules", ".go-over", ".git"]

pub type DiscoverResult {
  DiscoverResult(projects: List(String), skipped: List(String))
}

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

fn find_projects_unlimited(dir: String) -> List(String) {
  case is_project_dir(dir) {
    True -> [dir]
    False ->
      case simplifile.read_directory(dir) {
        Ok(names) ->
          names
          |> list.filter(fn(name) { !should_skip(name) })
          |> list.flat_map(fn(name) {
            find_projects_unlimited(filepath.join(dir, name))
          })
        Error(_) -> []
      }
  }
}

fn has_project_descendant(dir: String) -> Bool {
  !list.is_empty(find_projects_unlimited(dir))
}

fn do_discover(
  dir: String,
  depth: Int,
  max_depth: Int,
  skipped: List(String),
) -> #(List(String), List(String)) {
  case is_project_dir(dir) {
    True -> #([dir], skipped)
    False ->
      case depth >= max_depth {
        True -> {
          let skipped = case has_project_descendant(dir) {
            True -> list.append(skipped, find_projects_unlimited(dir))
            False -> skipped
          }
          #([], skipped)
        }
        False ->
          case simplifile.read_directory(dir) {
            Ok(names) ->
              names
              |> list.filter(fn(name) { !should_skip(name) })
              |> list.fold(#([], skipped), fn(acc, name) {
                let #(projects, skipped) = acc
                let child = filepath.join(dir, name)
                let #(child_projects, child_skipped) =
                  do_discover(child, depth + 1, max_depth, [])
                #(
                  list.append(projects, child_projects),
                  list.append(skipped, child_skipped),
                )
              })
            Error(_) -> #([], skipped)
          }
      }
  }
}

pub fn discover(scan_root: String, max_depth: Int) -> DiscoverResult {
  let #(projects, skipped) = do_discover(scan_root, 0, max_depth, [])

  DiscoverResult(
    projects: projects
      |> list.sort(fn(a, b) {
        case string.compare(a, b) {
          order.Eq -> order.Eq
          order.Lt -> order.Lt
          order.Gt -> order.Gt
        }
      }),
    skipped: skipped
      |> list.unique()
      |> list.sort(fn(a, b) {
        case string.compare(a, b) {
          order.Eq -> order.Eq
          order.Lt -> order.Lt
          order.Gt -> order.Gt
        }
      }),
  )
}

pub fn discover_or_error(
  scan_root: String,
  max_depth: Int,
) -> Result(DiscoverResult, String) {
  case discover(scan_root, max_depth) {
    DiscoverResult([], _) ->
      Error("no gleam projects found under " <> scan_root)
    result -> Ok(result)
  }
}
