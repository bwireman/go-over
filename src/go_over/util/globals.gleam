import directories
import filepath
import global_value
import gxyz/cli
import gxyz/function

const project_root_key = "project_root.global.data"

pub fn set_project_root(root: String) -> String {
  global_value.create_with_unique_name(project_root_key, function.freeze(root))
}

pub fn project_root() -> String {
  global_value.create_with_unique_name(project_root_key, function.freeze("."))
}

pub fn go_over_path() -> String {
  let #(path, name) = case use_global_cache() {
    True -> #(
      directories.cache_dir()
        |> cli.hard_fail_with_msg("could not get cache directory"),
      "go-over",
    )
    False -> #(project_root(), ".go-over")
  }

  filepath.join(path, name)
}

const verbose_key = "verbose.global.data"

pub fn set_verbose(verbose: Bool) -> Bool {
  global_value.create_with_unique_name(verbose_key, function.freeze(verbose))
}

pub fn verbose() -> Bool {
  global_value.create_with_unique_name(verbose_key, function.freeze(False))
}

const use_global_cache_key = "use_global_cache.global.data"

pub fn set_use_global_cache(use_global_cache: Bool) -> Bool {
  global_value.create_with_unique_name(
    use_global_cache_key,
    function.freeze(use_global_cache),
  )
}

pub fn use_global_cache() -> Bool {
  global_value.create_with_unique_name(
    use_global_cache_key,
    function.freeze(False),
  )
}

const force_key = "force.global.data"

pub fn set_force(force: Bool) -> Bool {
  global_value.create_with_unique_name(force_key, function.freeze(force))
}

pub fn force() -> Bool {
  global_value.create_with_unique_name(force_key, function.freeze(False))
}
