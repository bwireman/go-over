import gleam/erlang.{type EnsureAllStartedError}
import gleam/erlang/atom.{type Atom}
import gleam/list
import gleam/string

pub fn start() -> Result(List(Atom), EnsureAllStartedError) {
  erlang.ensure_all_started(atom.create_from_string("yamerl"))
}

@external(erlang, "yamll", "parse")
fn read(path: String) -> #(List(UtfCodepoint), List(List(UtfCodepoint)))

pub fn parse(path: String) -> #(String, List(String)) {
  let #(name, versions) = read(path)

  #(
    string.from_utf_codepoints(name),
    list.map(versions, string.from_utf_codepoints),
  )
}
