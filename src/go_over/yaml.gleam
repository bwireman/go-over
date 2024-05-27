import gleam/erlang
import gleam/erlang/atom
import gleam/list
import gleam/string

pub fn start() {
  erlang.ensure_all_started(atom.create_from_string("yamerl"))
}

@external(erlang, "yamll", "parse")
fn read(path: String) -> #(List(UtfCodepoint), List(List(UtfCodepoint)))

pub fn parse(path: String) {
  let #(name, versions) = read(path)

  #(
    string.from_utf_codepoints(name),
    list.map(versions, string.from_utf_codepoints),
  )
}
