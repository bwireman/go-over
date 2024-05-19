import gleam/erlang
import gleam/erlang/atom

pub fn start() {
  erlang.ensure_all_started(atom.create_from_string("yamerl"))
}

@external(erlang, "yamll", "parse")
pub fn parse(path: String) -> #(String, List(String))



