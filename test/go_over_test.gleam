import birdie
import gleeunit
import go_over/util/globals
import pprint

pub fn main() {
  globals.set_force(True)
  gleeunit.main()
}

@target(erlang)
fn runtime() {
  "Erlang"
}

@target(javascript)
fn runtime() {
  "Javascript"
}

pub fn birdie_snap(value: a, name: String) -> a {
  value
  |> pprint.with_config(pprint.Config(
    pprint.Unstyled,
    pprint.BitArraysAsString,
    pprint.Labels,
  ))
  |> birdie.snap(runtime() <> "@" <> name)

  value
}

pub fn birdie_snap_with_input(value: a, input: b, name: String) -> a {
  birdie_snap(#(input, value), name)

  value
}
