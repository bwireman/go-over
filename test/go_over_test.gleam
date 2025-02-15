import birdie
import gleeunit
import pprint

pub fn main() {
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
