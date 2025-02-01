import birdie
import gleeunit
import platform
import pprint

pub fn main() {
  gleeunit.main()
}

pub fn birdie_snap(value: a, name: String) -> a {
  value
  |> pprint.with_config(pprint.Config(
    pprint.Unstyled,
    pprint.BitArraysAsString,
    pprint.Labels,
  ))
  |> birdie.snap(name <> "_" <> pprint.format(platform.runtime()))

  value
}
