import gleeunit/should
import go_over/comparisons.{get_comparator, parse}
import stoiridh/version.{new}

pub fn parse_test() {
  parse(" 1.1.1")
  parse(" 0.1.0")
  parse(" 2.1.0-beta")
  parse("2.54.0")
}

pub fn get_comparator_test() {
  let assert Ok(v) = new(1, 1, 1)
  get_comparator("<= 1.1.1")(v)
  |> should.be_true

  get_comparator("< 1.1.1")(v)
  |> should.be_false

  get_comparator("> 1.1.1")(v)
  |> should.be_false

  get_comparator(">= 1.1.1")(v)
  |> should.be_true

  get_comparator("== 1.1.1")(v)
  |> should.be_true

  get_comparator("= 1.1.1")(v)
  |> should.be_true

  get_comparator("1.1.1")(v)
  |> should.be_true

  get_comparator("<= 1.1.1, > 1.0.0")(v)
  |> should.be_true

  get_comparator("> 1.0.0, <= 1.1.1")(v)
  |> should.be_true

  get_comparator("< 1.1.1, > 1.0.0")(v)
  |> should.be_false

  get_comparator("> 1.0.0, < 1.1.1")(v)
  |> should.be_false

  get_comparator("> 1.1.1, < 1.0.0")(v)
  |> should.be_false
}
