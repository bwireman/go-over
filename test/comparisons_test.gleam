import gleamsver.{parse}
import gleeunit/should
import go_over/comparisons

pub fn parse_test() {
  comparisons.parse(" 1.1.1")
  comparisons.parse(" 0.1.0")
  comparisons.parse(" 2.1.0-beta")
  comparisons.parse("2.54.0")
}

pub fn get_comparator_test() {
  let assert Ok(v) = parse("1.1.1")
  comparisons.get_comparator("<= 1.1.1")(v)
  |> should.be_true

  comparisons.get_comparator("< 1.1.1")(v)
  |> should.be_false

  comparisons.get_comparator("> 1.1.1")(v)
  |> should.be_false

  comparisons.get_comparator(">= 1.1.1")(v)
  |> should.be_true

  comparisons.get_comparator("== 1.1.1")(v)
  |> should.be_true

  comparisons.get_comparator("= 1.1.1")(v)
  |> should.be_true

  comparisons.get_comparator("1.1.1")(v)
  |> should.be_true

  comparisons.get_comparator("<= 1.1.1, > 1.0.0")(v)
  |> should.be_true

  comparisons.get_comparator("> 1.0.0, <= 1.1.1")(v)
  |> should.be_true

  comparisons.get_comparator("< 1.1.1, > 1.0.0")(v)
  |> should.be_false

  comparisons.get_comparator("> 1.0.0, < 1.1.1")(v)
  |> should.be_false

  comparisons.get_comparator("> 1.1.1, < 1.0.0")(v)
  |> should.be_false
}
