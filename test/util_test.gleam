@target(javascript)
import gleeunit/should
@target(javascript)
import go_over/util/util.{do_fetch}

@target(javascript)
pub fn do_fetch_test() {
  "https://localhost:8080"
  |> do_fetch()
  |> should.be_error()
}
