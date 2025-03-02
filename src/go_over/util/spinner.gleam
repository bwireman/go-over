@target(javascript)
import gleam/io
@target(erlang)
import gleam/list
import gleam/option
import gxyz/function
import spinner

// --- SPINNER ---
fn little_guy(msg: String) {
  "🕵️ " <> msg
}

@target(erlang)
pub fn new_spinner(msg: String, verbose: Bool) {
  function.iff(
    !verbose,
    fn() {
      spinner.new(little_guy(msg))
      |> spinner.with_frames(list.reverse(spinner.negative_dots_frames))
      |> spinner.start()
      |> option.Some()
    },
    option.None,
  )
}

@target(javascript)
pub fn new_spinner(msg: String, verbose: Bool) {
  function.iff_nil(!verbose, fn() {
    little_guy(msg)
    |> io.println()
  })
  option.None
}

@target(erlang)
pub fn set_text_spinner(
  spinner: option.Option(spinner.Spinner),
  msg: String,
  _: Bool,
) {
  option.map(spinner, spinner.set_text(_, little_guy(msg)))
  Nil
}

@target(javascript)
pub fn set_text_spinner(
  _: option.Option(spinner.Spinner),
  msg: String,
  verbose: Bool,
) {
  function.iff_nil(!verbose, fn() {
    little_guy(msg)
    |> io.println()
  })
}

pub fn stop_spinner(spinner: option.Option(spinner.Spinner)) {
  option.map(spinner, spinner.stop)
}
