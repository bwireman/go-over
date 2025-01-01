import gleam/io
@target(erlang)
import gleam/list
import gleam/option
import go_over/util/spinner
import gxyz/gxyz_function
import shellout

pub fn raw(msg: String, color: String) {
  shellout.style(msg, with: shellout.color([color]), custom: [])
}

pub fn progress(verbose: Bool, msg: String) {
  gxyz_function.iff_nil(verbose, fn() {
    shellout.style(msg, with: shellout.color(["brightmagenta"]), custom: [])
    |> io.println
  })
}

pub fn format_warning(msg: String) {
  shellout.style(msg <> "\n", with: shellout.color(["red"]), custom: [])
}

pub fn format_critical(msg: String) {
  format_warning(msg)
}

pub fn format_high(msg: String) {
  shellout.style(msg <> "\n", with: shellout.color(["yellow"]), custom: [])
}

pub fn format_moderate(msg: String) {
  shellout.style(msg <> "\n", with: shellout.color(["blue"]), custom: [])
}

pub fn format_low(msg: String) {
  shellout.style(msg <> "\n", with: shellout.color(["cyan"]), custom: [])
}

pub fn warning(msg: String) {
  format_warning(msg)
  |> io.println
}

pub fn high(msg: String) {
  format_high(msg)
  |> io.println
}

pub fn success(msg: String) {
  shellout.style(msg, with: shellout.color(["brightgreen"]), custom: [])
  |> io.println
}

// --- SPINNER ---
fn little_guy(msg: String) {
  "🕵️‍♂️ " <> msg
}

@target(erlang)
pub fn new_spinner(msg: String, verbose: Bool) {
  gxyz_function.iff(
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
  gxyz_function.iff_nil(!verbose, fn() {
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
  gxyz_function.iff_nil(!verbose, fn() {
    little_guy(msg)
    |> io.println()
  })
}

pub fn stop_spinner(spinner: option.Option(spinner.Spinner)) {
  option.map(spinner, spinner.stop)
}
