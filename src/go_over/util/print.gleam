import gleam/io
import gleam/option
import go_over/util/spinner
import gxyz/gxyz_function.{iff, iff_nil}
import shellout

pub fn progress(verbose: Bool, msg: String) {
  iff_nil(verbose, fn() {
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

pub fn new_spinner(msg: String, verbose: Bool) {
  iff(
    !verbose,
    fn() {
      spinner.new(msg)
      |> spinner.with_frames(spinner.moon_frames)
      |> spinner.start()
      |> option.Some()
    },
    option.None,
  )
}

pub fn stop_spinner(spinner: option.Option(spinner.Spinner)) {
  spinner |> option.map(spinner.stop)
}

pub fn set_text_spinner(
  spinner: option.Option(spinner.Spinner),
  msg: String,
  verbose: Bool,
) {
  iff_nil(!verbose, fn() {
    spinner |> option.map(spinner.set_text(_, "🕵️‍♂️ " <> msg))
    Nil
  })
}
