import gleam/io
import gxyz/function
import shellout

pub fn raw(msg: String, color: String) {
  shellout.style(msg, with: shellout.color([color]), custom: [])
}

pub fn progress(verbose: Bool, msg: String) {
  function.iff_nil(verbose, fn() {
    shellout.style(msg, with: shellout.color(["brightmagenta"]), custom: [])
    |> io.println()
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
  msg
  |> format_warning()
  |> io.println()
}

pub fn high(msg: String) {
  msg
  |> format_high()
  |> io.println()
}

pub fn success(msg: String) {
  shellout.style(msg, with: shellout.color(["brightgreen"]), custom: [])
  |> io.println()
}
