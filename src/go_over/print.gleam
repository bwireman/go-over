import gleam/io
import shellout

pub fn progress(msg: String) {
  shellout.style(msg, with: shellout.color(["magenta"]), custom: [])
  |> io.println
}

pub fn format_warning(msg: String) {
  shellout.style(msg <> "\n", with: shellout.color(["red"]), custom: [])
}

pub fn warning(msg: String) {
  format_warning(msg)
  |> io.println
}

pub fn success(msg: String) {
  shellout.style(msg, with: shellout.color(["brightgreen"]), custom: [])
  |> io.println
}
