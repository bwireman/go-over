import delay
import go_over/util/print
import shellout

pub fn hard_fail(res: Result(a, b), msg: String) -> a {
  case res {
    Ok(val) -> val
    _ -> {
      print.warning("Error: " <> msg)
      shellout.exit(1)
      panic as "Unreachable, please create an issue in https://github.com/bwireman/go-over if you see this"
    }
  }
}

pub fn retry_cmd(
  cmd: String,
  args: List(String),
) -> Result(String, #(Int, String)) {
  delay.delay_effect(fn() {
    shellout.command(run: cmd, with: args, in: ".", opt: [])
  })
  |> delay.retry_with_backoff(3)
  |> delay.run()
}
