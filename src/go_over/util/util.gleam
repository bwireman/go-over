import delay
import shellout

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

pub fn do_panic() {
  panic as "Unreachable, please create an issue in https://github.com/bwireman/go-over if you see this"
}
