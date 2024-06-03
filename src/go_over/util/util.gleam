import gleam/option.{type Option, None, Some}
import go_over/util/print
import shellout

pub fn iff(v: Bool, f: fn() -> a, default: a) -> a {
  case v {
    True -> f()

    _ -> default
  }
}

pub fn iffnil(v: Bool, f: fn() -> Nil) -> Nil {
  iff(v, f, Nil)
}

pub fn throwaway(v: Bool, f: fn() -> Result(a, b)) -> Nil {
  case v {
    True -> {
      let _ = f()
      Nil
    }
    _ -> Nil
  }
}

pub fn hard_fail(res: Result(a, b), msg: String) -> Option(a) {
  case res {
    Ok(val) -> Some(val)
    _ -> {
      print.warning("Error: " <> msg)
      shellout.exit(1)
      None
    }
  }
}
