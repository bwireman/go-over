import go_over/util/print
import shellout

pub fn iff(v: Bool, f: fn() -> a, default: a) -> a {
  case v {
    True -> f()

    _ -> default
  }
}

pub fn iff_nil(v: Bool, f: fn() -> Nil) -> Nil {
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

pub fn hard_fail(res: Result(a, b), msg: String) -> a {
  case res {
    Ok(val) -> val
    _ -> {
      print.warning("Error: " <> msg)
      shellout.exit(1)
      panic as "unreachable"
    }
  }
}

@target(javascript)
@external(javascript, "../../ffi.mjs", "do_fetch")
pub fn do_fetch(x: String) -> Result(String, Nil)
