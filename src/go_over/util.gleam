pub fn iff(v: Bool, f: fn() -> a, default: a) -> a {
  case v {
    True -> {
      f()
    }
    _ -> default
  }
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
