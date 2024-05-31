import gleam/list
import gleam/order
import gleam/string
import gleamsver.{type SemVer}

pub fn parse(ver: String) -> SemVer {
  let assert Ok(parsed) =
    string.split(ver, " ")
    |> list.last

  let assert Ok(semver) = gleamsver.parse(parsed)
  semver
}

pub fn get_comparator(ver: String) -> fn(SemVer) -> Bool {
  let cleaned =
    ver
    |> string.split(",")
    |> list.map(string.trim)

  case cleaned {
    [] -> all_good()
    [v] -> do_get_comparator(v)
    [first, ..tail] -> {
      list.fold(tail, do_get_comparator(first), fn(acc, v) {
        fn(semver) { acc(semver) && do_get_comparator(v)(semver) }
      })
    }
  }
}

fn do_get_comparator(ver: String) -> fn(SemVer) -> Bool {
  let assert Ok(op) =
    string.split(ver, " ")
    |> list.first

  let semver = parse(ver)
  case op {
    "<" -> lt(semver)
    "<=" -> lte(semver)
    ">" -> gt(semver)
    ">=" -> gte(semver)
    "==" -> eq(semver)
    "=" -> eq(semver)
    _ -> eq(semver)
  }
}

fn eq(r: SemVer) -> fn(SemVer) -> Bool {
  fn(l: SemVer) { gleamsver.compare(l, r) == order.Eq }
}

fn lt(r: SemVer) -> fn(SemVer) -> Bool {
  fn(l: SemVer) { gleamsver.compare(l, r) == order.Lt }
}

fn lte(r: SemVer) -> fn(SemVer) -> Bool {
  fn(l: SemVer) { eq(r)(l) || lt(r)(l) }
}

fn gt(r: SemVer) -> fn(SemVer) -> Bool {
  fn(l: SemVer) { gleamsver.compare(l, r) == order.Gt }
}

fn gte(r: SemVer) -> fn(SemVer) -> Bool {
  fn(l: SemVer) { eq(r)(l) || gt(r)(l) }
}

fn all_good() -> fn(SemVer) -> Bool {
  fn(_: SemVer) { False }
}
