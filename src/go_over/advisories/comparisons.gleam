import gleam/list
import gleam/order
import gleam/string
import gleamsver.{type SemVer}
import go_over/util/util.{hard_fail}

pub fn parse(ver: String) -> SemVer {
  let parsed =
    string.split(ver, " ")
    |> list.last
    |> hard_fail("could not parse " <> ver)

  let semver =
    gleamsver.parse(parsed)
    |> hard_fail("could not parse " <> parsed)
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
  let op =
    string.split(ver, " ")
    |> list.first
    |> hard_fail("could not parse " <> ver)

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
