import gleam/list
import gleam/order
import gleam/string
import gleamsver.{type SemVer}
import gxyz/cli

type CurriedComparator =
  fn(SemVer) -> Bool

pub fn parse(ver: String) -> SemVer {
  let parsed =
    string.split(ver, " ")
    |> list.last()
    |> cli.hard_fail_with_msg("could not parse " <> ver)

  gleamsver.parse(parsed)
  |> cli.hard_fail_with_msg("could not parse " <> parsed)
}

pub fn get_comparator(ver: String) -> CurriedComparator {
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

fn do_get_comparator(ver: String) -> CurriedComparator {
  let op =
    string.split(ver, " ")
    |> list.first()
    |> cli.hard_fail_with_msg("could not parse " <> ver)

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

fn eq(r: SemVer) -> CurriedComparator {
  fn(l) { gleamsver.are_equal(r, l) }
}

fn lt(r: SemVer) -> CurriedComparator {
  fn(l: SemVer) { gleamsver.compare(l, r) == order.Lt }
}

fn lte(r: SemVer) -> CurriedComparator {
  fn(l: SemVer) { eq(r)(l) || lt(r)(l) }
}

fn gt(r: SemVer) -> CurriedComparator {
  fn(l: SemVer) { gleamsver.compare(l, r) == order.Gt }
}

fn gte(r: SemVer) -> CurriedComparator {
  fn(l: SemVer) { eq(r)(l) || gt(r)(l) }
}

fn all_good() -> CurriedComparator {
  fn(_: SemVer) { False }
}
