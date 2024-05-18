import gleam/list
import gleam/order
import gleam/string
import stoiridh/version.{type Version}

pub fn parse(ver: String) {
  let assert Ok(v) =
    string.split(ver, " ")
    |> list.last

  let assert Ok(x) = version.parse(v)
  x
}

pub fn get_comparator(ver: String) {
  let cleaned =
    ver
    |> string.split(",")
    |> list.map(string.trim)

  case cleaned {
    [] -> all_good()
    [v] -> do_get_comparator(v)
    [first, ..tail] -> {
      list.fold(tail, do_get_comparator(first), fn(acc, v) {
        fn(x) { acc(x) && do_get_comparator(v)(x) }
      })
    }
  }
}

fn do_get_comparator(ver: String) {
  let assert Ok(op) =
    string.split(ver, " ")
    |> list.first

  let semver = parse(ver)
  map_to_comp(op)(semver)
}

pub fn map_to_comp(op: String) {
  case op {
    "<" -> lt
    "<=" -> lte
    ">" -> gt
    ">=" -> gte
    "==" -> eq
    "=" -> eq
    _ -> eq
  }
}

pub fn eq(r: Version) {
  fn(l) { version.compare(l, r) == order.Eq }
}

pub fn lt(r: Version) {
  fn(l) { version.compare(l, r) == order.Lt }
}

pub fn lte(r: Version) {
  fn(l) { eq(r)(l) || lt(r)(l) }
}

pub fn gt(r: Version) {
  fn(l) { version.compare(l, r) == order.Gt }
}

pub fn gte(r: Version) {
  fn(l) { eq(r)(l) || gt(r)(l) }
}

pub fn all_good() {
  fn(_: Version) { False }
}
