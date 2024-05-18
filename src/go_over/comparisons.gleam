import gleam/io
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
  let assert Ok(com) =
    string.split(ver, " ")
    |> list.first
    |> io.debug

  case com {
    "<" -> lt
    "<=" -> lt
    ">" -> gt
    ">=" -> gt
    "==" -> eq
    "=" -> eq
    _ -> eq
  }
}

pub fn eq(l: Version, r: Version) {
  version.compare(l, r) == order.Eq
}

pub fn lt(l: Version, r: Version) {
  version.compare(l, r) == order.Lt
}

pub fn lte(l: Version, r: Version) {
  eq(l, r) || lt(l, r)
}

pub fn gt(l: Version, r: Version) {
  version.compare(l, r) == order.Gt
}

pub fn gte(l: Version, r: Version) {
  eq(l, r) || gt(l, r)
}
