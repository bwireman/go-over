import gleam/list
import gleam/string

@external(erlang, "yamll", "parse")
fn read(
  path: String,
) -> #(
  List(UtfCodepoint),
  List(UtfCodepoint),
  List(UtfCodepoint),
  List(UtfCodepoint),
  List(List(UtfCodepoint)),
)

pub fn parse(path: String) -> #(String, String, String, String, List(String)) {
  let #(id, name, severity, desc, versions) = read(path)

  #(
    string.from_utf_codepoints(id),
    string.from_utf_codepoints(name),
    string.from_utf_codepoints(severity),
    string.from_utf_codepoints(desc),
    list.map(versions, string.from_utf_codepoints),
  )
}
