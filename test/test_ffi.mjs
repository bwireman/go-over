export function pprint(s) {
  return JSON.stringify(JSON.parse(s), null, "\t");
}
