import fetch from "sync-fetch"
import { parse as parse_yaml } from "yaml"
import { toList } from "../prelude.mjs"

export function do_fetch(path) {
    return fetch(path, {
        headers: { "User-Agent": "gleam.go-over.fetch" },
    }).text()
}

export function parse_adv(content) {
    const {
        id,
        package: pkg,
        severity,
        title,
        vulnerable_version_ranges,
    } = parse_yaml(content)

    return [id, pkg, severity, title, toList(vulnerable_version_ranges)]
}
