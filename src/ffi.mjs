import fetch from "sync-fetch"
import { parse as parse_yaml } from "yaml"
import { toList, Ok, Error } from "../prelude.mjs"

export function do_fetch(path) {
    try {
        const response = fetch(path, {
            headers: { "User-Agent": "gleam.go-over.fetch" },
        })

        if (response.ok) {
            return new Ok(response.text())
        }
    } catch {
        return new Error(null)
    }

    return new Error(null)
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
