import fetch from "sync-fetch"
import { parse as parse_yaml } from "yaml"
import { toList, Ok, Error } from "../prelude.mjs"

/**
 * Synchronously fetches url and returns the raw text
 *
 * @param {string} url
 * @returns Result<String, null>
 */
export function do_fetch(url) {
    try {
        const response = fetch(url, {
            headers: { "User-Agent": "sync-fetch go-over Gleam" },
        })

        if (response.ok) {
            return new Ok(response.text())
        }
    } catch {
        return new Error(null)
    }

    return new Error(null)
}

/**
 * Parses mirego/elixir-security-advisories advisory yaml
 * files and returns these fields in an array
 * - id: string
 * - package: string
 * - severity: string
 * - title: string
 * - vulnerable_version_ranges: List<string>
 *
 * @param {string} content
 * @returns Result<List<String | List<String>>, null>
 */
export function parse_adv(content) {
    const {
        id,
        package: pkg,
        severity,
        title,
        vulnerable_version_ranges,
    } = parse_yaml(content)

    return new Ok([id, pkg, severity, title, toList(vulnerable_version_ranges)])
}
