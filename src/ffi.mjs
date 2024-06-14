import { parse as parse_yaml } from "yaml"
import { toList, Ok, Error } from "../prelude.mjs"

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
    try {
        const {
            id,
            package: pkg,
            severity,
            title,
            vulnerable_version_ranges,
        } = parse_yaml(content)

        if (!id || !pkg || !severity || !title || !vulnerable_version_ranges) {
            return new Error(null)
        }

        return new Ok([
            id,
            pkg,
            severity,
            title,
            toList(vulnerable_version_ranges),
        ])
    } catch {
        return new Error(null)
    }
}
