import { parse as parse_yaml } from "yaml"
import { toList, Ok, Error } from "../prelude.mjs"

const string = "string"
const err = new Error(null)

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
            return err
        }

        if (typeof id !== string) {
            return err
        }

        if (typeof pkg !== string) {
            return err
        }

        if (typeof severity !== string) {
            return err
        }

        if (typeof title !== string) {
            return err
        }

        if (!Array.isArray(vulnerable_version_ranges)) {
            return err
        }

        for (const element of vulnerable_version_ranges) {
            if (typeof element !== string) {
                return err
            }
        }

        return new Ok([
            id,
            pkg,
            severity,
            title,
            toList(vulnerable_version_ranges),
        ])
    } catch {
        return err
    }
}
