import { parse as parse_yaml } from "yaml";
import { Error, Ok, toList } from "../prelude.mjs";

const string_type = "string";
const err = new Error(null);

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
    } = parse_yaml(content);

    if (!id || !pkg || !severity || !title || !vulnerable_version_ranges) {
      return err;
    }

    if (typeof id !== string_type) {
      return err;
    }

    if (typeof pkg !== string_type) {
      return err;
    }

    if (typeof severity !== string_type) {
      return err;
    }

    if (typeof title !== string_type) {
      return err;
    }

    if (!Array.isArray(vulnerable_version_ranges)) {
      return err;
    }

    for (const element of vulnerable_version_ranges) {
      if (typeof element !== string_type) {
        return err;
      }
    }

    return new Ok([
      id,
      pkg,
      severity,
      title,
      toList(vulnerable_version_ranges),
    ]);
  } catch {
    return err;
  }
}
