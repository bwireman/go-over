import { parse as parse_yaml } from 'yaml'
import { toList } from '../prelude.mjs'

export function parse(content) {
    const { id,
        package: pkg,
        severity,
        title,
        vulnerable_version_ranges,
    } = parse_yaml(content)

    return [
        id,
        pkg,
        severity,
        title,
        toList(vulnerable_version_ranges)
    ]
}