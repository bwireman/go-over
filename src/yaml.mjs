import { parse } from 'yaml'
import { List } from "../prelude.mjs"

export function readADV(contents) {
    let { package: package_name, first_patched_versions, vulnerable_version_ranges } = parse(contents)
    return { package: package_name, first_patched_versions, vulnerable_version_ranges }
}

export function name({ package: package_name, first_patched_versions, vulnerable_version_ranges }) {
    return package_name
}

export function first_patched_versions({ package: package_name, first_patched_versions, vulnerable_version_ranges }) {
    return List.fromArray(first_patched_versions)
}

export function vulnerable_version_ranges({ package: package_name, first_patched_versions, vulnerable_version_ranges }) {
    return List.fromArray(vulnerable_version_ranges)
}
