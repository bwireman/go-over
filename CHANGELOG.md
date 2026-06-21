# Changelog

## 4.0.0-rc2

### Fixed

- Vulnerability warnings were never emitted: `get_vulnerable_warnings` kept
  packages with empty advisory lists instead of those with matches

## 4.0.0

### Added

- `--format sarif` and `--sarif-output PATH` for GitHub Code Scanning
  integration
- `--workspace [PATH]` to audit every Gleam project under a directory
- `--root PATH` to audit a single project outside the current directory
- `workspace_max_depth` config option (default: `3`) for workspace discovery
- Info-level warnings for unnecessary ignore rules, git dependencies, and
  workspace projects skipped due to depth limits
- `--local` and `--global` flags (previously config-only)

### Changed

- Built-in outdated dependency checks removed from hex.pm metadata; `--outdated`
  and `[go-over] outdated` now run `gleam deps outdated` instead (default:
  `false`)
- `cache` config replaced by `force` (`cache = false` in v3 meant always
  refresh; use `force = true` in v4)
- `--ignore-indirect` CLI flag removed; use `[go-over.ignore] indirect = true`
- CLI `--format` overrides per-project `[go-over] format` in workspace mode
- Advisories repository is cloned once per audit instead of per check
- Default `global` cache behavior unchanged (`true`)
