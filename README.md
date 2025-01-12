# 🕵️‍♂️ go_over

[![Package Version](https://img.shields.io/hexpm/v/go_over)](https://hex.pm/packages/go_over)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/go_over/)
[![mit](https://img.shields.io/github/license/bwireman/go-over?color=brightgreen)](https://github.com/bwireman/over/blob/main/LICENSE)
[![gleam js](https://img.shields.io/badge/%20gleam%20%E2%9C%A8-js%20%F0%9F%8C%B8-yellow)](https://gleam.run/news/v0.16-gleam-compiles-to-javascript/)
[![gleam erlang](https://img.shields.io/badge/erlang%20%E2%98%8E%EF%B8%8F-red?style=flat&label=gleam%20%E2%9C%A8)](https://gleam.run)

A tool to audit Erlang & Elixir dependencies, to make sure your ✨ gleam
projects really sparkle!

🚨 _**NOTE**_: security advisories are _NOT_ currently monitored for gleam
dependencies. The language, while excellent, is far too new and niche

# 🔽 Install

```sh
gleam add --dev go_over
```

## 📣 Also!

- add `.go-over/` to your `.gitignore`
- make sure `git` & `curl` are installed

#### 🌸 Javascript

If running with Javascript install

```json
{
  "devDependencies": {
    "yaml": "^2.4.3"
  }
}
```

Bun, Deno & Nodejs are _all_ supported!

# ▶️ Usage

```sh
gleam run -m go_over
```

### 🎥 Obligatory VHS

![demo](https://raw.githubusercontent.com/bwireman/go-over/main/images/demo.gif)

### 🏴 Flags

- `--format` FORMAT: Specify the output format of any warnings, [minimal,
  verbose, json] (default: None)
- `--force`: Force pulling new data even if the cached data is still valid
- `--outdated`: Additionally check if newer versions of dependencies exist
- `--ignore-indirect`: Ignore all warnings for indirect dependencies
- `--verbose`: Print progress as packages are checked
- `--help`,-h: Print help

Flags override config values if set

### ⚙️ Config

Optional settings that can be added to your project's `gleam.toml`

```toml
[go-over]
# disables caching if false
# default: true
cache = true
# if true all cached data will be stored in user's home directory
# allowing cache to be shared between projects
# default: false
global = false
# sets output format for warnings ["minimal", "detailed", "json"]
# default: "minimal"
format = "minimal"
# will additionally check if newer versions of dependencies exist
# default: false
outdated = false

[go-over.ignore]
# will ignore all warnings for indirect dependencies
# default: false
indirect = false
# will ignore all warnings for dev-dependencies. Note: to ignore indirect dependencies regardless of source see go-over.ignore.indirect
# default: false
dev_dependencies = false
# list of package names to skip when auditing dependencies
# default: []
packages = ["example_package"]
# list of warning severities to skip when auditing dependencies
# default: []
# (case insensitive)
severity = ["example_moderate"]
# list of advisory IDs to skip when auditing dependencies
# default: []
ids = ["GHSA-xxxx-yyyy-zzzz"]
```

### ⌛ Caching

- Security advisory data is cached for **_six_** hours
- hex.pm retired package data is cached for **_one_** hour

## 🪝 pre-commit hooks

You can add go_over to you're pre-commit hooks by installing
[🌵cactus](https://hex.pm/packages/cactus) & then adding this to your
`gleam.toml`

```toml
[cactus.pre-commit]
actions = [
    { command = "go_over", kind = "module", args=["--outdated"] },
]
```

## ⚙️ CI

You can also schedule daily runs to keep your deps up to date and open issues
when necessary!
[Example ▶️](https://github.com/bwireman/go-over/blob/main/.github/workflows/deps.yml)

# 🖌️ Other Art

- As I'm sure is no surprise this tool is inspired by (and all around worse
  than) [mirego/mix_audit](https://github.com/mirego/mix_audit). Please check it
  out!
- It also draws inspiration from
  [mix hex.audit](https://hexdocs.pm/hex/Mix.Tasks.Hex.Audit.html)

# ⚖️ License

- This tool uses
  [mirego/elixir-security-advisories](https://github.com/mirego/elixir-security-advisories)
  which is it self licensed with

  - `BSD-3-Clause license`
  - `CC-BY 4.0 open source license`.
  - See their
    [#license section](https://github.com/mirego/elixir-security-advisories?tab=readme-ov-file#license)

- Code original to this repo is Licensed under `MIT`
