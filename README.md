# 🕵️‍♂️ go_over

[![Package Version](https://img.shields.io/hexpm/v/go_over)](https://hex.pm/packages/go_over)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/go_over/)

A tool to audit Erlang & Elixir dependencies, to make sure your gleam projects really sparkle! ✨

🚨 _**NOTE**_: security advisories are _NOT_ currently monitored for gleam dependencies. The language, while excellent, is far too new and niche

# 🔽 Install

```sh
gleam add --dev go_over
```

## 📣 Also!

- add `.go-over/` to your `.gitignore`
- make sure `git` is installed

# ▶️ Usage

```sh
gleam run -m go_over
```

### 🎥 Obligatory Asciinema

![demo](https://raw.githubusercontent.com/bwireman/go-over/main/images/demo.gif)

### 🏴 Flags

- `--skip`: will skip checking the cache and used the stored data no matter what
- `--force`: will force pulling new data even if the cached data is still valid

### ⚙️ Config

Optional settings that can be added to your project's `gleam.toml`

```toml
[go-over]
# disables caching if false
# default: true
cache = true
# sets output format ("minimal", "detailed")
# default: "minimal"
format = "minimal"

[go-over.ignore]
# list of package names to skip when checking for advisories & warnings
# default: []
packages = ["example_package"]
# list of warning severities to skip when checking for advisories & warnings
# default: []
# (case insensitive)
severity = ["example_moderate"]
# list of advisory IDs to skip when checking for advisories & warnings
# default: []
ids = ["GHSA-xxxx-yyyy-zzzz"]
```

### ⌛ Caching

- Security advisory data is cached for **_six_** hours
- hex.pm retired package data is cached for **_one_** hour

# 🖌️ Other Art

- As I'm sure is no surprise this tool is inspired by (and all around worse than) [mirego/mix_audit](https://github.com/mirego/mix_audit). Please check it out!
- It also draws inspiration from [mix hex.audit](https://hexdocs.pm/hex/Mix.Tasks.Hex.Audit.html)

# ⚖️ License

- This tool uses [mirego/elixir-security-advisories](https://github.com/mirego/elixir-security-advisories) which is it self licensed with

  - `BSD-3-Clause license`
  - `CC-BY 4.0 open source license`.
  - See their [#license section](https://github.com/mirego/elixir-security-advisories?tab=readme-ov-file#license)

- Code original to this repo is Licensed under `MIT`
