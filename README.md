# 🕵️‍♂️ go_over

[![Package Version](https://img.shields.io/hexpm/v/go_over)](https://hex.pm/packages/go_over)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/go_over/)

A tool to audit Erlang & Elixir dependency advisories as well as retired hex packages, to make sure your gleam projects really sparkle! ✨

🚨 _**NOTE**_: security advisories are _NOT_ currently monitored for gleam dependencies. The language, while excellent, is far too new and niche

# Install

```sh
gleam add --dev go_over
```

## 📣 Also!

- add `.go-over/` to your `.gitignore`
- make sure `git` is installed

# Usage

```sh
gleam run -m go_over
```

### 🎥 Obligatory Asciinema

![demo](https://raw.githubusercontent.com/bwireman/go-over/main/images/demo.gif)

### 🏴 Flags

- `--skip`: will skip checking the cache and used the stored data no matter what
- `--force`: will force pulling new data even if the cached data is still valid

### Caching

- Security advisory data is cached for six hours
- hex.pm retired package data is cached for one hour

# 🖌️ Other Art

- As I'm sure is no surprise this tool is inspired by (and all around worse than) [mirego/mix_audit](https://github.com/mirego/mix_audit). Please check it out!
- It also draws inspiration from [mix hex.audit](https://hexdocs.pm/hex/Mix.Tasks.Hex.Audit.html)

# License

This tool uses [mirego/elixir-security-advisories](https://github.com/mirego/elixir-security-advisories) which is it self licensed with `BSD-3-Clause license` and `CC-BY 4.0 open source license`. See their [#license section](https://github.com/mirego/elixir-security-advisories?tab=readme-ov-file#license)

Code original to this repo is Licensed under MIT
