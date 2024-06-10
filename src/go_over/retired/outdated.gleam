@target(erlang)
import gleam/hackney
import gleam/hexpm.{type ReleaseRetirement}
@target(erlang)
import gleam/http/request
import gleam/json
import gleam/option.{type Option}
import go_over/packages.{type Package}
import go_over/retired/core
import go_over/util/cache
import go_over/util/constants
import go_over/util/print
import go_over/util/util.{hard_fail}
import simplifile
