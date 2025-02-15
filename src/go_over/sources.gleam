import gleam/list
import gleam/option.{None, Some}
import gleam/pair
import go_over/advisories/advisories
import go_over/config.{type Config, Config}
import go_over/packages.{type Package}
import go_over/retired/hex
import go_over/retired/retired
import go_over/warning.{type Warning, Warning}
import gxyz/list as glist
import gxyz/tuple

pub fn get_vulnerable_warnings(
  pkgs: List(Package),
  conf: Config,
) -> List(Warning) {
  advisories.check_for_advisories(pkgs, conf.force, conf.verbose, conf.global)
  |> list.map(fn(p) { tuple.map2_1(p, config.filter_advisory_ids(conf, _)) })
  |> glist.filter_tap(pair.second, list.is_empty)
  |> list.flat_map(tuple.apply_from2(_, warning.adv_to_warning))
}

pub fn get_retired_warnings(pkgs: List(Package), conf: Config) -> List(Warning) {
  pkgs
  |> list.map(fn(pkg) {
    retired.check_retired(pkg, conf.force, conf.verbose, conf.global)
    |> option.map(pair.new(pkg, _))
  })
  |> option.values()
  |> list.map(tuple.apply_from2(_, warning.retired_to_warning))
}

pub fn get_hex_warnings(pkgs: List(Package), conf: Config) -> List(Warning) {
  let check_licenses = list.length(conf.allowed_licenses) > 0
  let outdated = conf.outdated
  let force = conf.force
  let verbose = conf.verbose
  let global = conf.global
  let allowed_licenses = conf.allowed_licenses

  list.flat_map(pkgs, fn(pkg) {
    let sources =
      hex.get_hex_info(pkg, force, verbose, global, allowed_licenses)

    list.map(sources, fn(source) {
      case source, outdated, check_licenses {
        hex.Outdated(new_version), True, _ ->
          Some(warning.outdated_to_warning(pkg, new_version))

        hex.RejectedLicense(name), _, True ->
          Some(warning.rejected_license_to_warning(pkg, name))

        _, _, _ -> None
      }
    })
  })
  |> option.values()
}
