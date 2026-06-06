import gleam/list
import gleam/option
import gleam/pair
import go_over/advisories/advisories
import go_over/config.{type Config}
import go_over/hex/hex
import go_over/hex/retired
import go_over/packages.{type Package}
import go_over/warning.{type Warning}
import gxyz/list as glist
import gxyz/tuple

pub fn get_vulnerable_warnings(
  pkgs: List(Package),
  conf: Config,
  all_advisories: List(advisories.Advisory),
) -> List(Warning) {
  advisories.check_for_advisories(pkgs, all_advisories)
  |> list.map(fn(p) { tuple.map2_1(p, config.filter_advisory_ids(conf, _)) })
  |> glist.filter_tap(pair.second, list.is_empty)
  |> list.flat_map(tuple.apply_from2(_, warning.adv_to_warning))
}

pub fn get_retired_warnings(
  pkgs: List(Package),
  conf: Config,
) -> List(Warning) {
  pkgs
  |> list.map(fn(pkg) {
    retired.check_retired(conf.puller, pkg)
    |> option.map(pair.new(pkg, _))
  })
  |> option.values()
  |> list.map(tuple.apply_from2(_, warning.retired_to_warning))
}

pub fn get_hex_warnings(
  pkgs: List(Package),
  conf: Config,
) -> #(List(Warning), List(String)) {
  let allowed_licenses = conf.allowed_licenses

  list.fold(pkgs, #([], []), fn(acc, pkg) {
    let #(warnings, licenses) = acc
    let pkg_licenses = hex.package_licenses(conf.puller, pkg)
    let pkg_warnings =
      hex.rejected_license_sources(pkg_licenses, allowed_licenses)
      |> list.map(fn(source) {
        case source {
          hex.RejectedLicense(name) ->
            warning.rejected_license_to_warning(pkg, name)
        }
      })

    #(list.append(warnings, pkg_warnings), list.append(licenses, pkg_licenses))
  })
}
