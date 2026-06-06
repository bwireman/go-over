import filepath
import gleam/json.{type Json, int, object, preprocessed_array, string}
import gleam/list
import gleam/option
import go_over/util/constants
import go_over/warning.{
  type Severity, type Warning, SeverityCritical, SeverityHigh, SeverityInfo,
  SeverityLow, SeverityModerate, SeverityPackageRetiredDeprecated,
  SeverityPackageRetiredInvalid, SeverityPackageRetiredOtherReason,
  SeverityPackageRetiredRenamed, SeverityPackageRetiredSecurity,
  SeverityRejectedLicense, SeverityUnknown, WarningReasonInfo,
  WarningReasonRejectedLicense, WarningReasonRetired, WarningReasonVulnerable,
  is_info, severity_as_string,
}

const schema = "https://json.schemastore.org/sarif-2.1.0.json"

pub fn to_sarif_log(runs: List(#(String, List(Warning)))) -> Json {
  object([
    #("$schema", string(schema)),
    #("version", string("2.1.0")),
    #(
      "runs",
      runs
        |> list.map(fn(run) {
          let #(project_root, warnings) = run
          to_sarif_run(project_root, warnings)
        })
        |> preprocessed_array(),
    ),
  ])
}

pub fn to_sarif_run(project_root: String, warnings: List(Warning)) -> Json {
  let warnings = list.filter(warnings, fn(w) { !is_info(w) })
  let rules = build_rules(warnings)
  let results = list.map(warnings, fn(w) { to_result(w, project_root) })

  object([
    #(
      "tool",
      object([
        #(
          "driver",
          object([
            #("name", string("go_over")),
            #("version", string(constants.version)),
            #("rules", rules),
          ]),
        ),
      ]),
    ),
    #("results", preprocessed_array(results)),
  ])
}

fn build_rules(warnings: List(Warning)) -> Json {
  warnings
  |> list.map(rule_id)
  |> list.unique()
  |> list.map(fn(id) {
    object([
      #("id", string(id)),
      #("shortDescription", object([#("text", string(id))])),
    ])
  })
  |> preprocessed_array()
}

fn to_result(w: Warning, project_root: String) -> Json {
  let manifest_uri = filepath.join(project_root, "manifest.toml")

  object([
    #("ruleId", string(rule_id(w))),
    #("level", string(sarif_level(w.severity))),
    #(
      "message",
      object([
        #(
          "text",
          string(
            w.package
            <> option.map(w.version, fn(v) { "@" <> v })
            |> option.unwrap("")
            <> ": "
            <> w.reason,
          ),
        ),
      ]),
    ),
    #(
      "locations",
      preprocessed_array([
        object([
          #(
            "physicalLocation",
            object([
              #("artifactLocation", object([#("uri", string(manifest_uri))])),
              #(
                "region",
                object([
                  #("startLine", int(1)),
                  #("startColumn", int(1)),
                ]),
              ),
            ]),
          ),
        ]),
      ]),
    ),
  ])
}

fn rule_id(w: Warning) -> String {
  case w.advisory_id {
    option.Some(id) -> id
    option.None ->
      case w.warning_reason_code {
        WarningReasonVulnerable -> "go-over/vulnerable"
        WarningReasonRetired -> "go-over/retired"
        WarningReasonRejectedLicense(_) -> "go-over/rejected-license"
        WarningReasonInfo -> "go-over/info"
      }
      <> ":"
      <> severity_as_string(w.severity)
  }
}

fn sarif_level(severity: Severity) -> String {
  case severity {
    SeverityCritical
    | SeverityHigh
    | SeverityPackageRetiredSecurity
    | SeverityRejectedLicense -> "error"
    SeverityModerate
    | SeverityPackageRetiredRenamed
    | SeverityPackageRetiredDeprecated -> "warning"
    SeverityLow
    | SeverityPackageRetiredInvalid
    | SeverityPackageRetiredOtherReason(_)
    | SeverityInfo
    | SeverityUnknown(_) -> "note"
  }
}
