# schema_contract.R
# ::rtemis.core::
# 2026- EDG rtemis.org

# Enforces the input-schema contract at the point the artifact is produced.
# Sourced by `generate_schemas.R`, which calls `assert_config_contract()` on
# every config (non-record) schema before writing it, so a violating schema
# cannot be generated.
#
# The contract (see `plan/schema-interface-boundary.md`, `ARCHITECTURE.md`):
#
#   A schema states what is true of the data. It never states what any
#   interface chooses to fill in.
#
# Three rules follow:
#
# 1. No top-level `required` beyond the keys that give the document its shape:
#    the discriminator that says which schema applies and, for a family that
#    nests its variant fields, the payload key that holds them. A config is
#    otherwise a partial expression of intent. Nested `required` is untouched:
#    a nested object is either a table row or an `origin` block, whose members
#    are structural, or a `$ref` to a schema asserted in its own right.
# 2. No conditional branch may introduce a `required`. A `then` or `else` may
#    constrain a *value* (`n_workers: {const: 1}` when there is no backend,
#    which is incoherent under any API) but may not demand a *key* that an
#    implementation could supply. The test for a candidate clause: could an
#    implementation satisfy it by filling in a value? If so, it is a resolution
#    rule and belongs in the record form only. `dependentRequired` is the same
#    demand in another spelling and is checked with it.
#
#    Note the asymmetry with a clause's `if`: an `if` uses `required` to
#    *scope* the condition ("when `backend` is present and equals 'none'"),
#    which is not a demand on the document. Only `then` and `else` are checked.
# 3. No subschema carries a `default`. Defaults are API policy and belong in
#    `defaults/v1/defaults.json`, which is versioned independently; emitting
#    one here would version-couple an artifact that is immutable once
#    published.
#
# Rules 2 and 3 hold at every depth, so they walk the whole document rather
# than its top level: a nullable `$ref` is emitted as a `oneOf`, and a rule
# that stopped at `properties` and `items` would not see inside one.
#
# Records are exempt from all three: a record asserts what ran, so every field
# is required and `required` is set wholesale by `S7_to_JSONSchema(record =)`.

# %% Subschema keywords ----
# Where a JSON Schema node holds further schemas: one directly, a list of
# them, or a name -> schema map.
SUBSCHEMA_KEYWORDS <- c(
  "items",
  "additionalItems",
  "contains",
  "additionalProperties",
  "unevaluatedProperties",
  "unevaluatedItems",
  "propertyNames",
  "not",
  "if",
  "then",
  "else"
)
SUBSCHEMA_LIST_KEYWORDS <- c("allOf", "anyOf", "oneOf", "prefixItems")
SUBSCHEMA_MAP_KEYWORDS <- c(
  "properties",
  "patternProperties",
  "dependentSchemas",
  "$defs",
  "definitions"
)


# %% .subschemas ----
# Every schema node reachable from `node`, itself included, as a list of
# `list(path, node)`. `path` is the chain of keys taken to reach the node, so
# the last element names the keyword a node was found under -- which is how
# rule 2 tells a `then` from the `if` beside it.
.subschemas <- function(node, path = character()) {
  if (!is.list(node)) {
    return(list())
  }
  out <- list(list(path = path, node = node))
  for (kw in SUBSCHEMA_KEYWORDS) {
    out <- c(out, .subschemas(node[[kw]], c(path, kw)))
  }
  for (kw in SUBSCHEMA_LIST_KEYWORDS) {
    children <- node[[kw]]
    if (is.list(children)) {
      for (i in seq_along(children)) {
        out <- c(
          out,
          .subschemas(children[[i]], c(path, paste0(kw, "[", i, "]")))
        )
      }
    }
  }
  for (kw in SUBSCHEMA_MAP_KEYWORDS) {
    children <- node[[kw]]
    if (is.list(children)) {
      for (nm in names(children)) {
        out <- c(out, .subschemas(children[[nm]], c(path, nm)))
      }
    }
  }
  out
} # /.subschemas


# %% .fmt_path ----
# A subschema's path as a readable location, "<root>" for the document itself.
.fmt_path <- function(path) {
  if (length(path) == 0L) "<root>" else paste(path, collapse = ".")
} # /.fmt_path


# %% .conditional_demands ----
# Locations where a conditional branch demands a key: "path (key, key)" for
# each `then` / `else` carrying a `required`, and for each `dependentRequired`.
.conditional_demands <- function(schema) {
  out <- character()
  for (sub in .subschemas(schema)) {
    path <- sub[["path"]]
    node <- sub[["node"]]
    branch <- length(path) > 0L && path[[length(path)]] %in% c("then", "else")
    req <- if (branch) as.character(node[["required"]]) else character()
    dep <- unlist(lapply(node[["dependentRequired"]], as.character))
    demanded <- unique(c(req, as.character(dep)))
    if (length(demanded) > 0L) {
      out <- c(
        out,
        paste0(.fmt_path(path), " (", paste(demanded, collapse = ", "), ")")
      )
    }
  }
  out
} # /.conditional_demands


# %% .defaulted_subschemas ----
# Locations of every subschema carrying a `default` keyword.
.defaulted_subschemas <- function(schema) {
  out <- character()
  for (sub in .subschemas(schema)) {
    if ("default" %in% names(sub[["node"]])) {
      out <- c(out, .fmt_path(sub[["path"]]))
    }
  }
  out
} # /.defaulted_subschemas


# %% .r_specific_prose ----
# Descriptions that name an R construct, by path.
#
# The corpus calls itself language-independent and is read by R, by the Rust
# CLI, by the browser, and by a model that writes no code at all. A description
# ending "See `setup_GLMNET`." spends a clause of every reader's attention on a
# function only one of them can call -- and the agent listing, which shows 27 of
# them at once, spent 27.
#
# Matched narrowly and by construct, not by taste: an R constructor
# (`setup_X`), a namespaced call (`pkg::fn`), and an internal dot-function
# (`.list_to_X`). Prose that merely mentions a package by name -- "Elastic net
# (glmnet)" -- says what the algorithm *is* and stays.
.r_specific_prose <- function(schema) {
  offenders <- character()
  for (entry in .subschemas(schema)) {
    node <- entry[["node"]]
    if (!is.list(node)) {
      next
    }
    text <- node[["description"]]
    if (!is.character(text) || length(text) != 1L) {
      next
    }
    hit <- regmatches(
      text,
      regexpr(
        "setup_[A-Za-z0-9_]+|[A-Za-z0-9.]+::[A-Za-z0-9_.]+|[.]list_to_[A-Za-z0-9_]+",
        text
      )
    )
    if (length(hit) == 1L && nzchar(hit)) {
      offenders <- c(
        offenders,
        paste0(.fmt_path(entry[["path"]]), " (", hit, ")")
      )
    }
  }
  offenders
} # /.r_specific_prose


# %% assert_config_contract ----
#' Assert a generated config schema honors the input-schema contract
#'
#' @description
#' Checks one generated JSON Schema against the rules a *config* document must
#' obey, and throws if any is broken. Shared by every package that publishes to
#' schema.rtemis.org -- rtemis and rtemis.draw -- so one registry cannot hold
#' documents held to two standards.
#'
#' @details
#' Four rules, each recorded where it is raised:
#'
#' - No top-level `required` beyond the keys carrying the document's shape:
#'   the discriminator and, where a family nests its variant fields, the
#'   payload key holding them. A config is otherwise a partial expression of
#'   intent.
#' - No `default`: defaults are versioned separately, in `defaults/v1`.
#' - No conditional demand for a key (`then`/`else` with `required`, or
#'   `dependentRequired`): an implementation could satisfy it by filling a
#'   value, which makes it a resolution rule and belongs to the record form.
#' - No R construct named in a description: the corpus is language-independent
#'   and is read by R, by the Rust CLI, by the browser, and by a model that
#'   writes no code at all.
#'
#' Record schemas are not subject to this: a record states what a run used, so
#' everything in it is required and the config's rules do not apply.
#'
#' @param schema Named list: The generated schema, as `S7_to_JSONSchema()` or
#' `S7_dispatcher_JSONSchema()` returns it.
#' @param id Character: The schema's `$id`, used to name it in the error.
#' @param structural Character: Keys this schema may require because they carry
#' the document's shape rather than a value -- the discriminator and, where the
#' variant's fields are nested, the payload key holding them. Empty for a leaf
#' or a flat config.
#'
#' @return The `schema`, invisibly, so it can wrap a write call. Throws with
#'   class `simpleError` listing every rule broken, so one run reports all of
#'   them rather than the first.
#'
#' @author EDG
#' @export
#' @examples
#' assert_config_contract(
#'   list(type = "object", properties = list(k = list(type = "integer"))),
#'   "https://schema.rtemis.org/example/v1/schema.json"
#' )
assert_config_contract <- function(
  schema,
  id = schema[["$id"]],
  structural = character()
) {
  problems <- character()

  stray <- setdiff(
    as.character(schema[["required"]]),
    c("$schema", structural)
  )
  if (length(stray) > 0L) {
    problems <- c(
      problems,
      paste0(
        "declares required propert",
        if (length(stray) == 1L) "y: " else "ies: ",
        paste(stray, collapse = ", "),
        ". A config is partial by nature; only the discriminator and payload ",
        "may be required."
      )
    )
  }

  demanded <- .conditional_demands(schema)
  if (length(demanded) > 0L) {
    problems <- c(
      problems,
      paste0(
        "conditionally demands a key at: ",
        paste(demanded, collapse = "; "),
        ". A `then` may constrain a value but may not demand a key an ",
        "implementation could supply -- that is a resolution rule, and it ",
        "belongs in the record form only."
      )
    )
  }

  defaulted <- .defaulted_subschemas(schema)
  if (length(defaulted) > 0L) {
    problems <- c(
      problems,
      paste0(
        "emits a `default` at: ",
        paste(defaulted, collapse = ", "),
        ". Defaults belong in defaults/v1/defaults.json, which is versioned ",
        "independently of the schemas."
      )
    )
  }

  r_prose <- .r_specific_prose(schema)
  if (length(r_prose) > 0L) {
    problems <- c(
      problems,
      paste0(
        "names an R construct in a description at: ",
        paste(r_prose, collapse = ", "),
        ". The corpus is language-independent; an R constructor or function ",
        "belongs in the roxygen docs, not in a document the CLI, the browser ",
        "and a model all read."
      )
    )
  }

  if (length(problems) > 0L) {
    stop(
      "Input-schema contract violated by ",
      id,
      ":\n  - ",
      paste(problems, collapse = "\n  - "),
      "\nSee plan/schema-interface-boundary.md.",
      call. = FALSE
    )
  }
  invisible(schema)
} # /assert_config_contract
