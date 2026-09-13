# schema_contract.R
# ::rtemis.core::
# 2026- EDG rtemis.org

# Enforces the input-schema contract at the point the artifact is produced.
# Schema producers call `assert_config_contract()` on each config schema and
# `assert_description_language()` on every published schema before writing it.
#
# The contract:
#
#   A schema states what is true of the data. It never states what any
#   interface chooses to fill in.
#
# Five rules follow:
#
# 1. No top-level `required` beyond the key that gives the document its shape:
#    a family dispatcher's discriminator, which says which variant's schema
#    applies to its siblings. A config is otherwise a partial expression of
#    intent. Nested `required` is untouched:
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
# 4. No description names an R construct. The corpus calls itself
#    language-independent and is read by R, by the Rust CLI, by the browser and
#    by a model that writes no code at all; an R constructor or a namespaced
#    call spends a clause of every reader's attention on a function only one of
#    them can call. Matched by construct rather than by taste -- see
#    `.r_specific_prose()` for what counts.
# 5. No description spells a value the way R does. `NULL`, `TRUE`, `FALSE` and
#    `NA` are not JSON, and `@property` is S7 accessor syntax: a reader told
#    "NULL = unweighted" and writing what it says produces an invalid
#    document. Say what the absent value *means* ("Unset leaves the cases
#    unweighted"), or spell the literal as JSON writes it.
#
# Rules 2 to 5 hold at every depth, so they walk the whole document rather
# than its top level: a nullable `$ref` is emitted as a `oneOf`, and a rule
# that stopped at `properties` and `items` would not see inside one.
#
# Rules 1 to 3 are about what a *config* may demand, and records are exempt
# from them: a record asserts what ran, so every field is required and
# `required` is set wholesale by `S7_to_JSONSchema(record =)`. Rules 4 and 5
# are about prose, which every published document has, so they are also
# reachable on their own through `assert_description_language()` -- the entry
# point for a record or a result class, neither of which goes through
# `assert_config_contract()`.

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
  .offending_descriptions(
    schema,
    "setup_[A-Za-z0-9_]+|[A-Za-z0-9.]+::[A-Za-z0-9_.]+|[.]list_to_[A-Za-z0-9_]+"
  )
} # /.r_specific_prose


# %% .r_literal_prose ----
# Descriptions that spell a value the way R does, by path.
#
# Rule 5, and the one a writer breaks without noticing, because the R word for
# the value is right there in the property's own `@param`: "NULL = unweighted"
# is correct roxygen and an invalid instruction to everyone else, who must
# write `null`. The same holds for `TRUE`/`FALSE` against `true`/`false`, for
# `NA` against a JSON document that has no such literal at all, and for the
# `@property` accessor, which is S7 syntax for a key the document spells bare.
#
# Word-bounded so a value is matched and a sentence is not: "unset" and
# "nullable" do not match, and `NA` does not match inside "N/A" or a longer
# word. The remedy is almost always to say what the absent value means rather
# than to transliterate the literal.
# Email addresses with a dotted domain are excluded before matching. Bare
# `@name` and `object@name` still count as accessor syntax.
.r_literal_prose <- function(schema) {
  .offending_descriptions(
    schema,
    "\\bNULL\\b|\\bTRUE\\b|\\bFALSE\\b|\\bNA\\b|@[A-Za-z_][A-Za-z0-9_]*",
    exclude_pattern = "[[:alnum:]._%+-]+@[[:alnum:]-]+([.][[:alnum:]-]+)+"
  )
} # /.r_literal_prose


# %% .offending_descriptions ----
# Every description in the document matching `pattern`, reported as
# "path (first match)" so the message names both where and what.
# Spans matching `exclude_pattern`, if supplied, are replaced with spaces.
.offending_descriptions <- function(schema, pattern, exclude_pattern = NULL) {
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
    if (!is.null(exclude_pattern)) {
      text <- gsub(exclude_pattern, " ", text)
    }
    hit <- regmatches(text, regexpr(pattern, text))
    if (length(hit) == 1L && nzchar(hit)) {
      offenders <- c(
        offenders,
        paste0(.fmt_path(entry[["path"]]), " (", hit, ")")
      )
    }
  }
  offenders
} # /.offending_descriptions


# %% .description_language_problems ----
# Rules 4 and 5 as message strings: the two that are about prose rather than
# about what a document demands, so they hold for a record and a result class
# as much as for a config.
.description_language_problems <- function(schema) {
  problems <- character()

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

  r_literals <- .r_literal_prose(schema)
  if (length(r_literals) > 0L) {
    problems <- c(
      problems,
      paste0(
        "spells a value the way R does, in a description at: ",
        paste(r_literals, collapse = ", "),
        ". A reader who writes what the description says produces an invalid ",
        "document: JSON has `null`, `true` and `false`, no `NA`, and no ",
        "`@property` accessor. Say what the absent value means -- \"Unset ",
        "leaves the cases unweighted\" -- and leave the R spelling to the ",
        "roxygen `@param`, where it is correct."
      )
    )
  }

  problems
} # /.description_language_problems


# %% assert_description_language ----
#' Assert a published schema's descriptions are language-independent
#'
#' @description
#' The two prose rules of the input-schema contract, on their own: no
#' description may name an R construct, and none may spell a value the way R
#' does. `assert_config_contract()` applies both along with the rules about
#' what a config may demand; this is the entry point for a document those do
#' not govern -- a record, or a result class whose `required` states what
#' rtemis always writes.
#'
#' @details
#' Every published document is read by R, by the Rust CLI, by the browser and
#' by a model that writes no code at all, so its prose is part of the interface
#' rather than a comment on it. A description reading "NULL = unweighted" is
#' correct roxygen and an invalid instruction to every reader but one, who must
#' write `null`; the fix is to say what the absent value means, not to
#' transliterate the literal.
#'
#' @param schema Named list: The generated schema, as `S7_to_JSONSchema()` or
#' `S7_dispatcher_JSONSchema()` returns it.
#' @param id Character: The schema's `$id`, used to name it in the error.
#'
#' @return The `schema`, invisibly, so it can wrap a write call. Throws with
#'   class `simpleError` naming every offending description, so one run reports
#'   all of them rather than the first.
#'
#' @author EDG
#' @export
#' @examples
#' assert_description_language(
#'   list(
#'     type = "object",
#'     properties = list(k = list(type = "integer", description = "Clusters."))
#'   ),
#'   "https://schema.rtemis.org/example/v1/schema.json"
#' )
assert_description_language <- function(schema, id = schema[["$id"]]) {
  problems <- .description_language_problems(schema)
  if (length(problems) > 0L) {
    stop(
      "Published-description contract violated by ",
      id,
      ":\n  - ",
      paste(problems, collapse = "\n  - "),
      "\nSee ?rtemis.core::assert_description_language.",
      call. = FALSE
    )
  }
  invisible(schema)
} # /assert_description_language


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
#' Five rules, each recorded where it is raised:
#'
#' - No top-level `required` beyond the key carrying the document's shape:
#'   a family dispatcher's discriminator, which selects the variant whose
#'   settings are its siblings. A config is otherwise a partial expression of
#'   intent.
#' - No `default`: defaults are versioned separately, in `defaults/v1`.
#' - No conditional demand for a key (`then`/`else` with `required`, or
#'   `dependentRequired`): an implementation could satisfy it by filling a
#'   value, which makes it a resolution rule and belongs to the record form.
#' - No R construct named in a description: the corpus is language-independent
#'   and is read by R, by the Rust CLI, by the browser, and by a model that
#'   writes no code at all.
#' - No R spelling of a value in a description: `NULL`, `TRUE`, `FALSE`, `NA`
#'   and the `@property` accessor are R, not JSON.
#'
#' The first three are about what a config may demand, and record schemas are
#' not subject to them: a record states what a run used, so everything in it is
#' required. The last two are about prose, which every published document has;
#' `assert_description_language()` applies just those, for a record or a result
#' class that does not come through here.
#'
#' @param schema Named list: The generated schema, as `S7_to_JSONSchema()` or
#' `S7_dispatcher_JSONSchema()` returns it.
#' @param id Character: The schema's `$id`, used to name it in the error.
#' @param structural Character: Keys this schema may require because they carry
#' the document's shape rather than a value -- a family dispatcher's
#' discriminator. Empty for a leaf or a flat config.
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

  # `$schema` identifies the document rather than stating anything about it, so
  # requiring it demands nothing an implementation could fill in. No generator
  # puts it in `required` -- both `S7_to_JSONSchema()` and the dispatcher strip
  # it -- so this tolerates a hand-written schema rather than anything emitted.
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
        ". A config is partial by nature; only a dispatcher's discriminator ",
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

  problems <- c(problems, .description_language_problems(schema))

  if (length(problems) > 0L) {
    stop(
      "Input-schema contract violated by ",
      id,
      ":\n  - ",
      paste(problems, collapse = "\n  - "),
      "\nSee ?rtemis.core::assert_config_contract.",
      call. = FALSE
    )
  }
  invisible(schema)
} # /assert_config_contract
