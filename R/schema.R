# 2026- EDG rtemis.org

# How every JSON Schema published to schema.rtemis.org is written. The registry
# has more than one producer -- rtemis generates the machine-learning families,
# rtemis.draw the chart families -- and a document's shape is a property of the
# registry rather than of whichever package emitted it. One writer here is what
# keeps them from drifting into separate conventions.

# %% SCHEMA_KEYWORDS_FIRST ----
# Placed first, in this order, wherever a schema object appears; everything else
# keeps the order its generator built it in; `$defs` and `definitions` go last.
#
# This is `schemars`'s convention, which the harness crate's Rust generator
# emits natively. Adopting it means the registry's producers write one shape
# without any of them transforming its output to match another's. It fixes these
# nine keywords and no others, which is why a generator's own order still
# decides the rest.
SCHEMA_KEYWORDS_FIRST <- c(
  "$id",
  "$schema",
  "title",
  "description",
  "type",
  "format",
  "properties"
)


# %% SCHEMA_KEYWORDS_LAST ----
SCHEMA_KEYWORDS_LAST <- c("$defs", "definitions")


# %% SCHEMA_MAP_KEYWORDS ----
# Keywords whose value is a map of *names* to schemas rather than a schema. The
# keys are the author's -- property names, pattern strings, definition names --
# so they are never reordered; only the schemas they hold are.
SCHEMA_MAP_KEYWORDS <- c(
  "properties",
  "patternProperties",
  "dependentSchemas",
  "$defs",
  "definitions"
)


# %% order_schema_keywords ----
#' Put a schema's keywords in reading order, at every depth
#'
#' The order keys appear in is insignificant to a validator and significant to
#' a reader, so the registry fixes it. Applied by `write_JSONSchema()` rather than
#' left to each generator, so a producer cannot emit an unordered document by
#' forgetting to call it.
#'
#' @param x Named list: A schema, or any node within one.
#'
#' @return `x`, with the placed keywords first and last at every depth.
#'
#' @author EDG
#' @keywords internal
#' @noRd
order_schema_keywords <- function(x) {
  if (!is.list(x) || is.null(names(x))) {
    return(if (is.list(x)) lapply(x, order_schema_keywords) else x)
  }
  x[] <- lapply(seq_along(x), function(i) {
    if (names(x)[[i]] %in% SCHEMA_MAP_KEYWORDS) {
      lapply(x[[i]], order_schema_keywords)
    } else {
      order_schema_keywords(x[[i]])
    }
  })
  nms <- names(x)
  x[c(
    intersect(SCHEMA_KEYWORDS_FIRST, nms),
    setdiff(nms, c(SCHEMA_KEYWORDS_FIRST, SCHEMA_KEYWORDS_LAST)),
    intersect(SCHEMA_KEYWORDS_LAST, nms)
  )]
} # /rtemis.core::order_schema_keywords


# %% write_JSONSchema ----
#' Write a JSON Schema to file
#'
#' Serializes a schema built as a named list and writes it, with the keywords in
#' the registry's reading order. Every generator that publishes to
#' schema.rtemis.org writes through this, so the documents share one shape.
#'
#' @param schema Named list: The schema.
#' @param file Character: Path to output JSON file.
#' @param overwrite Logical: If TRUE, overwrite an existing file.
#' @param digits Integer or NA: Significant digits for numeric values, passed to
#'   `jsonlite::toJSON()`. `NA` writes each number at full precision as R prints
#'   it. Pass `I(17)` where a value must round-trip an IEEE 754 double exactly:
#'   jsonlite's own default is 4 *decimal places*, which silently rounds, and a
#'   document that is nearly right is worse than one that is obviously wrong.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `schema`, invisibly.
#'
#' @author EDG
#' @export
#' @examplesIf requireNamespace("jsonlite", quietly = TRUE)
#' schema <- list(
#'   `$schema` = "https://json-schema.org/draft/2020-12/schema",
#'   `$id` = "https://example.org/demo/v1/schema.json",
#'   title = "Demo",
#'   type = "object",
#'   properties = list(n = list(type = "integer", minimum = 1L))
#' )
#' tmpfile <- file.path(tempdir(), "demo.schema.json")
#' write_JSONSchema(schema, tmpfile, overwrite = TRUE, verbosity = 0L)
write_JSONSchema <- function(
  schema,
  file,
  overwrite = FALSE,
  digits = NA,
  verbosity = 1L
) {
  check_dependencies("jsonlite")
  json_str <- as.character(jsonlite::toJSON(
    order_schema_keywords(schema),
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null",
    null = "null",
    digits = digits
  ))
  write_lines(
    json_str,
    file = file,
    overwrite = overwrite,
    verbosity = verbosity
  )
  invisible(schema)
} # /rtemis.core::write_JSONSchema


# %% write_lines ----
#' Write lines to file
#'
#' Normalizes path, check if directory exists, creates it if necessary,
#' writes lines to file, and checks if file was created successfully.
#'
#' @param x Character: Text to write to file.
#' @param file Character: Path to output file.
#' @param overwrite Logical: If TRUE, overwrite an existing file.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Invisible NULL. Called for the side effect of writing to file.
#'
#' @author EDG
#' @export
#' @examples
#' tmpfile <- file.path(tempdir(), "demo.txt")
#' write_lines("hello", tmpfile, overwrite = TRUE, verbosity = 0L)
write_lines <- function(x, file, overwrite = FALSE, verbosity = 1L) {
  # Normalize path
  file <- normalizePath(file, mustWork = FALSE)
  # Check if file exists
  if (file.exists(file)) {
    if (overwrite) {
      if (verbosity >= 1L) {
        msg(fmt(
          paste("Overwriting existing file:", file),
          col = rtemis_colors[["orange"]]
        ))
      }
    } else {
      abort(
        "File already exists: ",
        file,
        ". Set `overwrite = TRUE` to overwrite.",
        class = c("rtemis_file_exists", "rtemis_io_error")
      )
    }
  }
  # Get directory name
  dir <- dirname(file)
  # Check if directory exists, create it if not
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    if (!dir.exists(dir)) {
      abort(
        "Failed to create directory: ",
        dir,
        class = "rtemis_io_error"
      )
    } else {
      if (verbosity >= 1L) {
        msg(checkmark(), "Created directory:", dir)
      }
    }
  }
  # Write lines to file
  writeLines(x, con = file)
  # Check if file was created successfully
  if (!file.exists(file)) {
    abort(
      "Failed to create file: ",
      file,
      class = "rtemis_io_error"
    )
  } else {
    if (verbosity >= 1L) {
      msg(checkmark(), "Created file:", file)
    }
  }
  invisible(NULL)
} # /rtemis.core::write_lines
