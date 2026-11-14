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
#' @param digits NULL, or Integer or NA: NULL writes through [to_JSON()], which
#'   writes each double in the shortest form that reads back identical. An
#'   integer or `NA` is passed to `jsonlite::toJSON()` as its `digits`.
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
  digits = NULL,
  verbosity = 1L
) {
  check_dependencies("jsonlite")
  ordered <- order_schema_keywords(schema)
  json_str <- if (is.null(digits)) {
    to_JSON(ordered, pretty = TRUE)
  } else {
    as.character(jsonlite::toJSON(
      ordered,
      auto_unbox = TRUE,
      pretty = TRUE,
      na = "null",
      null = "null",
      digits = digits
    ))
  }
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


# %% JSON_SCHEMA_DIALECT ----
# The dialect every document emitted by `class_to_JSONSchema()` declares.
JSON_SCHEMA_DIALECT <- "https://json-schema.org/draft/2020-12/schema"


# %% schema_leaf ----
#' JSON Schema fragment for a scalar
#'
#' The element type, before container wrapping and nullability. Each keyword
#' is emitted from its own field; no keyword is emitted without one.
#'
#' @param spec Named list: A property spec.
#'
#' @return Named list: JSON Schema fragment.
#'
#' @author EDG
#' @keywords internal
#' @noRd
schema_leaf <- function(spec) {
  out <- list(type = spec[["type"]])
  if (!is.null(spec[["enum"]])) {
    # I(): `enum` is an array even with one member.
    out[["enum"]] <- I(spec[["enum"]])
  }
  if (!is.null(spec[["const"]])) {
    out[["const"]] <- spec[["const"]]
  }
  for (pair in list(
    c("minimum", "minimum"),
    c("maximum", "maximum"),
    c("exclusive_minimum", "exclusiveMinimum"),
    c("exclusive_maximum", "exclusiveMaximum")
  )) {
    value <- spec[[pair[[1L]]]]
    if (!is.null(value)) {
      out[[pair[[2L]]]] <- value
    }
  }
  if (identical(spec[["type"]], "string")) {
    min_length <- spec[["min_length"]] %||% 0L
    if (min_length > 0L) {
      out[["minLength"]] <- as.integer(min_length)
    }
    if (!is.null(spec[["max_length"]])) {
      out[["maxLength"]] <- as.integer(spec[["max_length"]])
    }
    if (!is.null(spec[["pattern"]])) {
      out[["pattern"]] <- spec[["pattern"]]
    }
  }
  out
} # /rtemis.core::schema_leaf


# %% schema_counts ----
#' Add `minItems`, `maxItems` and `uniqueItems` to an array fragment
#'
#' @param out Named list: Array fragment.
#' @param spec Named list: A property spec.
#' @param min_key,max_key Character: Keyword names, `minItems`/`maxItems` for
#'   arrays and `minProperties`/`maxProperties` for objects.
#'
#' @return `out` with the count keywords added.
#'
#' @author EDG
#' @keywords internal
#' @noRd
schema_counts <- function(
  out,
  spec,
  min_key = "minItems",
  max_key = "maxItems"
) {
  min_items <- spec[["min_items"]] %||% 1L
  if (min_items > 0L) {
    out[[min_key]] <- as.integer(min_items)
  }
  if (!is.null(spec[["max_items"]])) {
    out[[max_key]] <- as.integer(spec[["max_items"]])
  }
  if (min_key == "minItems" && isTRUE(spec[["unique_items"]])) {
    out[["uniqueItems"]] <- TRUE
  }
  out
} # /rtemis.core::schema_counts


# %% schema_object_map ----
#' JSON Schema fragment for a string-keyed object of one value schema
#'
#' @param value_schema Named list: Schema of each value.
#' @param spec Named list: A property spec, for the entry counts and `keys`.
#' @param defs Environment: The emission's collector.
#'
#' @return Named list: JSON Schema fragment.
#'
#' @author EDG
#' @keywords internal
#' @noRd
schema_object_map <- function(value_schema, spec, defs) {
  keys <- spec[["keys"]]
  schema_counts(
    list(
      type = "object",
      propertyNames = if (is.null(keys)) {
        list(minLength = 1L)
      } else {
        spec_to_JSONSchema(keys, defs)
      },
      additionalProperties = value_schema
    ),
    spec,
    min_key = "minProperties",
    max_key = "maxProperties"
  )
} # /rtemis.core::schema_object_map


# %% empty_object ----
#' The JSON object `{}`, as an R value
#'
#' `list()` is written as `[]`; a list with empty names is written as `{}`.
#'
#' @return Named list of length 0.
#'
#' @author EDG
#' @keywords internal
#' @noRd
empty_object <- function() {
  structure(list(), names = character(0L))
} # /rtemis.core::empty_object


# %% schema_ref ----
#' A `$ref` to a definition
#'
#' @param name Character: Definition name in `$defs`.
#'
#' @return Named list: `{"$ref": "#/$defs/<name>"}`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
schema_ref <- function(name) {
  list(`$ref` = paste0("#/$defs/", name))
} # /rtemis.core::schema_ref


# %% schema_define ----
#' Record a definition in `$defs`
#'
#' Two declarations may share a definition name only when they emit the same
#' schema; a second, different schema under one name is a declaration error.
#'
#' @param defs Environment: Definitions collected so far, by name.
#' @param name Character: Definition name.
#' @param schema Named list: The definition.
#'
#' @return NULL, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
schema_define <- function(defs, name, schema) {
  if (exists(name, envir = defs_classes(defs), inherits = FALSE)) {
    abort(
      "A def_name and an S7 class both define '",
      name,
      "' in $defs.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (exists(name, envir = defs, inherits = FALSE)) {
    if (!identical(get(name, envir = defs), schema)) {
      abort(
        "Two declarations define '",
        name,
        "' in $defs with different schemas.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  } else {
    assign(name, schema, envir = defs)
  }
  invisible(NULL)
} # /rtemis.core::schema_define


# %% spec_to_JSONSchema ----
#' JSON Schema fragment for a property spec
#'
#' Emits the shape (`spec_shape()`), its description, and its node marker,
#' calls the definition site of `annotate`, then applies `def_name` and
#' nullability. Never emits a `default`: a default is what an interface fills
#' in, not a fact about the document. A spec with a `def_name` is emitted once
#' into `defs` and referred to by `$ref`, or refers to the URI that `refs`
#' maps the name to; an S7 class referenced by `prop_instance()` is emitted
#' into `defs` under its class name, or referred to by its URI in `refs`.
#'
#' @param spec Named list: A property spec.
#' @param defs Environment: The emission's collector, from
#'   `new_JSONSchema_defs()`.
#'
#' @return Named list: JSON Schema fragment.
#'
#' @author EDG
#' @keywords internal
#' @noRd
spec_to_JSONSchema <- function(spec, defs) {
  def_name <- spec[["def_name"]]
  external <- if (is.null(def_name)) NULL else defs_refs(defs)[def_name]
  if (!is.null(external) && !is.na(external)) {
    out <- list(`$ref` = unname(external))
    if (isTRUE(spec[["nullable"]])) {
      out <- schema_nullable(out)
    }
    return(out)
  }
  shape <- spec_shape(spec)
  container <- spec[["container"]]
  out <- switch(
    shape,
    union = list(
      anyOf = unname(lapply(spec[["alternatives"]], spec_to_JSONSchema, defs))
    ),
    struct = {
      members <- lapply(names(spec[["members"]]), function(nm) {
        member_to_JSONSchema(spec[["members"]][[nm]], defs, nm, NULL)
      })
      names(members) <- names(spec[["members"]])
      obj <- list(type = "object", properties = members)
      if (length(spec[["required"]]) > 0L) {
        obj[["required"]] <- I(spec[["required"]])
      }
      additional <- spec[["additional_members"]]
      if (is.null(additional)) {
        obj[["additionalProperties"]] <- FALSE
      } else {
        # Undeclared member names are keys, held to the rule of map keys.
        obj[["propertyNames"]] <- list(minLength = 1L)
        obj[["additionalProperties"]] <- spec_to_JSONSchema(additional, defs)
      }
      min_members <- spec[["min_members"]] %||% 0L
      if (min_members > 0L) {
        obj[["minProperties"]] <- as.integer(min_members)
      }
      if (!is.null(spec[["max_members"]])) {
        obj[["maxProperties"]] <- as.integer(spec[["max_members"]])
      }
      obj
    },
    object = instance_to_JSONSchema(spec, defs),
    collection = {
      ref <- annotate_fragment(
        class_schema_ref(spec_target(spec), defs),
        container_item_spec(spec),
        defs
      )
      if (identical(container, "map")) {
        schema_object_map(ref, spec, defs)
      } else {
        schema_counts(list(type = "array", items = ref), spec)
      }
    },
    nested_array = schema_counts(
      list(type = "array", items = spec_to_JSONSchema(spec[["items"]], defs)),
      spec
    ),
    nested_map = schema_object_map(
      spec_to_JSONSchema(spec[["values"]], defs),
      spec,
      defs
    ),
    matrix = {
      cells <- spec[["items"]]
      cell <- schema_leaf(cells)
      if (isTRUE(cells[["nullable"]])) {
        cell <- schema_nullable(cell)
      }
      cell <- annotate_fragment(cell, cells, defs)
      row <- list(type = "array", items = cell)
      if (!is.null(spec[["ncol"]])) {
        row[["minItems"]] <- spec[["ncol"]]
        row[["maxItems"]] <- spec[["ncol"]]
      }
      schema_counts(list(type = "array", items = row), spec)
    },
    table = table_to_JSONSchema(spec, defs),
    factor = factor_to_JSONSchema(spec),
    bag = schema_object_map(
      annotate_fragment(empty_object(), container_item_spec(spec), defs),
      spec,
      defs
    ),
    any = empty_object(),
    {
      if (identical(container, "none")) {
        schema_leaf(spec)
      } else {
        item <- annotate_fragment(
          schema_leaf(spec),
          container_item_spec(spec),
          defs
        )
        if (identical(container, "array")) {
          schema_counts(list(type = "array", items = item), spec)
        } else {
          schema_object_map(item, spec, defs)
        }
      }
    }
  )
  contains <- spec[["contains"]]
  if (!is.null(contains)) {
    out[["contains"]] <- spec_to_JSONSchema(contains, defs)
    min_contains <- spec[["min_contains"]] %||% 1L
    if (min_contains != 1L) {
      out[["minContains"]] <- as.integer(min_contains)
    }
    if (!is.null(spec[["max_contains"]])) {
      out[["maxContains"]] <- as.integer(spec[["max_contains"]])
    }
  }
  description <- spec[["description"]] %||% ""
  if (nzchar(description)) {
    out[["description"]] <- description
  }
  if (shape %in% c("matrix", "table", "factor")) {
    out[["x-rtemis"]] <- list(node = shape)
  }
  out <- annotate_fragment(out, spec, defs)
  if (!is.null(def_name)) {
    schema_define(defs, def_name, out)
    out <- schema_ref(def_name)
  }
  if (isTRUE(spec[["nullable"]]) && shape != "any") {
    out <- schema_nullable(out)
  }
  out
} # /rtemis.core::spec_to_JSONSchema


# %% member_to_JSONSchema ----
#' JSON Schema fragment of a member, with the member site of `annotate`
#'
#' @param spec Named list: The member's spec.
#' @param defs Environment: The emission's collector.
#' @param name Character: Member name.
#' @param owner Character or NULL: Qualified name of the class that holds the
#'   member, or NULL for a struct or table member.
#'
#' @return Named list: JSON Schema fragment.
#'
#' @author EDG
#' @keywords internal
#' @noRd
member_to_JSONSchema <- function(spec, defs, name, owner) {
  annotate_fragment(
    spec_to_JSONSchema(spec, defs),
    spec,
    defs,
    site = list(kind = "member", name = name, owner = owner)
  )
} # /rtemis.core::member_to_JSONSchema


# %% instance_to_JSONSchema ----
#' JSON Schema fragment of an instance, with its use-site members
#'
#' @param spec Named list: Instance spec.
#' @param defs Environment: The emission's collector.
#'
#' @return Named list: `$ref` to the class, with `properties` and `required`
#'   beside it for the use-site members.
#'
#' @author EDG
#' @keywords internal
#' @noRd
instance_to_JSONSchema <- function(spec, defs) {
  cls <- spec_target(spec)
  out <- class_schema_ref(cls, defs)
  members <- spec[["members"]]
  if (length(members) > 0L) {
    check_instance_members(spec, cls)
    owner <- target_class_name(cls)
    props <- lapply(names(members), function(nm) {
      member_to_JSONSchema(members[[nm]], defs, nm, owner)
    })
    names(props) <- names(members)
    out[["properties"]] <- props
    req <- names(members)[
      !vapply(members, function(m) isTRUE(m[["nullable"]]), logical(1L))
    ]
    if (length(req) > 0L) {
      out[["required"]] <- I(req)
    }
  }
  out
} # /rtemis.core::instance_to_JSONSchema


# %% table_to_JSONSchema ----
#' JSON Schema fragment of a table: an array of row objects
#'
#' @param spec Named list: Table spec.
#' @param defs Environment: The emission's collector.
#'
#' @return Named list: JSON Schema fragment.
#'
#' @author EDG
#' @keywords internal
#' @noRd
table_to_JSONSchema <- function(spec, defs) {
  columns <- spec[["columns"]]
  props <- lapply(names(columns), function(nm) {
    member_to_JSONSchema(columns[[nm]], defs, nm, NULL)
  })
  names(props) <- names(columns)
  row <- list(type = "object", properties = props)
  if (length(spec[["required"]]) > 0L) {
    row[["required"]] <- I(spec[["required"]])
  }
  additional <- spec[["additional_columns"]]
  if (is.null(additional)) {
    row[["additionalProperties"]] <- FALSE
  } else {
    row[["propertyNames"]] <- list(minLength = 1L)
    row[["additionalProperties"]] <- spec_to_JSONSchema(additional, defs)
  }
  schema_counts(list(type = "array", items = row), spec)
} # /rtemis.core::table_to_JSONSchema


# %% factor_to_JSONSchema ----
#' JSON Schema fragment of a factor: its levels and codes
#'
#' @param spec Named list: Factor spec.
#'
#' @return Named list: JSON Schema fragment.
#'
#' @author EDG
#' @keywords internal
#' @noRd
factor_to_JSONSchema <- function(spec) {
  level <- list(type = "string", minLength = 1L)
  if (!is.null(spec[["enum"]])) {
    level[["enum"]] <- I(spec[["enum"]])
  }
  code <- list(
    type = if (isTRUE(spec[["nullable_codes"]])) {
      c("integer", "null")
    } else {
      "integer"
    },
    minimum = 1L
  )
  list(
    type = "object",
    properties = list(
      levels = list(
        type = "array",
        items = level,
        minItems = 1L,
        uniqueItems = TRUE
      ),
      codes = schema_counts(list(type = "array", items = code), spec)
    ),
    required = I(c("levels", "codes")),
    additionalProperties = FALSE
  )
} # /rtemis.core::factor_to_JSONSchema


# %% container_item_spec ----
#' The item declaration of a container stored in the released layout
#'
#' @param spec Named list: Array or map spec in the scalar, collection, or bag
#'   layout.
#'
#' @return Named list: The item's spec.
#'
#' @author EDG
#' @keywords internal
#' @noRd
container_item_spec <- function(spec) {
  shape <- spec_shape(spec)
  if (shape == "bag") {
    return(prop_spec(prop_any()))
  }
  item <- spec
  item[["container"]] <- "none"
  item[["nullable"]] <- FALSE
  item[["min_items"]] <- 1L
  item["max_items"] <- list(NULL)
  item[["unique_items"]] <- FALSE
  item["default"] <- list(NULL)
  item["def_name"] <- list(NULL)
  item[["description"]] <- ""
  for (nm in c("contains", "min_contains", "max_contains", "keys")) {
    item[[nm]] <- NULL
  }
  item
} # /rtemis.core::container_item_spec


# %% schema_nullable ----
#' Admit null in a fragment
#'
#' A fragment with a `type` gains `"null"` in it, and in its `enum` when it has
#' one: a value must satisfy both keywords. A union's `anyOf` gains
#' `{"type": "null"}` as its last alternative. A fragment with a `const` or a
#' `$ref` is wrapped in `anyOf` with `{"type": "null"}`.
#'
#' @param out Named list: JSON Schema fragment.
#'
#' @return Named list: JSON Schema fragment.
#'
#' @author EDG
#' @keywords internal
#' @noRd
schema_nullable <- function(out) {
  if (
    !is.null(out[["anyOf"]]) &&
      is.null(out[["type"]]) &&
      is.null(out[["$ref"]])
  ) {
    out[["anyOf"]] <- c(out[["anyOf"]], list(list(type = "null")))
    return(out)
  }
  # A `const` fragment keeps its `const`, so that a nullable constant and a
  # nullable one-member `enum` emit different schemas.
  if (is.null(out[["type"]]) || !is.null(out[["const"]])) {
    return(list(anyOf = list(out, list(type = "null"))))
  }
  out[["type"]] <- c(out[["type"]], "null")
  if (!is.null(out[["enum"]])) {
    out[["enum"]] <- I(c(as.list(out[["enum"]]), list(NULL)))
  }
  out
} # /rtemis.core::schema_nullable


# %% class_schema_ref ----
#' A `$ref` to an S7 class's definition, emitting the definition once
#'
#' A class that the collector's `refs` maps to a URI is referred to by that
#' URI, marked with its qualified name, and defined nowhere. Any other class
#' is defined in `$defs` under its class name, by the collector's
#' `class_schema` function or by `class_object_schema()`, and marked with its
#' qualified name.
#'
#' @param cls S7 class.
#' @param defs Environment: The emission's collector.
#'
#' @return Named list: `{"$ref": "#/$defs/<Class>"}`, or the mapped `$ref`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
class_schema_ref <- function(cls, defs) {
  qualified <- target_class_name(cls)
  external <- defs_refs(defs)[qualified]
  if (!is.na(external)) {
    return(list(
      `$ref` = unname(external),
      `x-rtemis` = list(class = qualified)
    ))
  }
  name <- cls@name
  classes <- defs_classes(defs)
  if (exists(name, envir = classes, inherits = FALSE)) {
    if (!identical(get(name, envir = classes), cls)) {
      abort(
        "Two different S7 classes are named '",
        name,
        "'; $defs holds one definition per name.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    return(schema_ref(name))
  }
  if (exists(name, envir = defs, inherits = FALSE)) {
    abort(
      "A def_name and an S7 class both define '",
      name,
      "' in $defs.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  assign(name, cls, envir = classes)
  # A placeholder first, so that a class referring to itself terminates.
  assign(name, list(), envir = defs)
  hook <- defs_option(defs, "class_schema")
  body <- if (is.null(hook)) NULL else hook(cls, defs)
  if (is.null(body)) {
    body <- class_object_schema(cls, defs)
  }
  marker <- body[["x-rtemis"]] %||% list()
  marker[["class"]] <- qualified
  body[["x-rtemis"]] <- marker
  assign(name, body, envir = defs)
  schema_ref(name)
} # /rtemis.core::class_schema_ref


# %% defs_classes ----
#' The classes defined in a `defs` environment, by name
#'
#' Kept under a hidden name, which `ls()` omits and no `def_name` can take.
#'
#' @param defs Environment: Definitions collected so far, by name.
#'
#' @return Environment of S7 classes, by class name.
#'
#' @author EDG
#' @keywords internal
#' @noRd
defs_classes <- function(defs) {
  if (!exists(".classes", envir = defs, inherits = FALSE)) {
    assign(".classes", new.env(parent = emptyenv()), envir = defs)
  }
  get(".classes", envir = defs, inherits = FALSE)
} # /rtemis.core::defs_classes


# %% defs_option ----
#' An emission option held by a collector
#'
#' @param defs Environment: The emission's collector.
#' @param name Character: `"refs"`, `"annotate"`, or `"class_schema"`.
#'
#' @return The option, or NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
defs_option <- function(defs, name) {
  get0(paste0(".", name), envir = defs, inherits = FALSE)
} # /rtemis.core::defs_option


# %% defs_refs ----
#' The `refs` map of a collector
#'
#' @param defs Environment: The emission's collector.
#'
#' @return Named character vector, empty when the collector has none.
#'
#' @author EDG
#' @keywords internal
#' @noRd
defs_refs <- function(defs) {
  defs_option(defs, "refs") %||% stats::setNames(character(0L), character(0L))
} # /rtemis.core::defs_refs


# %% annotate_fragment ----
#' Call a collector's `annotate` function on a fragment
#'
#' @param fragment Named list: JSON Schema fragment.
#' @param spec Named list: The declaration's spec.
#' @param defs Environment: The emission's collector.
#' @param site Named list: `list(kind = "definition")` or
#'   `list(kind = "member", name = , owner = )`.
#'
#' @return The annotated fragment. Aborts when `annotate` changes anything it
#'   may not.
#'
#' @author EDG
#' @keywords internal
#' @noRd
annotate_fragment <- function(
  fragment,
  spec,
  defs,
  site = list(kind = "definition")
) {
  force(fragment)
  annotate <- defs_option(defs, "annotate")
  if (is.null(annotate)) {
    return(fragment)
  }
  out <- annotate(fragment, spec_declaration(spec), site)
  check_annotation(fragment, out)
  out
} # /rtemis.core::annotate_fragment


# %% ANNOTATION_KEYWORDS ----
# Keywords `annotate` may add to a fragment, beside any `x-` keyword.
ANNOTATION_KEYWORDS <- c(
  "$comment",
  "readOnly",
  "writeOnly",
  "deprecated",
  "examples",
  "title",
  "description"
)


# %% check_annotation ----
#' Check what an `annotate` function changed
#'
#' It may add annotation keywords and `x-` keywords, add members to
#' `x-rtemis` other than `node` and `class`, and change `description`.
#'
#' @param before Named list: The fragment given.
#' @param after Value returned.
#'
#' @return NULL, invisibly. Aborts on any other change.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_annotation <- function(before, after) {
  fail <- function(what) {
    abort(
      "`annotate` ",
      what,
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (!is.list(after) || (length(after) > 0L && is.null(names(after)))) {
    fail("must return a named list.")
  }
  for (nm in setdiff(names(before), "description")) {
    if (!nm %in% names(after)) {
      fail(paste0("removed '", nm, "'."))
    }
    if (nm == "x-rtemis") {
      inner <- before[[nm]]
      for (k in names(inner)) {
        if (!identical(after[[nm]][[k]], inner[[k]])) {
          fail(paste0("changed 'x-rtemis' member '", k, "'."))
        }
      }
      for (k in c("node", "class")) {
        if (is.null(inner[[k]]) && !is.null(after[[nm]][[k]])) {
          fail(paste0("added the 'x-rtemis' member '", k, "'."))
        }
      }
    } else if (!identical(after[[nm]], before[[nm]])) {
      fail(paste0("changed '", nm, "'."))
    }
  }
  added <- setdiff(names(after), names(before))
  bad <- added[!added %in% ANNOTATION_KEYWORDS & !startsWith(added, "x-")]
  if (length(bad) > 0L) {
    fail(paste0("added '", bad[[1L]], "', which is not an annotation."))
  }
  rtemis <- after[["x-rtemis"]]
  if (
    is.null(before[["x-rtemis"]]) &&
      !is.null(rtemis) &&
      any(c("node", "class") %in% names(rtemis))
  ) {
    fail("added an 'x-rtemis' member 'node' or 'class'.")
  }
  invisible(NULL)
} # /rtemis.core::check_annotation


# %% class_object_schema ----
#' JSON Schema object for an S7 class's properties
#'
#' Every property must be built by a `prop_*` factory. A property is required
#' when it is not nullable and has no default, or when `required` names it;
#' the object is closed. A class declared by `schema_class()` carries its
#' description.
#'
#' @param cls S7 class.
#' @param defs Environment: Definitions collected so far, by name.
#' @param envelope Named list of S7 properties: Members placed before the
#'   class's own properties.
#' @param required Character: Further member names to require.
#'
#' @return Named list: JSON Schema fragment.
#'
#' @author EDG
#' @keywords internal
#' @noRd
class_object_schema <- function(
  cls,
  defs,
  envelope = list(),
  required = character(0L)
) {
  props <- c(envelope, cls@properties)
  specs <- lapply(names(props), function(nm) {
    spec <- prop_spec(props[[nm]])
    if (is.null(spec)) {
      abort(
        "Property '",
        nm,
        "' of ",
        cls@name,
        " is not built by a prop_* factory, so it has no declaration to emit.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    spec
  })
  names(specs) <- names(props)
  owner <- target_class_name(cls)
  properties <- lapply(names(specs), function(nm) {
    member_to_JSONSchema(specs[[nm]], defs, nm, owner)
  })
  names(properties) <- names(specs)
  declared_required <- names(specs)[vapply(
    specs,
    function(s) is.null(s[["default"]]) && !isTRUE(s[["nullable"]]),
    logical(1L)
  )]
  unknown <- setdiff(required, names(specs))
  if (length(unknown) > 0L) {
    abort(
      "`required` names members that ",
      cls@name,
      " does not have: ",
      paste0("'", unknown, "'", collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  req <- unique(c(required, declared_required))
  out <- list()
  description <- class_description(cls)
  if (!is.null(description)) {
    out[["description"]] <- description
  }
  out[["type"]] <- "object"
  out[["properties"]] <- properties
  if (length(req) > 0L) {
    out[["required"]] <- I(req)
  }
  out[["additionalProperties"]] <- FALSE
  out
} # /rtemis.core::class_object_schema


# %% class_to_JSONSchema ----
#' Generate a JSON Schema document from an S7 class
#'
#' Emits a JSON Schema (draft 2020-12) for documents that serialize an object
#' of `cls`: a closed object with one member per property, each typed from its
#' `prop_*` declaration. S7 classes referenced through [prop_instance()] are
#' defined once in `$defs` under their class names;
#' declarations carrying a `def_name` are defined once under that name. Write
#' the result with [write_JSONSchema()].
#'
#' A class declared by [schema_class()] supplies its description: the root
#' class's as the default of `description`, and every other class's in its
#' `$defs` entry. The [SchemaPublication] of a published root class is written
#' as `x-rtemis.publication`, with the producer package, the language `"r"`,
#' and the qualified class name.
#'
#' @details
#' A member is required when it is not nullable and has no default, or when
#' `required` names it. `required` applies to the document root only; a class
#' reached through [prop_instance()] has derived presence. No `default` keyword
#' is emitted. Every property must be built by a `prop_*` factory.
#'
#' `envelope` holds members that belong to the document rather than to the R
#' object, such as a `$schema` identifier and a format version declared with
#' `const`. They are placed first. Name them in `required` when the document
#' must carry them; a constant is its own default, so it is otherwise
#' optional.
#'
#' @param cls S7 class: The document's class.
#' @param id Character: The schema's `$id` URL.
#' @param title Character: Schema title.
#' @param description Character: What a document of this class is. Defaults to
#'   the description declared by [schema_class()], or `""`, which emits no
#'   `description`.
#' @param envelope Named list of S7 properties: Document members placed before
#'   the class's own properties.
#' @param required Character: Members of the document root to require in
#'   addition to those without a default. An unknown name aborts.
#' @param refs Named character vector or NULL: Absolute URIs, by qualified
#'   class name (`"pkg::Class"`) or by `def_name`. A class or a `def_name`
#'   named here is referred to by its URI and defined nowhere in the document;
#'   see [new_JSONSchema_defs()].
#' @param annotate Function or NULL: Adds annotations to each fragment; see
#'   [new_JSONSchema_defs()].
#'
#' @return Named list: The JSON Schema.
#'
#' @author EDG
#' @export
#'
#' @examples
#' Point <- S7::new_class(
#'   "Point",
#'   properties = list(
#'     x = prop_number(description = "Horizontal coordinate."),
#'     label = prop_string("", min_length = 0L)
#'   )
#' )
#' schema <- class_to_JSONSchema(
#'   Point,
#'   id = "https://example.org/point/v1/schema.json",
#'   title = "Point",
#'   envelope = list(version = prop_string(const = "1.0.0")),
#'   required = "version"
#' )
#' schema[["required"]]
class_to_JSONSchema <- function(
  cls,
  id,
  title = cls@name,
  description = class_description(cls) %||% "",
  envelope = list(),
  required = character(0L),
  refs = NULL,
  annotate = NULL
) {
  check_target_class(cls)
  check_character_scalar(id)
  check_character_scalar(title)
  if (
    !is.character(description) ||
      length(description) != 1L ||
      is.na(description)
  ) {
    abort(
      "`description` must be a character string.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (
    !is.list(envelope) || (length(envelope) > 0L && is.null(names(envelope)))
  ) {
    abort(
      "`envelope` must be a named list of properties.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  defs <- new_JSONSchema_defs(refs = refs, annotate = annotate)
  body <- class_object_schema(
    cls,
    defs,
    envelope = envelope,
    required = required
  )
  out <- list(
    `$schema` = JSON_SCHEMA_DIALECT,
    `$id` = id,
    title = title
  )
  if (nzchar(description)) {
    out[["description"]] <- description
  }
  body[["description"]] <- NULL
  out <- c(out, body)
  publication <- publication_annotation(cls)
  if (!is.null(publication)) {
    out[["x-rtemis"]] <- list(publication = publication)
  }
  entries <- JSONSchema_defs(defs)
  if (length(entries) > 0L) {
    out[["$defs"]] <- entries
  }
  out
} # /rtemis.core::class_to_JSONSchema


# %% new_JSONSchema_defs ----
#' Start a JSON Schema emission
#'
#' Returns the collector that one emission threads through
#' [prop_to_JSONSchema()]: it gathers the `$defs` entries the fragments refer
#' to and holds the options of the emission. A producer that assembles its
#' own class objects, under its own presence and membership rules, emits each
#' member with [prop_to_JSONSchema()] and writes [JSONSchema_defs()] as the
#' document's `$defs`.
#'
#' `annotate(fragment, declaration, site)` is called for every declaration,
#' including the item of a container and each envelope member, and not for
#' matrix rows, table row objects, the `levels` and `codes` of a factor, or
#' class objects. `declaration` is the declaration's [prop_declaration()].
#' `site` is `list(kind = "definition")` before `def_name` and `nullable`
#' apply, and `list(kind = "member", name = , owner = )` on a member's
#' fragment after them, where `owner` is the qualified name of the class that
#' holds the member, or NULL for a struct or table member. `annotate` returns
#' the fragment with keywords added from `x-*`, `$comment`, `readOnly`,
#' `writeOnly`, `deprecated`, `examples`, and `title`, with members added to
#' `x-rtemis` other than `node` and `class`, or with `description` changed;
#' any other change aborts. A fact about one member, such as `readOnly`, is
#' added at the member site, so that a `$defs` entry shared by several
#' members carries none.
#'
#' @param refs Named character vector or NULL: Absolute URIs, by qualified
#'   class name (`"pkg::Class"`) or by `def_name`. An instance of a class named
#'   here emits `{"$ref": <URI>}`, marked with `x-rtemis.class`, and a
#'   declaration with a `def_name` named here emits `{"$ref": <URI>}`; neither
#'   is defined in `$defs`.
#' @param annotate Function or NULL: `function(fragment, declaration, site)`,
#'   as described above.
#' @param class_schema Function or NULL: `function(cls, defs)` returning the
#'   object schema of a class that enters `$defs`, or NULL for the class
#'   object that [class_to_JSONSchema()] writes: one member per property,
#'   required when not nullable and without a default, closed. Core adds the
#'   class's `x-rtemis.class` marker to either.
#'
#' @return Environment: The collector.
#'
#' @author EDG
#' @export
#'
#' @examples
#' Point <- S7::new_class("Point", properties = list(x = prop_number(0)))
#' defs <- new_JSONSchema_defs(
#'   annotate = function(fragment, declaration, site) {
#'     if (identical(site[["kind"]], "member")) {
#'       fragment[["readOnly"]] <- TRUE
#'     }
#'     fragment
#'   }
#' )
#' prop_to_JSONSchema(prop_instance(Point), defs, name = "origin")
#' names(JSONSchema_defs(defs))
new_JSONSchema_defs <- function(
  refs = NULL,
  annotate = NULL,
  class_schema = NULL
) {
  if (!is.null(refs)) {
    if (
      !is.character(refs) ||
        anyNA(refs) ||
        is.null(names(refs)) ||
        !is.null(validate_keys(refs))
    ) {
      abort(
        "`refs` must be a character vector with distinct, non-empty names.",
        class = c("rtemis_type_error", "rtemis_input_error")
      )
    }
    relative <- refs[!grepl("^[A-Za-z][A-Za-z0-9+.-]*:", refs)]
    if (length(relative) > 0L) {
      abort(
        "`refs` must hold absolute URIs; '",
        relative[[1L]],
        "' has no scheme.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
  for (f in list(annotate = annotate, class_schema = class_schema)) {
    if (!is.null(f) && !is.function(f)) {
      abort(
        "`annotate` and `class_schema` must be functions or NULL.",
        class = c("rtemis_type_error", "rtemis_input_error")
      )
    }
  }
  defs <- new.env(parent = emptyenv())
  assign(".refs", refs, envir = defs)
  assign(".annotate", annotate, envir = defs)
  assign(".class_schema", class_schema, envir = defs)
  defs
} # /rtemis.core::new_JSONSchema_defs


# %% JSONSchema_defs ----
#' The `$defs` entries an emission collected
#'
#' @param defs Environment: A collector from [new_JSONSchema_defs()].
#'
#' @return Named list: The entries, sorted by name in code-point order, as
#'   [class_to_JSONSchema()] writes them.
#'
#' @author EDG
#' @export
#'
#' @examples
#' Point <- S7::new_class("Point", properties = list(x = prop_number(0)))
#' defs <- new_JSONSchema_defs()
#' prop_to_JSONSchema(prop_instance(Point), defs)
#' JSONSchema_defs(defs)[["Point"]][["type"]]
JSONSchema_defs <- function(defs) {
  check_defs(defs)
  def_names <- sort(ls(defs), method = "radix")
  mget(def_names, envir = defs)
} # /rtemis.core::JSONSchema_defs


# %% prop_to_JSONSchema ----
#' JSON Schema fragment of a property
#'
#' Emits the fragment of one declaration, adding to `defs` every definition
#' it refers to: the classes reached through [prop_instance()] and the
#' declarations with a `def_name`.
#'
#' @param property S7 property: A declaration from a `prop_*` factory.
#' @param defs Environment: A collector from [new_JSONSchema_defs()].
#' @param name Character or NULL: The member name, which calls the member site
#'   of the collector's `annotate`.
#' @param owner Character or NULL: Qualified name of the class that holds the
#'   member, or NULL for a member of a struct or of a producer's own object.
#'
#' @return Named list: JSON Schema fragment.
#'
#' @author EDG
#' @export
#'
#' @examples
#' defs <- new_JSONSchema_defs()
#' prop_to_JSONSchema(prop_array(prop_integer(min = 1L)), defs)
prop_to_JSONSchema <- function(property, defs, name = NULL, owner = NULL) {
  spec <- encode_spec_of(property)
  check_defs(defs)
  if (is.null(name)) {
    return(spec_to_JSONSchema(spec, defs))
  }
  check_character_scalar(name)
  if (!is.null(owner)) {
    check_character_scalar(owner)
  }
  member_to_JSONSchema(spec, defs, name, owner)
} # /rtemis.core::prop_to_JSONSchema


# %% check_defs ----
#' Check a collector argument
#'
#' @param defs Value to check.
#'
#' @return NULL, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_defs <- function(defs) {
  if (
    !is.environment(defs) || !exists(".refs", envir = defs, inherits = FALSE)
  ) {
    abort(
      "`defs` must be a collector from new_JSONSchema_defs().",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  invisible(NULL)
} # /rtemis.core::check_defs
