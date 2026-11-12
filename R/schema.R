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
#' @param spec Named list: A property spec, for the entry counts.
#'
#' @return Named list: JSON Schema fragment.
#'
#' @author EDG
#' @keywords internal
#' @noRd
schema_object_map <- function(value_schema, spec) {
  schema_counts(
    list(
      type = "object",
      propertyNames = list(minLength = 1L),
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
#' Emits the shape (`spec_shape()`), then nullability, then the description.
#' Never emits a `default`: a default is what an interface fills in, not a
#' fact about the document. A spec with a `def_name` is emitted once into
#' `defs` and referred to by `$ref`; an S7 class referenced by
#' `prop_instance()` is emitted into `defs` under its class name.
#'
#' @param spec Named list: A property spec.
#' @param defs Environment: Definitions collected so far, by name.
#'
#' @return Named list: JSON Schema fragment.
#'
#' @author EDG
#' @keywords internal
#' @noRd
spec_to_JSONSchema <- function(spec, defs) {
  shape <- spec_shape(spec)
  container <- spec[["container"]]
  out <- switch(
    shape,
    union = list(
      anyOf = unname(lapply(spec[["alternatives"]], spec_to_JSONSchema, defs))
    ),
    struct = {
      members <- lapply(spec[["members"]], spec_to_JSONSchema, defs)
      obj <- list(type = "object", properties = members)
      if (length(spec[["required"]]) > 0L) {
        obj[["required"]] <- I(spec[["required"]])
      }
      additional <- spec[["additional_members"]]
      obj[["additionalProperties"]] <- if (is.null(additional)) {
        FALSE
      } else {
        spec_to_JSONSchema(additional, defs)
      }
      obj
    },
    object = class_schema_ref(spec[["target"]], defs),
    collection = {
      ref <- class_schema_ref(spec[["target"]], defs)
      if (identical(container, "map")) {
        schema_object_map(ref, spec)
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
      spec
    ),
    matrix = {
      ncol <- spec[["ncol"]]
      row <- list(
        type = "array",
        items = schema_leaf(spec[["items"]]),
        minItems = ncol,
        maxItems = ncol
      )
      schema_counts(list(type = "array", items = row), spec)
    },
    bag = schema_object_map(empty_object(), spec),
    any = empty_object(),
    {
      if (identical(container, "array")) {
        schema_counts(list(type = "array", items = schema_leaf(spec)), spec)
      } else if (identical(container, "map")) {
        schema_object_map(schema_leaf(spec), spec)
      } else {
        schema_leaf(spec)
      }
    }
  )
  description <- spec[["description"]] %||% ""
  if (nzchar(description)) {
    out[["description"]] <- description
  }
  def_name <- spec[["def_name"]]
  if (!is.null(def_name)) {
    schema_define(defs, def_name, out)
    out <- schema_ref(def_name)
  }
  if (isTRUE(spec[["nullable"]]) && shape != "any") {
    out <- schema_nullable(out)
  }
  out
} # /rtemis.core::spec_to_JSONSchema


# %% schema_nullable ----
#' Admit null in a fragment
#'
#' A fragment with a `type` gains `"null"` in it, and in its `enum` when it has
#' one: a value must satisfy both keywords. A `const` becomes
#' `enum: [<const>, null]`. A `$ref` or `anyOf` fragment is wrapped in `anyOf`
#' with `{"type": "null"}`.
#'
#' @param out Named list: JSON Schema fragment.
#'
#' @return Named list: JSON Schema fragment.
#'
#' @author EDG
#' @keywords internal
#' @noRd
schema_nullable <- function(out) {
  if (is.null(out[["type"]])) {
    return(list(anyOf = list(out, list(type = "null"))))
  }
  out[["type"]] <- c(out[["type"]], "null")
  if (!is.null(out[["const"]])) {
    out[["enum"]] <- I(list(out[["const"]], NULL))
    out[["const"]] <- NULL
  } else if (!is.null(out[["enum"]])) {
    out[["enum"]] <- I(c(as.list(out[["enum"]]), list(NULL)))
  }
  out
} # /rtemis.core::schema_nullable


# %% class_schema_ref ----
#' A `$ref` to an S7 class's definition, emitting the definition once
#'
#' @param cls S7 class.
#' @param defs Environment: Definitions collected so far, by name.
#'
#' @return Named list: `{"$ref": "#/$defs/<Class>"}`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
class_schema_ref <- function(cls, defs) {
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
  assign(name, class_object_schema(cls, defs), envir = defs)
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


# %% class_object_schema ----
#' JSON Schema object for an S7 class's properties
#'
#' Every property must be built by a `prop_*` factory. A property is required
#' when it is not nullable and has no default, or when `required` names it;
#' the object is closed.
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
  properties <- lapply(specs, spec_to_JSONSchema, defs)
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
  out <- list(type = "object", properties = properties)
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
#' @param description Character: What a document of this class is.
#' @param envelope Named list of S7 properties: Document members placed before
#'   the class's own properties.
#' @param required Character: Members of the document root to require in
#'   addition to those without a default. An unknown name aborts.
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
  description = "",
  envelope = list(),
  required = character(0L)
) {
  check_target_class(cls)
  check_character_scalar(id)
  check_character_scalar(title)
  if (
    !is.list(envelope) || (length(envelope) > 0L && is.null(names(envelope)))
  ) {
    abort(
      "`envelope` must be a named list of properties.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  defs <- new.env(parent = emptyenv())
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
  out <- c(out, body)
  def_names <- sort(ls(defs))
  if (length(def_names) > 0L) {
    out[["$defs"]] <- mget(def_names, envir = defs)
  }
  out
} # /rtemis.core::class_to_JSONSchema
