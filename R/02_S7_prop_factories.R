# 2026- EDG rtemis.org

# TOC ----
# Spec ------------------------------------------------------------------------
#   PROP_TYPES / PROP_CONTAINERS
#   new_prop_spec / validate_with_spec / spec_validator / make_prop
# Factories -------------------------------------------------------------------
#   prop_boolean
#   prop_integer
#   prop_float
#   prop_string
#   prop_bag
#   prop_const
# Introspection ---------------------------------------------------------------
#   prop_spec
#
# These are the type-generic property factories: one declaration carries type,
# default, bounds, enum and description, and the S7 validator is generated from
# it. They are the successor to the hand-written properties in
# `00_S7_properties.R` -- `prob_scalar` is `prop_float(min = 0, max = 1)`,
# `optional_character_scalar` is `prop_string(nullable = TRUE)` -- and the
# generic half of the `prop_*` family in rtemis, whose factories add the
# supervised-learning fields (tunability, data bounds, conditional application).
# Field names match rtemis's `PropertySpec` so that the two can be collapsed
# onto one implementation later.

# %% PROP_TYPES ----
# JSON Schema base types a property's leaf value may take. "object" is an
# opaque pass-through: a named list handed to a foreign backend, with no
# per-key contract (see `prop_bag()`).
PROP_TYPES <- c("boolean", "integer", "number", "string", "object")

# %% PROP_CONTAINERS ----
# How a property's values are wrapped.
# - "none"  a single value
# - "array" a JSON array; in R a plain vector of the leaf type
# - "map"   a string-keyed object; in R a *named* vector of the leaf type
PROP_CONTAINERS <- c("none", "array", "map")


# %% new_prop_spec ----
#' Build and validate a property spec
#'
#' The spec is a plain named list rather than an S7 object on purpose: a
#' property is stored in a class definition and written to the package's
#' lazy-load database, and an S7 object would carry a copy of its whole class
#' definition along with it, once per property.
#'
#' @param type Character: One of `PROP_TYPES`.
#' @param default Default value, or NULL for none.
#' @param minimum,maximum Numeric or NULL: Inclusive bounds.
#' @param exclusive_minimum,exclusive_maximum Numeric or NULL: Exclusive bounds.
#' @param enum Character or NULL: Allowed values.
#' @param const Scalar or NULL: The single value the property is fixed to.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param container Character: One of `PROP_CONTAINERS`.
#' @param min_items Integer: Fewest elements a non-scalar container may hold.
#' @param unique_items Logical: If TRUE, a container's elements must be distinct.
#' @param allow_empty Logical: String type only. If TRUE, `""` is a valid value.
#' @param description Character: Human-readable description.
#'
#' @return Named list of spec fields.
#'
#' @author EDG
#' @keywords internal
#' @noRd
new_prop_spec <- function(
  type,
  default = NULL,
  minimum = NULL,
  maximum = NULL,
  exclusive_minimum = NULL,
  exclusive_maximum = NULL,
  enum = NULL,
  const = NULL,
  nullable = FALSE,
  container = "none",
  min_items = 1L,
  unique_items = FALSE,
  allow_empty = FALSE,
  description = ""
) {
  if (!type %in% PROP_TYPES) {
    abort(
      "`type` must be one of ",
      paste(PROP_TYPES, collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (!container %in% PROP_CONTAINERS) {
    abort(
      "`container` must be one of ",
      paste(PROP_CONTAINERS, collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  check_logical_scalar(nullable)
  check_logical_scalar(unique_items)
  check_logical_scalar(allow_empty)
  check_pos_integer_scalar(min_items)
  # Not `check_character_scalar()`: the empty string is the "undocumented"
  # default here, and that helper rejects it.
  if (
    !is.character(description) ||
      length(description) != 1L ||
      is.na(description)
  ) {
    abort(
      "`description` must be a single string.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (!is.null(enum)) {
    if (type != "string") {
      abort(
        "`enum` applies to string properties only.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    check_character(enum, allow_null = FALSE, arg_name = "enum")
  }
  fields <- list(
    type = type,
    default = default,
    minimum = minimum,
    maximum = maximum,
    exclusive_minimum = exclusive_minimum,
    exclusive_maximum = exclusive_maximum,
    enum = enum,
    const = const,
    nullable = nullable,
    container = container,
    min_items = as.integer(min_items),
    unique_items = unique_items,
    allow_empty = allow_empty,
    description = description
  )
  # A default that does not satisfy its own spec is a declaration error, so it
  # should fail when the package loads rather than at first instantiation.
  # `default = NULL` on a non-nullable property means "no default supplied",
  # not "the value NULL", so it is not checked.
  if (!is.null(default)) {
    msg <- validate_with_spec(default, fields)
    if (!is.null(msg)) {
      abort(
        "`default` ",
        msg,
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
  fields
} # /rtemis.core::new_prop_spec


# %% validate_keys ----
#' Validate a value's names as JSON object keys
#'
#' Shared by the "map" container and the "object" scalar, both of which become
#' a JSON object. A key exists to address one element, so a key that is
#' missing, empty, blank or repeated is rejected here rather than reaching a
#' backend. R permits repeated names, but they lose data: `x[["a"]]` returns
#' the first match and no name reaches the rest, while `{"a": 1, "a": 2}` is
#' left undefined by RFC 8259 and resolved differently by different parsers.
#'
#' @param value Named vector or list.
#'
#' @return NULL if every name is a usable key, otherwise character error
#'   message.
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_keys <- function(value) {
  keys <- names(value)
  if (is.null(keys)) {
    return("must be named.")
  }
  if (anyNA(keys) || !all(nzchar(trimws(keys)))) {
    return("must have a non-empty name for every element.")
  }
  repeated <- unique(keys[duplicated(keys)])
  if (length(repeated) > 0L) {
    return(paste0(
      "must have distinct names (",
      paste0("'", repeated, "'", collapse = ", "),
      if (length(repeated) == 1L) " is repeated)." else " are repeated)."
    ))
  }
  NULL
} # /rtemis.core::validate_keys


# %% validate_with_spec ----
#' Validate a property value against its spec
#'
#' Shared validator body for all factory-built properties. Returns NULL if the
#' value is valid, otherwise a character message, per the S7 validator
#' contract. The property's S7 class already enforces the base type; this
#' checks arity, missingness, bounds and enum membership.
#'
#' Reads its fields with `[[`, so a spec carrying additional fields -- rtemis's,
#' for instance -- validates against the generic ones and ignores the rest.
#'
#' @param value Property value being set.
#' @param fields Named list of spec fields, from `new_prop_spec()`.
#'
#' @return NULL if valid, otherwise character error message.
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_with_spec <- function(value, fields) {
  nullable <- isTRUE(fields[["nullable"]])
  container <- fields[["container"]]
  type <- fields[["type"]]
  if (is.null(value)) {
    return(if (nullable) NULL else "must not be NULL.")
  }
  object_scalar <- type == "object" && container == "none"
  if (object_scalar && !is.list(value)) {
    return("must be a list.")
  }
  if (length(value) == 0L) {
    # NULL is the only "unset" value: nullable properties declare their class
    # as `NULL | <base>` so S7 prototypes them to NULL rather than to the base
    # class's empty vector. An empty vector reaching here is a real value and
    # is rejected, so `!is.null()` guards downstream stay meaningful.
    return(
      if (nullable) {
        "must not be empty (use NULL to leave it unset)."
      } else {
        "must not be empty."
      }
    )
  }
  # A JSON object is keyed by strings, so every element of a "map" -- and of an
  # "object" scalar, which is one object however many keys it holds -- must
  # carry a usable key.
  if (object_scalar || container == "map") {
    msg <- validate_keys(value)
    if (!is.null(msg)) {
      return(msg)
    }
    if (object_scalar) {
      # Past its keys, an "object" scalar's contents are the consumer's
      # contract and not ours, so the leaf checks below do not apply to it.
      return(NULL)
    }
  }
  if (container == "none" && length(value) > 1L) {
    return("must be a single value.")
  }
  if (anyNA(value)) {
    return("must not contain missing values.")
  }
  if (container != "none") {
    min_items <- fields[["min_items"]]
    if (!is.null(min_items) && length(value) < min_items) {
      return(paste0(
        "must have at least ",
        min_items,
        if (min_items == 1L) " element." else " elements."
      ))
    }
    if (isTRUE(fields[["unique_items"]]) && anyDuplicated(value)) {
      return("must have distinct elements.")
    }
  }
  if (type == "number" && !all(is.finite(value))) {
    return("must be finite.")
  }
  if (type == "string" && !isTRUE(fields[["allow_empty"]])) {
    if (!all(nzchar(trimws(value)))) {
      return("must not be an empty string.")
    }
  }
  const <- fields[["const"]]
  if (!is.null(const) && !all(value == const)) {
    return(paste0("must be ", paste(deparse(const), collapse = ""), "."))
  }
  minimum <- fields[["minimum"]]
  maximum <- fields[["maximum"]]
  exclusive_minimum <- fields[["exclusive_minimum"]]
  exclusive_maximum <- fields[["exclusive_maximum"]]
  enum <- fields[["enum"]]
  if (!is.null(minimum) && any(value < minimum)) {
    return(paste0("must be >= ", minimum, "."))
  }
  if (!is.null(maximum) && any(value > maximum)) {
    return(paste0("must be <= ", maximum, "."))
  }
  if (!is.null(exclusive_minimum) && any(value <= exclusive_minimum)) {
    return(paste0("must be > ", exclusive_minimum, "."))
  }
  if (!is.null(exclusive_maximum) && any(value >= exclusive_maximum)) {
    return(paste0("must be < ", exclusive_maximum, "."))
  }
  if (!is.null(enum) && !all(value %in% enum)) {
    return(paste0(
      "must be one of ",
      paste0("'", enum, "'", collapse = ", "),
      "."
    ))
  }
  NULL
} # /rtemis.core::validate_with_spec


# %% spec_validator ----
#' Build a property's validator over its spec fields
#'
#' A factory rather than an inline closure so that the validator's environment
#' holds the fields and nothing else. An inline closure would capture the whole
#' calling frame, and anything reachable from it is written to the lazy-load
#' database alongside the validator.
#'
#' @param fields Named list of spec fields, from `new_prop_spec()`.
#'
#' @return Function of one argument, suitable as an S7 property validator.
#'
#' @author EDG
#' @keywords internal
#' @noRd
spec_validator <- function(fields) {
  force(fields)
  function(value) validate_with_spec(value, fields)
} # /rtemis.core::spec_validator


# %% make_prop ----
#' Build an S7 property from a spec
#'
#' Engine behind the `prop_*` factories: derives the property's S7 class from
#' the spec's type (union with NULL when nullable, NULL *first* so that S7
#' prototypes it to NULL), installs a spec-driven validator, and stores the
#' spec on the property under `"spec"`. S7 properties are named lists and S7
#' reads its own fields by name, so the extra element is inert to S7 itself.
#'
#' @param fields Named list of spec fields, from `new_prop_spec()`.
#'
#' @return S7 property, with the spec attached as `$spec`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
make_prop <- function(fields) {
  base_class <- switch(
    fields[["type"]],
    boolean = class_logical,
    integer = class_integer,
    number = class_numeric,
    string = class_character,
    object = class_list
  )
  p <- new_property(
    class = if (fields[["nullable"]]) NULL | base_class else base_class,
    default = fields[["default"]],
    validator = spec_validator(fields)
  )
  p[["spec"]] <- fields
  p
} # /rtemis.core::make_prop


# %% prop_boolean ----
#' Logical (boolean) S7 property
#'
#' @param default Logical: Default value (NULL only if `nullable`).
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @export
#'
#' @examples
#' verbose <- prop_boolean(default = TRUE, description = "Print progress")
prop_boolean <- function(
  default = FALSE,
  nullable = FALSE,
  description = ""
) {
  make_prop(new_prop_spec(
    type = "boolean",
    default = default,
    nullable = nullable,
    description = description
  ))
} # /rtemis.core::prop_boolean


# %% prop_integer ----
#' Integer S7 property
#'
#' Accepts R integers only (`3L`, not `3`), so that a whole-number contract is
#' enforced by the type rather than by a rounding check.
#'
#' @param default Integer: Default value (NULL for none, or if `nullable`).
#' @param min,max Integer or NULL: Inclusive bounds.
#' @param exclusive_min,exclusive_max Integer or NULL: Exclusive bounds.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param vector Logical: If TRUE, the value is vector-valued (a JSON array).
#' @param min_items Integer: Fewest elements a `vector` value may hold.
#' @param unique_items Logical: If TRUE, a `vector` value's elements must be
#'   distinct.
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @export
#'
#' @examples
#' n_iter <- prop_integer(default = 100L, min = 1L, description = "Iterations")
prop_integer <- function(
  default = NULL,
  min = NULL,
  max = NULL,
  exclusive_min = NULL,
  exclusive_max = NULL,
  nullable = FALSE,
  vector = FALSE,
  min_items = 1L,
  unique_items = FALSE,
  description = ""
) {
  make_prop(new_prop_spec(
    type = "integer",
    default = default,
    minimum = min,
    maximum = max,
    exclusive_minimum = exclusive_min,
    exclusive_maximum = exclusive_max,
    nullable = nullable,
    container = if (vector) "array" else "none",
    min_items = min_items,
    unique_items = unique_items,
    description = description
  ))
} # /rtemis.core::prop_integer


# %% prop_float ----
#' Numeric (floating-point) S7 property
#'
#' The one factory whose name differs from its JSON Schema type: it emits type
#' "number" (which in JSON Schema includes integers), but is named `prop_float`
#' because declarers think in the integer/float pairing -- "number" next to
#' `prop_integer` invites the same ambiguity as R's "numeric". Accepts R
#' integer values too (`class_numeric`), floats being a superset of integers.
#'
#' @param default Numeric: Default value (NULL for none, or if `nullable`).
#' @param min,max Numeric or NULL: Inclusive bounds.
#' @param exclusive_min,exclusive_max Numeric or NULL: Exclusive bounds.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param vector Logical: If TRUE, the value is vector-valued (a JSON array).
#' @param min_items Integer: Fewest elements a `vector` value may hold.
#' @param unique_items Logical: If TRUE, a `vector` value's elements must be
#'   distinct.
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Sampling temperature in [0, 2]
#' temperature <- prop_float(default = 0.3, min = 0, max = 2)
#' # Learning rate in (0, 1]
#' lr <- prop_float(default = 0.1, exclusive_min = 0, max = 1)
prop_float <- function(
  default = NULL,
  min = NULL,
  max = NULL,
  exclusive_min = NULL,
  exclusive_max = NULL,
  nullable = FALSE,
  vector = FALSE,
  min_items = 1L,
  unique_items = FALSE,
  description = ""
) {
  make_prop(new_prop_spec(
    type = "number",
    default = default,
    minimum = min,
    maximum = max,
    exclusive_minimum = exclusive_min,
    exclusive_maximum = exclusive_max,
    nullable = nullable,
    container = if (vector) "array" else "none",
    min_items = min_items,
    unique_items = unique_items,
    description = description
  ))
} # /rtemis.core::prop_float


# %% prop_string ----
#' Character (string) S7 property
#'
#' Empty and whitespace-only strings are rejected by default: an unset value is
#' `NULL`, so `""` reaching a property is almost always a mistake rather than a
#' deliberate empty name. Pass `allow_empty = TRUE` where it is meaningful.
#'
#' @param default Character: Default value (NULL for none, or if `nullable`).
#' @param enum Character or NULL: Allowed values.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param vector Logical: If TRUE, the value is vector-valued (a JSON array).
#' @param map Logical: If TRUE, the value is a *named* vector (a JSON object
#'   with string values). Mutually exclusive with `vector`.
#' @param min_items Integer: Fewest elements a `vector` or `map` value may hold.
#' @param unique_items Logical: If TRUE, a `vector` value's elements must be
#'   distinct.
#' @param allow_empty Logical: If TRUE, `""` is a valid value.
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @export
#'
#' @examples
#' model_name <- prop_string(description = "Model name")
#' backend <- prop_string(default = "ollama", enum = c("ollama", "openai"))
#' api_key <- prop_string(nullable = TRUE, description = "API key")
prop_string <- function(
  default = NULL,
  enum = NULL,
  nullable = FALSE,
  vector = FALSE,
  map = FALSE,
  min_items = 1L,
  unique_items = FALSE,
  allow_empty = FALSE,
  description = ""
) {
  check_logical_scalar(vector)
  check_logical_scalar(map)
  if (vector && map) {
    abort(
      "`vector` and `map` are mutually exclusive.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  make_prop(new_prop_spec(
    type = "string",
    default = default,
    enum = enum,
    nullable = nullable,
    container = if (vector) {
      "array"
    } else if (map) {
      "map"
    } else {
      "none"
    },
    min_items = min_items,
    unique_items = unique_items,
    allow_empty = allow_empty,
    description = description
  ))
} # /rtemis.core::prop_string


# %% prop_bag ----
#' Opaque named list S7 property
#'
#' A pass-through for values with no per-key contract of our own -- extra
#' request headers, backend-specific options -- carried as one value however
#' many keys it holds. The keys are required: the value becomes a JSON object,
#' so every element must carry a name, and the names must be distinct. Use
#' NULL, not `list()`, for no value.
#'
#' @param default List: Default value (NULL for none, or if `nullable`).
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @export
#'
#' @examples
#' extra_headers <- prop_bag(nullable = TRUE, description = "Extra headers")
prop_bag <- function(
  default = NULL,
  nullable = TRUE,
  description = ""
) {
  make_prop(new_prop_spec(
    type = "object",
    default = default,
    nullable = nullable,
    description = description
  ))
} # /rtemis.core::prop_bag


# %% prop_const ----
#' Constant S7 property
#'
#' A property fixed to one value: it is the only value that validates, and it
#' is the default, so the declaration is the whole contract.
#'
#' @param value Logical, numeric or character scalar: The value.
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @export
#'
#' @examples
#' type <- prop_const("object", description = "JSON Schema type")
prop_const <- function(value, description = "") {
  if (length(value) != 1L || is.na(value)) {
    abort(
      "`value` must be a single non-NA scalar.",
      class = c("rtemis_length_error", "rtemis_input_error")
    )
  }
  type <- if (is.character(value)) {
    "string"
  } else if (is.logical(value)) {
    "boolean"
  } else if (is.integer(value)) {
    "integer"
  } else if (is.numeric(value)) {
    "number"
  } else {
    abort(
      "`value` must be logical, numeric or character.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  make_prop(new_prop_spec(
    type = type,
    default = value,
    const = value,
    # The constant is the contract; an empty string is a legitimate one.
    allow_empty = TRUE,
    description = description
  ))
} # /rtemis.core::prop_const


# %% prop_spec ----
#' A factory-built property's spec
#'
#' The machine-readable declaration behind a property: type, default, bounds,
#' enum and description. Read it to generate documentation, a JSON Schema, or a
#' defaults artifact from the class definition itself.
#'
#' @param property S7 property, from one of the `prop_*` factories.
#'
#' @return Named list of spec fields, or NULL if the property was not built by
#'   a `prop_*` factory.
#'
#' @author EDG
#' @export
#'
#' @examples
#' temperature <- prop_float(default = 0.3, min = 0, max = 2)
#' prop_spec(temperature)[["maximum"]]
prop_spec <- function(property) {
  if (!inherits(property, "S7_property")) {
    abort(
      "`property` must be an S7 property.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  property[["spec"]]
} # /rtemis.core::prop_spec
