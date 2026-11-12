# 2026- EDG rtemis.org

# TOC ----
# Spec ------------------------------------------------------------------------
#   PROP_TYPES / PROP_CONTAINERS
#   new_prop_spec / spec_shape
#   validate_keys / validate_with_spec / validate_value / spec_validator
#   make_prop
# Scalar factories ------------------------------------------------------------
#   prop_boolean
#   prop_integer
#   prop_float
#   prop_string
#   prop_bag
#   prop_const
# Structural factories --------------------------------------------------------
#   prop_object
#   prop_collection
#   prop_array
#   prop_map
#   prop_struct
#   prop_union
#   prop_matrix
# Introspection ---------------------------------------------------------------
#   prop_spec
#
# One declaration carries a property's type, shape, default, bounds, enum, and
# description; the S7 validator is generated from it, and the JSON Schema
# emitter in `schema.R` reads the same declaration. Field names match rtemis's
# `PropertySpec` where the two describe the same construct.

# %% PROP_TYPES ----
# JSON Schema type of a property's value.
# - "boolean", "integer", "number", "string": a leaf value, or the leaf of a
#   vector, map, or matrix container.
# - "object": a JSON object -- an opaque named list (`prop_bag()`), an S7
#   object or collection of them (`prop_object()`, `prop_collection()`), a map
#   of a nested spec (`prop_map()`), or a declared struct (`prop_struct()`).
# - "array": a JSON array of a nested spec (`prop_array()`).
# - "union": a value matching at least one alternative spec (`prop_union()`).
PROP_TYPES <- c(
  "boolean",
  "integer",
  "number",
  "string",
  "object",
  "array",
  "union"
)

# %% PROP_CONTAINERS ----
# How a property's values are wrapped.
# - "none"   a single value
# - "array"  a JSON array; in R a plain vector of the leaf type, or a list when
#            the elements are S7 objects or a nested spec (`items`)
# - "map"    a string-keyed object; in R a *named* vector of the leaf type, or a
#            named list when the values are S7 objects or a nested spec
# - "matrix" a JSON array of fixed-length arrays; in R a matrix with `ncol`
#            columns, one row per inner array
# - "struct" a JSON object with declared members; in R a named list
PROP_CONTAINERS <- c("none", "array", "map", "matrix", "struct")


# %% new_prop_spec ----
#' Build and validate a property spec
#'
#' The spec is a plain named list rather than an S7 object: a property is
#' stored in a class definition and written to the package's lazy-load
#' database, and an S7 object would carry a copy of its whole class definition
#' along with it, once per property.
#'
#' @param type Character: One of `PROP_TYPES`.
#' @param default Default value, or NULL for none.
#' @param minimum,maximum Numeric or NULL: Inclusive bounds.
#' @param exclusive_minimum,exclusive_maximum Numeric or NULL: Exclusive bounds.
#' @param enum Character or NULL: Allowed values.
#' @param const Scalar or NULL: The single value the property is fixed to.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param container Character: One of `PROP_CONTAINERS`.
#' @param min_items Integer: Fewest elements, entries, or rows a container may
#'   hold. `0L` admits an empty container.
#' @param max_items Integer or NULL: Most elements, entries, or rows a container
#'   may hold.
#' @param unique_items Logical: If TRUE, a container's elements must be distinct.
#' @param allow_empty Logical: String type only. If TRUE, `""` is a valid value.
#' @param pattern Character or NULL: String type only. Regular expression every
#'   value must match, evaluated with `perl = TRUE`.
#' @param min_length Integer or NULL: String type only. Fewest characters a
#'   value may have.
#' @param items Named list or NULL: Spec of each element of an "array"
#'   container, or of each cell of a "matrix" container.
#' @param values Named list or NULL: Spec of each value of a "map" container.
#' @param members Named list of specs or NULL: Members of a "struct" container.
#' @param required Character or NULL: Members a "struct" value must contain.
#' @param additional Logical: "struct" container only. If TRUE, members beyond
#'   `members` are accepted and kept as they are.
#' @param alternatives List of specs or NULL: Alternatives of a "union".
#' @param ncol Integer or NULL: Column count of a "matrix" container.
#' @param target S7 class or NULL: Class of the S7 object, or of every element
#'   of a collection.
#' @param def_name Character or NULL: Name under which the emitted JSON Schema
#'   defines this spec once, in `$defs`, and refers to it everywhere it is used.
#' @param description Character: Human-readable description.
#' @param default_env Environment: Where a quoted default is evaluated.
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
  max_items = NULL,
  unique_items = FALSE,
  allow_empty = FALSE,
  pattern = NULL,
  min_length = NULL,
  items = NULL,
  values = NULL,
  members = NULL,
  required = NULL,
  additional = FALSE,
  alternatives = NULL,
  ncol = NULL,
  target = NULL,
  def_name = NULL,
  description = "",
  default_env = parent.frame()
) {
  if (is.call(default)) {
    default <- eval(default, envir = default_env)
  }
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
  check_logical_scalar(additional)
  check_nonneg_count(min_items, "min_items")
  if (!is.null(max_items)) {
    check_nonneg_count(max_items, "max_items")
    if (max_items < min_items) {
      abort(
        "`max_items` must be at least `min_items`.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
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
  if (!is.null(pattern) || !is.null(min_length)) {
    if (type != "string") {
      abort(
        "`pattern` and `min_length` apply to string properties only.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
  if (!is.null(pattern)) {
    if (!is.character(pattern) || length(pattern) != 1L || is.na(pattern)) {
      abort(
        "`pattern` must be a single string.",
        class = c("rtemis_type_error", "rtemis_input_error")
      )
    }
    ok <- tryCatch(
      {
        grepl(pattern, "", perl = TRUE)
        TRUE
      },
      error = function(e) FALSE,
      warning = function(w) FALSE
    )
    if (!ok) {
      abort(
        "`pattern` must be a valid regular expression.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
  if (!is.null(min_length)) {
    check_nonneg_count(min_length, "min_length")
  }
  if (!is.null(def_name)) {
    if (
      !is.character(def_name) ||
        length(def_name) != 1L ||
        is.na(def_name) ||
        !grepl("^[A-Za-z][A-Za-z0-9_]*$", def_name)
    ) {
      abort(
        "`def_name` must be a single name of letters, digits and underscores, ",
        "starting with a letter.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
  if (container == "matrix") {
    check_nonneg_count(ncol, "ncol")
    if (ncol < 1L) {
      abort(
        "`ncol` must be at least 1.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
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
    max_items = if (is.null(max_items)) NULL else as.integer(max_items),
    unique_items = unique_items,
    allow_empty = allow_empty,
    pattern = pattern,
    min_length = if (is.null(min_length)) NULL else as.integer(min_length),
    items = items,
    values = values,
    members = members,
    required = required,
    additional = additional,
    alternatives = alternatives,
    ncol = if (is.null(ncol)) NULL else as.integer(ncol),
    target = target,
    target_class = if (is.null(target)) NULL else target_class_name(target),
    def_name = def_name,
    description = description
  )
  # A default that does not satisfy its own spec is a declaration error, so it
  # fails when the package loads rather than at first instantiation.
  # `default = NULL` on a non-nullable property means "no default supplied",
  # not "the value NULL", so it is not checked.
  # A leaf default's R type is checked by `make_prop()` (quoted defaults) and
  # by S7 (literal defaults), so only the spec is checked here; a nested shape
  # has no S7 class of its own and is checked in full.
  if (!is.null(default)) {
    msg <- if (spec_shape(fields) %in% c("leaf", "bag", "matrix")) {
      validate_with_spec(default, fields)
    } else {
      validate_value(default, fields)
    }
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


# %% check_nonneg_count ----
#' Check a count argument of a spec
#'
#' @param x Value to check.
#' @param arg_name Character: Argument name, for the message.
#'
#' @return NULL, invisibly. Aborts unless `x` is one non-missing, non-negative
#'   whole number.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_nonneg_count <- function(x, arg_name) {
  if (
    !is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      x < 0 ||
      x != round(x)
  ) {
    abort(
      "`",
      arg_name,
      "` must be a single non-negative whole number.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  invisible(NULL)
} # /rtemis.core::check_nonneg_count


# %% target_class_name ----
#' Qualified name of an S7 class
#'
#' @param cls S7 class.
#'
#' @return Character: `"package::Class"` when the class records its package,
#'   otherwise `"Class"`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
target_class_name <- function(cls) {
  pkg <- cls@package
  if (is.null(pkg) || !nzchar(pkg)) cls@name else paste0(pkg, "::", cls@name)
} # /rtemis.core::target_class_name


# %% check_target_class ----
#' Check that an argument is an S7 class
#'
#' @param cls Value to check.
#'
#' @return NULL, invisibly. Aborts unless `cls` is an S7 class.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_target_class <- function(cls) {
  if (!inherits(cls, "S7_class")) {
    abort(
      "`cls` must be an S7 class.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  invisible(NULL)
} # /rtemis.core::check_target_class


# %% nested_spec ----
#' The spec of a factory-built property passed as a nested declaration
#'
#' @param x S7 property, from one of the `prop_*` factories.
#' @param what Character: Argument description, for the message.
#'
#' @return Named list of spec fields.
#'
#' @author EDG
#' @keywords internal
#' @noRd
nested_spec <- function(x, what) {
  spec <- if (inherits(x, "S7_property")) x[["spec"]] else NULL
  if (is.null(spec)) {
    abort(
      "`",
      what,
      "` must be a property built by a prop_* factory.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  spec
} # /rtemis.core::nested_spec


# %% spec_shape ----
#' The shape a spec declares
#'
#' One name per validation and emission branch, derived from `type`,
#' `container` and the nested fields.
#'
#' @param fields Named list of spec fields.
#'
#' @return Character, one of `"union"`, `"struct"`, `"object"`,
#'   `"collection"`, `"nested_array"`, `"nested_map"`, `"matrix"`, `"bag"`,
#'   `"leaf"`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
spec_shape <- function(fields) {
  container <- fields[["container"]]
  type <- fields[["type"]]
  if (identical(type, "union")) {
    return("union")
  }
  if (identical(container, "struct")) {
    return("struct")
  }
  if (identical(container, "matrix")) {
    return("matrix")
  }
  if (!is.null(fields[["target"]])) {
    return(if (identical(container, "none")) "object" else "collection")
  }
  if (identical(container, "array") && !is.null(fields[["items"]])) {
    return("nested_array")
  }
  if (identical(container, "map") && !is.null(fields[["values"]])) {
    return("nested_map")
  }
  if (identical(type, "object") && identical(container, "none")) {
    return("bag")
  }
  "leaf"
} # /rtemis.core::spec_shape


# %% validate_keys ----
#' Validate a value's names as JSON object keys
#'
#' Shared by every shape that becomes a JSON object. A key exists to address
#' one element, so a key that is missing, empty, or repeated is rejected. R
#' permits repeated names, but they lose data: `x[["a"]]` returns the first
#' match, while `{"a": 1, "a": 2}` is left undefined by RFC 8259 and resolved
#' differently by different parsers. The rule matches the emitted
#' `propertyNames: {minLength: 1}`, so a key of spaces is valid.
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
  if (anyNA(keys) || !all(nzchar(keys))) {
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


# %% validate_count ----
#' Validate a container's size against its bounds
#'
#' @param n Integer: Number of elements, entries, or rows.
#' @param fields Named list of spec fields.
#'
#' @return NULL if valid, otherwise character error message.
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_count <- function(n, fields) {
  min_items <- fields[["min_items"]]
  max_items <- fields[["max_items"]]
  unit <- if (identical(fields[["container"]], "matrix")) "row" else "element"
  if (!is.null(min_items) && n < min_items) {
    return(paste0(
      "must have at least ",
      min_items,
      " ",
      unit,
      if (min_items == 1L) "." else "s."
    ))
  }
  if (!is.null(max_items) && n > max_items) {
    return(paste0(
      "must have at most ",
      max_items,
      " ",
      unit,
      if (max_items == 1L) "." else "s."
    ))
  }
  NULL
} # /rtemis.core::validate_count


# %% validate_leaf_type ----
#' Validate the R type of a value against a leaf JSON type
#'
#' A top-level property's S7 class enforces its base type. An element of a
#' list container has no S7 class of its own, so its type is checked here.
#'
#' @param value Value being checked.
#' @param type Character: One of `"boolean"`, `"integer"`, `"number"`,
#'   `"string"`.
#'
#' @return NULL if valid, otherwise character error message.
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_leaf_type <- function(value, type) {
  ok <- switch(
    type,
    boolean = is.logical(value),
    integer = is.integer(value),
    number = is.numeric(value),
    string = is.character(value),
    TRUE
  )
  if (ok) {
    NULL
  } else {
    paste0("must be of type ", type, ".")
  }
} # /rtemis.core::validate_leaf_type


# %% validate_value ----
#' Validate a value against a spec, including its R type
#'
#' The entry point for nested values -- struct members, array elements, map
#' values and union alternatives -- which carry no S7 class of their own. It
#' checks what `make_prop()` would have delegated to S7 and then applies
#' `validate_with_spec()`.
#'
#' @param value Value being checked.
#' @param fields Named list of spec fields.
#'
#' @return NULL if valid, otherwise character error message.
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_value <- function(value, fields) {
  if (!is.null(value)) {
    shape <- spec_shape(fields)
    if (shape == "leaf" || shape == "matrix") {
      msg <- validate_leaf_type(value, fields[["type"]])
      if (!is.null(msg)) {
        return(msg)
      }
    }
    if (shape == "object" && !S7_inherits(value, fields[["target"]])) {
      return(paste0("must be a ", fields[["target"]]@name, " object."))
    }
  }
  validate_with_spec(value, fields)
} # /rtemis.core::validate_value


# %% validate_with_spec ----
#' Validate a property value against its spec
#'
#' Shared validator body for all factory-built properties. Returns NULL if the
#' value is valid, otherwise a character message, per the S7 validator
#' contract. A top-level property's S7 class already enforces the base type;
#' this checks shape, arity, missingness, bounds, and enum membership, and
#' descends into nested specs through `validate_value()`.
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
  if (is.null(value)) {
    return(if (nullable) NULL else "must not be NULL.")
  }
  shape <- spec_shape(fields)
  switch(
    shape,
    union = validate_union(value, fields),
    struct = validate_struct(value, fields),
    object = NULL,
    collection = validate_collection(value, fields),
    nested_array = validate_nested_array(value, fields),
    nested_map = validate_nested_map(value, fields),
    matrix = validate_matrix(value, fields),
    validate_leaf(value, fields)
  )
} # /rtemis.core::validate_with_spec


# %% validate_leaf ----
#' Validate a leaf value, or a vector or map of leaf values
#'
#' Also validates the "object" scalar of `prop_bag()`, whose contents beyond
#' its keys are the consumer's contract.
#'
#' @param value Property value being set.
#' @param fields Named list of spec fields.
#'
#' @return NULL if valid, otherwise character error message.
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_leaf <- function(value, fields) {
  nullable <- isTRUE(fields[["nullable"]])
  container <- fields[["container"]]
  type <- fields[["type"]]
  object_scalar <- type == "object" && container == "none"
  if (object_scalar && !is.list(value)) {
    return("must be a list.")
  }
  if (length(value) == 0L) {
    # A vector or map container whose `min_items` is 0 holds an empty value
    # legitimately: `[]` or `{}`.
    if (container != "none" && identical(fields[["min_items"]], 0L)) {
      return(NULL)
    }
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
    msg <- validate_count(length(value), fields)
    if (!is.null(msg)) {
      return(msg)
    }
    if (isTRUE(fields[["unique_items"]]) && anyDuplicated(value)) {
      return("must have distinct elements.")
    }
  }
  if (type == "number" && !all(is.finite(value))) {
    return("must be finite.")
  }
  if (type == "string") {
    if (!isTRUE(fields[["allow_empty"]]) && !all(nzchar(trimws(value)))) {
      return("must not be an empty string.")
    }
    min_length <- fields[["min_length"]]
    if (!is.null(min_length) && any(nchar(value) < min_length)) {
      return(paste0(
        "must have at least ",
        min_length,
        if (min_length == 1L) " character." else " characters."
      ))
    }
    pattern <- fields[["pattern"]]
    if (!is.null(pattern) && !all(grepl(pattern, value, perl = TRUE))) {
      return(paste0("must match the pattern '", pattern, "'."))
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
} # /rtemis.core::validate_leaf


# %% validate_union ----
#' Validate a value against the alternatives of a union
#'
#' @param value Property value being set.
#' @param fields Named list of spec fields.
#'
#' @return NULL if `value` satisfies at least one alternative, otherwise
#'   character error message listing each alternative's objection.
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_union <- function(value, fields) {
  msgs <- character(0L)
  for (alt in fields[["alternatives"]]) {
    msg <- validate_value(value, alt)
    if (is.null(msg)) {
      return(NULL)
    }
    msgs <- c(msgs, msg)
  }
  paste0(
    "must match one of ",
    length(msgs),
    " alternatives (",
    paste0(seq_along(msgs), ": ", msgs, collapse = "; "),
    ")."
  )
} # /rtemis.core::validate_union


# %% validate_struct ----
#' Validate a named list against declared struct members
#'
#' @param value Property value being set.
#' @param fields Named list of spec fields.
#'
#' @return NULL if valid, otherwise character error message.
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_struct <- function(value, fields) {
  if (!is.list(value) || is.object(value)) {
    return("must be a named list.")
  }
  members <- fields[["members"]]
  present <- names(value)
  if (length(value) > 0L) {
    msg <- validate_keys(value)
    if (!is.null(msg)) {
      return(msg)
    }
  } else {
    present <- character(0L)
  }
  missing_members <- setdiff(fields[["required"]], present)
  if (length(missing_members) > 0L) {
    return(paste0(
      "must have member",
      if (length(missing_members) > 1L) "s " else " ",
      paste0("'", missing_members, "'", collapse = ", "),
      "."
    ))
  }
  unknown <- setdiff(present, names(members))
  if (length(unknown) > 0L && !isTRUE(fields[["additional"]])) {
    return(paste0(
      "must not have member",
      if (length(unknown) > 1L) "s " else " ",
      paste0("'", unknown, "'", collapse = ", "),
      "; allowed members are ",
      paste0("'", names(members), "'", collapse = ", "),
      "."
    ))
  }
  for (nm in intersect(names(members), present)) {
    msg <- validate_value(value[[nm]], members[[nm]])
    if (!is.null(msg)) {
      return(paste0("member '", nm, "' ", msg))
    }
  }
  NULL
} # /rtemis.core::validate_struct


# %% validate_elements ----
#' Validate every element of a list against one spec
#'
#' @param value List.
#' @param fields Named list: Spec every element must satisfy.
#' @param label Character: `"element"` or `"entry"`, for the message.
#'
#' @return NULL if valid, otherwise character error message naming the first
#'   failing element by position or key.
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_elements <- function(value, fields, label) {
  keys <- names(value)
  for (i in seq_along(value)) {
    msg <- validate_value(value[[i]], fields)
    if (!is.null(msg)) {
      where <- if (label == "entry") {
        paste0("entry '", keys[[i]], "'")
      } else {
        paste0("element ", i)
      }
      return(paste0(where, " ", msg))
    }
  }
  NULL
} # /rtemis.core::validate_elements


# %% validate_collection ----
#' Validate an array or map of S7 objects
#'
#' @param value Property value being set.
#' @param fields Named list of spec fields.
#'
#' @return NULL if valid, otherwise character error message.
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_collection <- function(value, fields) {
  if (!is.list(value) || is.object(value)) {
    return("must be a list.")
  }
  map <- identical(fields[["container"]], "map")
  if (map && length(value) > 0L) {
    msg <- validate_keys(value)
    if (!is.null(msg)) {
      return(msg)
    }
  }
  msg <- validate_count(length(value), fields)
  if (!is.null(msg)) {
    return(msg)
  }
  target <- fields[["target"]]
  keys <- names(value)
  for (i in seq_along(value)) {
    if (!S7_inherits(value[[i]], target)) {
      where <- if (map) {
        paste0("entry '", keys[[i]], "'")
      } else {
        paste0("element ", i)
      }
      return(paste0(where, " must be a ", target@name, " object."))
    }
  }
  NULL
} # /rtemis.core::validate_collection


# %% validate_nested_array ----
#' Validate a list whose every element satisfies a nested spec
#'
#' @param value Property value being set.
#' @param fields Named list of spec fields.
#'
#' @return NULL if valid, otherwise character error message.
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_nested_array <- function(value, fields) {
  if (!is.list(value) || is.object(value)) {
    return("must be a list.")
  }
  msg <- validate_count(length(value), fields)
  if (!is.null(msg)) {
    return(msg)
  }
  if (isTRUE(fields[["unique_items"]]) && anyDuplicated(unname(value))) {
    return("must have distinct elements.")
  }
  validate_elements(value, fields[["items"]], "element")
} # /rtemis.core::validate_nested_array


# %% validate_nested_map ----
#' Validate a named list whose every value satisfies a nested spec
#'
#' @param value Property value being set.
#' @param fields Named list of spec fields.
#'
#' @return NULL if valid, otherwise character error message.
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_nested_map <- function(value, fields) {
  if (!is.list(value) || is.object(value)) {
    return("must be a named list.")
  }
  if (length(value) > 0L) {
    msg <- validate_keys(value)
    if (!is.null(msg)) {
      return(msg)
    }
  }
  msg <- validate_count(length(value), fields)
  if (!is.null(msg)) {
    return(msg)
  }
  validate_elements(value, fields[["values"]], "entry")
} # /rtemis.core::validate_nested_map


# %% validate_matrix ----
#' Validate a fixed-width matrix and its cells
#'
#' @param value Property value being set.
#' @param fields Named list of spec fields.
#'
#' @return NULL if valid, otherwise character error message.
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_matrix <- function(value, fields) {
  if (!is.matrix(value)) {
    return("must be a matrix.")
  }
  ncol <- fields[["ncol"]]
  if (ncol(value) != ncol) {
    return(paste0(
      "must have ",
      ncol,
      if (ncol == 1L) " column." else " columns."
    ))
  }
  msg <- validate_count(nrow(value), fields)
  if (!is.null(msg)) {
    return(msg)
  }
  if (length(value) == 0L) {
    return(NULL)
  }
  cells <- fields[["items"]]
  cells[["container"]] <- "array"
  cells[["min_items"]] <- 0L
  cells[["max_items"]] <- NULL
  cells[["unique_items"]] <- FALSE
  msg <- validate_leaf(as.vector(value), cells)
  if (!is.null(msg)) {
    return(paste0("cells ", msg))
  }
  NULL
} # /rtemis.core::validate_matrix


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
#' the spec's shape (union with NULL when nullable, NULL *first* so that S7
#' prototypes it to NULL), installs a spec-driven validator, and stores the
#' spec on the property under `"spec"`. S7 properties are named lists and S7
#' reads its own fields by name, so the extra element is inert to S7 itself.
#'
#' @param fields Named list of spec fields, from `new_prop_spec()`.
#' @param default Default value or quoted call.
#'
#' @return S7 property, with the spec attached as `$spec`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
make_prop <- function(fields, default = fields[["default"]]) {
  shape <- spec_shape(fields)
  base_class <- switch(
    shape,
    union = class_any,
    object = fields[["target"]],
    struct = ,
    collection = ,
    nested_array = ,
    nested_map = class_list,
    switch(
      fields[["type"]],
      boolean = class_logical,
      integer = class_integer,
      number = class_numeric,
      string = class_character,
      object = class_list
    )
  )
  if (is.call(default) && shape %in% c("leaf", "bag", "matrix")) {
    types <- switch(
      fields[["type"]],
      boolean = "logical",
      integer = "integer",
      number = c("integer", "double"),
      string = "character",
      object = "list"
    )
    if (fields[["nullable"]]) {
      types <- c(types, "NULL")
    }
    value_type <- typeof(fields[["default"]])
    if (!value_type %in% types) {
      abort(
        "`default` must be of type <",
        paste(types, collapse = "/"),
        ">, not <",
        value_type,
        ">.",
        class = c("rtemis_type_error", "rtemis_input_error")
      )
    }
  }
  # class_any already admits NULL.
  cls <- if (fields[["nullable"]] && shape != "union") {
    NULL | base_class
  } else {
    base_class
  }
  p <- new_property(
    class = cls,
    default = default,
    validator = spec_validator(fields)
  )
  p[["spec"]] <- fields
  p
} # /rtemis.core::make_prop


# %% prop_boolean ----
#' Logical (boolean) S7 property
#'
#' @param default Logical: Default value. `FALSE` unless `nullable`, in which
#'   case NULL (unset), as for every other factory.
#'   Use a quoted call for a default that needs construction. The call is
#'   evaluated once in the caller's environment to validate
#'   the default and record it in [prop_spec()], then again for each object in
#'   the class's environment.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param def_name Character or NULL: Name of this declaration's definition in
#'   an emitted JSON Schema's `$defs`; see [class_to_JSONSchema()].
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @export
#'
#' @examples
#' verbose <- prop_boolean(default = TRUE, description = "Print progress")
#' # Unset until the user chooses
#' shuffle <- prop_boolean(nullable = TRUE)
#' prop_spec(shuffle)[["default"]]
prop_boolean <- function(
  default = if (nullable) NULL else FALSE,
  nullable = FALSE,
  def_name = NULL,
  description = ""
) {
  fields <- new_prop_spec(
    type = "boolean",
    default = default,
    nullable = nullable,
    def_name = def_name,
    description = description,
    default_env = parent.frame()
  )
  make_prop(fields, default = default)
} # /rtemis.core::prop_boolean


# %% prop_integer ----
#' Integer S7 property
#'
#' Accepts R integers only (`3L`, not `3`), so that a whole-number contract is
#' enforced by the type rather than by a rounding check.
#'
#' @param default Integer: Default value (NULL for none, or if `nullable`).
#'   Use a quoted call for a default that needs construction. The call is
#'   evaluated once in the caller's environment to validate
#'   the default and record it in [prop_spec()], then again for each object in
#'   the class's environment.
#' @param min,max Integer or NULL: Inclusive bounds.
#' @param exclusive_min,exclusive_max Integer or NULL: Exclusive bounds.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param vector Logical: If TRUE, the value is vector-valued (a JSON array).
#' @param min_items Integer: Fewest elements a `vector` value may hold. `0L`
#'   admits an empty vector.
#' @param max_items Integer or NULL: Most elements a `vector` value may hold.
#' @param unique_items Logical: If TRUE, a `vector` value's elements must be
#'   distinct.
#' @param def_name Character or NULL: Name of this declaration's definition in
#'   an emitted JSON Schema's `$defs`; see [class_to_JSONSchema()].
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @export
#'
#' @examples
#' n_iter <- prop_integer(default = 100L, min = 1L, description = "Iterations")
#' # Distinct 1-based positions; an empty set is valid
#' positions <- prop_integer(
#'   min = 1L,
#'   vector = TRUE,
#'   min_items = 0L,
#'   unique_items = TRUE
#' )
prop_integer <- function(
  default = NULL,
  min = NULL,
  max = NULL,
  exclusive_min = NULL,
  exclusive_max = NULL,
  nullable = FALSE,
  vector = FALSE,
  min_items = 1L,
  max_items = NULL,
  unique_items = FALSE,
  def_name = NULL,
  description = ""
) {
  fields <- new_prop_spec(
    type = "integer",
    default = default,
    minimum = min,
    maximum = max,
    exclusive_minimum = exclusive_min,
    exclusive_maximum = exclusive_max,
    nullable = nullable,
    container = if (vector) "array" else "none",
    min_items = min_items,
    max_items = max_items,
    unique_items = unique_items,
    def_name = def_name,
    description = description,
    default_env = parent.frame()
  )
  make_prop(fields, default = default)
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
#'   Use a quoted call for a default that needs construction. The call is
#'   evaluated once in the caller's environment to validate
#'   the default and record it in [prop_spec()], then again for each object in
#'   the class's environment.
#' @param min,max Numeric or NULL: Inclusive bounds.
#' @param exclusive_min,exclusive_max Numeric or NULL: Exclusive bounds.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param vector Logical: If TRUE, the value is vector-valued (a JSON array).
#' @param min_items Integer: Fewest elements a `vector` value may hold. `0L`
#'   admits an empty vector.
#' @param max_items Integer or NULL: Most elements a `vector` value may hold.
#' @param unique_items Logical: If TRUE, a `vector` value's elements must be
#'   distinct.
#' @param def_name Character or NULL: Name of this declaration's definition in
#'   an emitted JSON Schema's `$defs`; see [class_to_JSONSchema()].
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
  max_items = NULL,
  unique_items = FALSE,
  def_name = NULL,
  description = ""
) {
  fields <- new_prop_spec(
    type = "number",
    default = default,
    minimum = min,
    maximum = max,
    exclusive_minimum = exclusive_min,
    exclusive_maximum = exclusive_max,
    nullable = nullable,
    container = if (vector) "array" else "none",
    min_items = min_items,
    max_items = max_items,
    unique_items = unique_items,
    def_name = def_name,
    description = description,
    default_env = parent.frame()
  )
  make_prop(fields, default = default)
} # /rtemis.core::prop_float


# %% prop_string ----
#' Character (string) S7 property
#'
#' Empty and whitespace-only strings are rejected by default: an unset value is
#' `NULL`, so `""` reaching a property is almost always a mistake rather than a
#' deliberate empty name. Pass `allow_empty = TRUE` where it is meaningful.
#'
#' @param default Character: Default value (NULL for none, or if `nullable`).
#'   Use a quoted call for a default that needs construction. The call is
#'   evaluated once in the caller's environment to validate
#'   the default and record it in [prop_spec()], then again for each object in
#'   the class's environment.
#' @param enum Character or NULL: Allowed values.
#' @param pattern Character or NULL: Regular expression every value must match,
#'   evaluated with `perl = TRUE` and emitted as JSON Schema `pattern`. Anchor
#'   it (`^...$`) to constrain the whole string. Write it in the syntax common
#'   to PCRE and ECMA-262, the dialect JSON Schema validators use.
#' @param min_length Integer or NULL: Fewest characters a value may have.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param vector Logical: If TRUE, the value is vector-valued (a JSON array).
#' @param map Logical: If TRUE, the value is a *named* vector (a JSON object
#'   with string values). Mutually exclusive with `vector`.
#' @param min_items Integer: Fewest elements a `vector` or `map` value may hold.
#'   `0L` admits an empty value.
#' @param max_items Integer or NULL: Most elements a `vector` or `map` value may
#'   hold.
#' @param unique_items Logical: If TRUE, a `vector` value's elements must be
#'   distinct.
#' @param allow_empty Logical: If TRUE, `""` is a valid value.
#' @param def_name Character or NULL: Name of this declaration's definition in
#'   an emitted JSON Schema's `$defs`; see [class_to_JSONSchema()].
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
#' # One-letter amino acid codes, at least two residues
#' sequence <- prop_string(pattern = "^[A-Za-z*]+$", min_length = 2L)
prop_string <- function(
  default = NULL,
  enum = NULL,
  pattern = NULL,
  min_length = NULL,
  nullable = FALSE,
  vector = FALSE,
  map = FALSE,
  min_items = 1L,
  max_items = NULL,
  unique_items = FALSE,
  allow_empty = FALSE,
  def_name = NULL,
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
  fields <- new_prop_spec(
    type = "string",
    default = default,
    enum = enum,
    pattern = pattern,
    min_length = min_length,
    nullable = nullable,
    container = if (vector) {
      "array"
    } else if (map) {
      "map"
    } else {
      "none"
    },
    min_items = min_items,
    max_items = max_items,
    unique_items = unique_items,
    allow_empty = allow_empty,
    def_name = def_name,
    description = description,
    default_env = parent.frame()
  )
  make_prop(fields, default = default)
} # /rtemis.core::prop_string


# %% prop_bag ----
#' Opaque named list S7 property
#'
#' A pass-through for values whose keys carry no declared contract -- extra
#' request headers, backend-specific options -- carried as one value however
#' many keys it holds. The keys are required: the value becomes a JSON object,
#' so every element must carry a name, and the names must be distinct. Use
#' NULL, not `list()`, for no value.
#'
#' @param default List: Default value (NULL for none, or if `nullable`).
#'   Use a quoted call for a default that needs construction. The call is
#'   evaluated once in the caller's environment to validate
#'   the default and record it in [prop_spec()], then again for each object in
#'   the class's environment.
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
  fields <- new_prop_spec(
    type = "object",
    default = default,
    nullable = nullable,
    description = description,
    default_env = parent.frame()
  )
  make_prop(fields, default = default)
} # /rtemis.core::prop_bag


# %% prop_const ----
#' Constant S7 property
#'
#' A property fixed to one value: it is the only value that validates, and it
#' is the default, so the declaration is the whole contract.
#'
#' @param value Logical, numeric, or character scalar: The value.
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


# %% prop_object ----
#' S7 object property
#'
#' A property whose value is an instance of an S7 class, or of a subclass. The
#' class carries its own validation; the emitted JSON Schema defines the class
#' once in `$defs`, under its class name, and refers to it.
#'
#' @param cls S7 class: Required class of the value.
#' @param default Default value, NULL for none, or a quoted constructor call
#'   such as `quote(Settings())`, evaluated once at declaration and again for
#'   each object.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @export
#'
#' @examples
#' Point <- S7::new_class("Point", properties = list(x = prop_float(default = 0)))
#' Shape <- S7::new_class(
#'   "Shape",
#'   properties = list(origin = prop_object(Point, default = quote(Point())))
#' )
#' Shape()@origin@x
prop_object <- function(
  cls,
  default = NULL,
  nullable = FALSE,
  description = ""
) {
  check_target_class(cls)
  fields <- new_prop_spec(
    type = "object",
    default = default,
    nullable = nullable,
    target = cls,
    description = description,
    default_env = parent.frame()
  )
  make_prop(fields, default = default)
} # /rtemis.core::prop_object


# %% prop_collection ----
#' Collection of S7 objects
#'
#' An array (unnamed list) or map (named list) whose every element is an
#' instance of one S7 class. Map keys must be non-empty and distinct.
#'
#' @param cls S7 class: Required class of every element.
#' @param container Character \{"array", "map"\}: An array is a JSON array; a
#'   map is a JSON object keyed by the list's names.
#' @param default Default value, NULL for none, or a quoted call. An empty
#'   `list()` is a valid default when `min_items` is `0L`.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param min_items Integer: Fewest elements. `0L` admits an empty collection.
#' @param max_items Integer or NULL: Most elements.
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @export
#'
#' @examples
#' Point <- S7::new_class("Point", properties = list(x = prop_float(default = 0)))
#' Path <- S7::new_class(
#'   "Path",
#'   properties = list(points = prop_collection(Point, default = list()))
#' )
#' length(Path(points = list(Point(), Point(x = 1)))@points)
prop_collection <- function(
  cls,
  container = c("array", "map"),
  default = NULL,
  nullable = FALSE,
  min_items = 0L,
  max_items = NULL,
  description = ""
) {
  check_target_class(cls)
  container <- match.arg(container)
  fields <- new_prop_spec(
    type = "object",
    default = default,
    nullable = nullable,
    container = container,
    min_items = min_items,
    max_items = max_items,
    target = cls,
    description = description,
    default_env = parent.frame()
  )
  make_prop(fields, default = default)
} # /rtemis.core::prop_collection


# %% prop_array ----
#' Array of a nested declaration
#'
#' A list whose every element satisfies `items`: a struct, union, matrix, map,
#' another array, or any scalar factory. For an array of scalars held as an R
#' vector, use the scalar factory's `vector = TRUE` instead.
#'
#' @param items S7 property from a `prop_*` factory: Declaration of each
#'   element.
#' @param default Default value, NULL for none, or a quoted call. An empty
#'   `list()` is a valid default when `min_items` is `0L`.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param min_items Integer: Fewest elements. `0L` admits an empty list.
#' @param max_items Integer or NULL: Most elements.
#' @param unique_items Logical: If TRUE, elements must be distinct.
#' @param def_name Character or NULL: Name of this declaration's definition in
#'   an emitted JSON Schema's `$defs`; see [class_to_JSONSchema()].
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @export
#'
#' @examples
#' record <- prop_struct(
#'   members = list(position = prop_integer(min = 1L)),
#'   required = "position",
#'   additional = TRUE
#' )
#' variants <- prop_array(record, default = list(), min_items = 0L)
prop_array <- function(
  items,
  default = NULL,
  nullable = FALSE,
  min_items = 1L,
  max_items = NULL,
  unique_items = FALSE,
  def_name = NULL,
  description = ""
) {
  fields <- new_prop_spec(
    type = "array",
    default = default,
    nullable = nullable,
    container = "array",
    min_items = min_items,
    max_items = max_items,
    unique_items = unique_items,
    items = nested_spec(items, "items"),
    def_name = def_name,
    description = description,
    default_env = parent.frame()
  )
  make_prop(fields, default = default)
} # /rtemis.core::prop_array


# %% prop_map ----
#' Map of a nested declaration
#'
#' A named list whose every value satisfies `values`. Keys must be non-empty
#' and distinct. For a map of strings held as a named character vector, use
#' `prop_string(map = TRUE)` instead.
#'
#' @param values S7 property from a `prop_*` factory: Declaration of each value.
#' @param default Default value, NULL for none, or a quoted call. An empty
#'   `list()` is a valid default when `min_items` is `0L`.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param min_items Integer: Fewest entries. `0L` admits an empty map.
#' @param max_items Integer or NULL: Most entries.
#' @param def_name Character or NULL: Name of this declaration's definition in
#'   an emitted JSON Schema's `$defs`; see [class_to_JSONSchema()].
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @export
#'
#' @examples
#' thresholds <- prop_map(prop_float(min = 0, max = 1), min_items = 0L)
prop_map <- function(
  values,
  default = NULL,
  nullable = FALSE,
  min_items = 1L,
  max_items = NULL,
  def_name = NULL,
  description = ""
) {
  fields <- new_prop_spec(
    type = "object",
    default = default,
    nullable = nullable,
    container = "map",
    min_items = min_items,
    max_items = max_items,
    values = nested_spec(values, "values"),
    def_name = def_name,
    description = description,
    default_env = parent.frame()
  )
  make_prop(fields, default = default)
} # /rtemis.core::prop_map


# %% prop_struct ----
#' Object with declared members
#'
#' A named list whose members are declared by name. Members listed in
#' `required` must be present; the others may be omitted. With
#' `additional = TRUE`, members beyond the declared ones are accepted and kept
#' as they are, which describes an open record such as a variant annotation
#' with source-specific fields.
#'
#' @param members Named list of S7 properties from `prop_*` factories: The
#'   declared members.
#' @param required Character or NULL: Names of members that must be present.
#' @param additional Logical: If TRUE, undeclared members are accepted.
#' @param default Default value, NULL for none, or a quoted call.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param def_name Character or NULL: Name of this declaration's definition in
#'   an emitted JSON Schema's `$defs`; see [class_to_JSONSchema()].
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @export
#'
#' @examples
#' variant <- prop_struct(
#'   members = list(position = prop_integer(min = 1L)),
#'   required = "position",
#'   additional = TRUE,
#'   description = "Sequence variant record."
#' )
prop_struct <- function(
  members,
  required = NULL,
  additional = FALSE,
  default = NULL,
  nullable = FALSE,
  def_name = NULL,
  description = ""
) {
  if (
    !is.list(members) ||
      length(members) == 0L ||
      !is.null(validate_keys(members))
  ) {
    abort(
      "`members` must be a non-empty list of properties with distinct names.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  specs <- lapply(names(members), function(nm) {
    nested_spec(members[[nm]], paste0("members[['", nm, "']]"))
  })
  names(specs) <- names(members)
  if (!is.null(required)) {
    check_character(required, allow_null = FALSE, arg_name = "required")
    unknown <- setdiff(required, names(members))
    if (length(unknown) > 0L) {
      abort(
        "`required` names undeclared members: ",
        paste0("'", unknown, "'", collapse = ", "),
        ".",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
  fields <- new_prop_spec(
    type = "object",
    default = default,
    nullable = nullable,
    container = "struct",
    members = specs,
    required = required,
    additional = additional,
    def_name = def_name,
    description = description,
    default_env = parent.frame()
  )
  make_prop(fields, default = default)
} # /rtemis.core::prop_struct


# %% prop_union ----
#' Union of declarations
#'
#' A value is valid when it satisfies at least one alternative. The emitted
#' JSON Schema is `anyOf` the alternatives.
#'
#' @param alternatives List of S7 properties from `prop_*` factories: The
#'   alternatives, at least two.
#' @param default Default value, NULL for none, or a quoted call.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param def_name Character or NULL: Name of this declaration's definition in
#'   an emitted JSON Schema's `$defs`; see [class_to_JSONSchema()].
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @export
#'
#' @examples
#' # 1-based positions, or [start, end] ranges
#' index <- prop_union(list(
#'   prop_integer(min = 1L, vector = TRUE, min_items = 0L),
#'   prop_matrix(prop_integer(min = 1L), ncol = 2L, min_items = 0L)
#' ))
prop_union <- function(
  alternatives,
  default = NULL,
  nullable = FALSE,
  def_name = NULL,
  description = ""
) {
  if (!is.list(alternatives) || length(alternatives) < 2L) {
    abort(
      "`alternatives` must be a list of at least two properties.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  specs <- lapply(seq_along(alternatives), function(i) {
    nested_spec(alternatives[[i]], paste0("alternatives[[", i, "]]"))
  })
  fields <- new_prop_spec(
    type = "union",
    default = default,
    nullable = nullable,
    alternatives = unname(specs),
    def_name = def_name,
    description = description,
    default_env = parent.frame()
  )
  make_prop(fields, default = default)
} # /rtemis.core::prop_union


# %% prop_matrix ----
#' Fixed-width integer or numeric matrix
#'
#' A matrix with exactly `ncol` columns, emitted as a JSON array of
#' `ncol`-element arrays, one per row. Every cell satisfies `items`. Use it for
#' a set of fixed-length tuples, such as `[start, end]` ranges.
#'
#' @param items S7 property from [prop_integer()] or [prop_float()], scalar:
#'   Declaration of each cell, including its bounds.
#' @param ncol Integer: Number of columns.
#' @param default Default value, NULL for none, or a quoted call.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param min_items Integer: Fewest rows. `0L` admits a matrix with no rows.
#' @param max_items Integer or NULL: Most rows.
#' @param def_name Character or NULL: Name of this declaration's definition in
#'   an emitted JSON Schema's `$defs`; see [class_to_JSONSchema()].
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @export
#'
#' @examples
#' ranges <- prop_matrix(prop_integer(min = 1L), ncol = 2L, min_items = 0L)
#' Region <- S7::new_class("Region", properties = list(index = ranges))
#' Region(index = rbind(c(3L, 10L), c(15L, 22L)))@index
prop_matrix <- function(
  items,
  ncol,
  default = NULL,
  nullable = FALSE,
  min_items = 1L,
  max_items = NULL,
  def_name = NULL,
  description = ""
) {
  cells <- nested_spec(items, "items")
  if (
    !cells[["type"]] %in% c("integer", "number") ||
      !identical(cells[["container"]], "none") ||
      isTRUE(cells[["nullable"]])
  ) {
    abort(
      "`items` must be a non-nullable scalar prop_integer() or prop_float().",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  fields <- new_prop_spec(
    type = cells[["type"]],
    default = default,
    nullable = nullable,
    container = "matrix",
    min_items = min_items,
    max_items = max_items,
    items = cells,
    ncol = ncol,
    def_name = def_name,
    description = description,
    default_env = parent.frame()
  )
  make_prop(fields, default = default)
} # /rtemis.core::prop_matrix


# %% prop_spec ----
#' A factory-built property's spec
#'
#' The machine-readable declaration behind a property: type, shape, default,
#' bounds, enum, and description. Read it to generate documentation, a JSON
#' Schema, or a defaults artifact from the class definition itself.
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
