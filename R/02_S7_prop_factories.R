# 02_S7_prop_factories.R
# ::rtemis.core::
# 2026- EDG rtemis.org

# TOC ----
# Spec ------------------------------------------------------------------------
#   PROP_TYPES / PROP_CONTAINERS / SCALAR_TYPES
#   new_prop_spec / check_value_set
#   check_nonneg_count / target_class_name / check_target_class
#   nested_spec / plain_item / take_dots
#   spec_shape
# Validation ------------------------------------------------------------------
#   validate_keys / validate_count / validate_attributes / validate_leaf_type
#   validate_with_spec / validate_value
#   validate_leaf / validate_bag / validate_union / validate_struct
#   validate_instance / validate_collection / validate_nested_array
#   validate_nested_map / validate_matrix
#   spec_validator / make_prop
# Builders --------------------------------------------------------------------
#   scalar_prop / container_prop
# Scalar factories ------------------------------------------------------------
#   prop_boolean / prop_integer / prop_number / prop_string
# Structural factories --------------------------------------------------------
#   prop_array / prop_map / prop_matrix / prop_struct / prop_union
#   prop_instance / prop_any
# Introspection ---------------------------------------------------------------
#   prop_spec
#
# The factories follow the grammar in rtemis-skills `R/prop-factories.md`:
# one factory per JSON Schema node, one argument per keyword, containers by
# composition. One declaration carries a property's type, shape, default,
# bounds, and description; the S7 validator is generated from it, and the JSON
# Schema emitter in `schema.R` reads the same declaration.

# %% PROP_TYPES ----
# JSON Schema type of a property's value.
# - "boolean", "integer", "number", "string": a scalar, or the scalar of an
#   array, map, or matrix stored in the scalar layout.
# - "object": an S7 instance or a collection of them (`prop_instance()`), a
#   map of any values (`prop_map(prop_any())`), a map of a nested declaration
#   (`prop_map()`), or a struct (`prop_struct()`).
# - "array": an array of a nested declaration (`prop_array()`).
# - "union": a value matching at least one alternative (`prop_union()`).
# - "any": any value (`prop_any()`).
PROP_TYPES <- c(
  "boolean",
  "integer",
  "number",
  "string",
  "object",
  "array",
  "union",
  "any"
)

# %% PROP_CONTAINERS ----
# How a declaration's values are stored.
# - "none"   a single value
# - "array"  a JSON array; in R an atomic vector of a non-nullable scalar, or a
#            list of instances (`target`) or of a nested declaration (`items`)
# - "map"    a JSON object keyed by names; in R a named atomic vector of a
#            non-nullable scalar, or a named list
# - "matrix" a JSON array of fixed-length arrays; in R a matrix with `ncol`
#            columns, one row per inner array
# - "struct" a JSON object with declared members; in R a named list
PROP_CONTAINERS <- c("none", "array", "map", "matrix", "struct")

# %% SCALAR_TYPES ----
SCALAR_TYPES <- c("boolean", "integer", "number", "string")

# %% RELEASED_DOTS ----
# Arguments that released versions passed to the scalar factories, accepted
# through `...` (rule: "Names kept for released dependents").
RELEASED_DOTS <- list(
  boolean = character(0L),
  integer = c("vector", "min_items", "unique_items"),
  number = c("vector", "min_items", "unique_items"),
  string = c("vector", "map", "allow_empty", "min_items", "unique_items")
)


# %% new_prop_spec ----
#' Build and validate a property spec
#'
#' The spec is a plain named list: a property is stored in a class definition
#' and written to the package's lazy-load database, and an S7 object would
#' carry a copy of its whole class definition along with it, once per
#' property.
#'
#' @param type Character: One of `PROP_TYPES`.
#' @param default Default value, a quoted call, or NULL.
#' @param minimum,maximum Numeric or NULL: Inclusive bounds.
#' @param exclusive_minimum,exclusive_maximum Numeric or NULL: Exclusive bounds.
#' @param enum Vector or NULL: Allowed values, of the declared type.
#' @param const Scalar or NULL: The single allowed value.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param container Character: One of `PROP_CONTAINERS`.
#' @param min_items Integer: Fewest elements, members, or rows.
#' @param max_items Integer or NULL: Most elements, members, or rows.
#' @param unique_items Logical: If TRUE, elements must be distinct.
#' @param pattern Character or NULL: Regular expression every string matches.
#' @param min_length Integer or NULL: Fewest characters of a string.
#' @param max_length Integer or NULL: Most characters of a string.
#' @param items Named list or NULL: Spec of each array element or matrix cell.
#' @param values Named list or NULL: Spec of each map value.
#' @param members Named list of specs or NULL: Members of a struct.
#' @param required Character or NULL: Members a struct value must contain.
#' @param additional_members Named list or NULL: Spec of each struct member
#'   beyond `members`; NULL for a closed struct.
#' @param alternatives List of specs or NULL: Alternatives of a union.
#' @param ncol Integer or NULL: Column count of a matrix.
#' @param target S7 class or NULL: Class of the instance, or of every element
#'   of a collection.
#' @param def_name Character or NULL: Name of the spec in an emitted `$defs`.
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
  pattern = NULL,
  min_length = NULL,
  max_length = NULL,
  items = NULL,
  values = NULL,
  members = NULL,
  required = NULL,
  additional_members = NULL,
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
  # The empty string is the "undocumented" description, which
  # `check_character_scalar()` rejects.
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
  scalar <- type %in% SCALAR_TYPES
  if (!is.null(enum) && !is.null(const)) {
    abort(
      "`enum` and `const` are mutually exclusive.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (!is.null(enum)) {
    if (!type %in% c("integer", "number", "string")) {
      abort(
        "`enum` applies to integer, number, and string declarations.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    if (
      length(enum) == 0L ||
        anyNA(enum) ||
        !is.null(attributes(enum)) ||
        !is.null(validate_leaf_type(enum, type))
    ) {
      abort(
        "`enum` must be a non-empty vector of ",
        type,
        " values without missing values.",
        class = c("rtemis_type_error", "rtemis_input_error")
      )
    }
  }
  if (!is.null(const)) {
    if (!scalar) {
      abort(
        "`const` applies to scalar declarations.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    if (
      length(const) != 1L ||
        is.na(const) ||
        !is.null(attributes(const)) ||
        !is.null(validate_leaf_type(const, type))
    ) {
      abort(
        "`const` must be a single ",
        type,
        " value.",
        class = c("rtemis_type_error", "rtemis_input_error")
      )
    }
  }
  string_keywords <- !is.null(pattern) ||
    !is.null(min_length) ||
    !is.null(max_length)
  if (string_keywords && type != "string") {
    abort(
      "`pattern`, `min_length`, and `max_length` apply to string ",
      "declarations only.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
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
        grepl(ucp_pattern(pattern), "", perl = TRUE)
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
  if (!is.null(max_length)) {
    check_nonneg_count(max_length, "max_length")
    if (max_length < (min_length %||% 0L)) {
      abort(
        "`max_length` must be at least `min_length`.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
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
  # Grammar 6: the default is the given `default`, then the `const`. A
  # container's default is its own; the scalar `const` is not one.
  if (is.null(default) && !is.null(const) && container == "none") {
    default <- const
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
    # Stored for the released readers of the spec list: TRUE when a string
    # declaration admits "".
    allow_empty = identical(type, "string") && identical(min_length, 0L),
    pattern = pattern,
    min_length = if (is.null(min_length)) NULL else as.integer(min_length),
    max_length = if (is.null(max_length)) NULL else as.integer(max_length),
    items = items,
    values = values,
    members = members,
    required = required,
    additional_members = additional_members,
    alternatives = alternatives,
    ncol = if (is.null(ncol)) NULL else as.integer(ncol),
    target = target,
    target_class = if (is.null(target)) NULL else target_class_name(target),
    def_name = def_name,
    description = description
  )
  check_value_set(fields)
  # A default that does not satisfy its own spec is a declaration error, so it
  # fails when the package loads rather than at first instantiation.
  # `default = NULL` on a non-nullable declaration means "no default", not the
  # value NULL, so it is not checked.
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


# %% check_value_set ----
#' Check `enum` members and `const` against the other keywords
#'
#' Grammar 7: each keyword constrains the value on its own, so every `enum`
#' member and the `const` must satisfy the node's other keywords.
#'
#' @param fields Named list of spec fields.
#'
#' @return NULL, invisibly. Aborts on the first member that fails.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_value_set <- function(fields) {
  set <- fields[["enum"]] %||% fields[["const"]]
  if (is.null(set)) {
    return(invisible(NULL))
  }
  what <- if (is.null(fields[["enum"]])) "`const`" else "`enum` member"
  scalar <- fields
  scalar[["enum"]] <- NULL
  scalar[["const"]] <- NULL
  scalar[["container"]] <- "none"
  scalar[["nullable"]] <- FALSE
  for (v in set) {
    msg <- validate_leaf(v, scalar)
    if (!is.null(msg)) {
      abort(
        what,
        " ",
        paste(deparse(v), collapse = ""),
        " ",
        msg,
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
  invisible(NULL)
} # /rtemis.core::check_value_set


# %% ucp_pattern ----
#' Prefix a pattern for Unicode-aware evaluation
#'
#' PCRE without `(*UCP)` classifies only ASCII characters for `\s`, `\w`,
#' and `\d`; ECMA-262, the dialect of JSON Schema `pattern`, classifies
#' Unicode spaces as `\s`.
#'
#' @param pattern Character: Regular expression.
#'
#' @return Character: `pattern` prefixed with `(*UCP)`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
ucp_pattern <- function(pattern) {
  paste0("(*UCP)", pattern)
} # /rtemis.core::ucp_pattern


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
#'   otherwise `"Class"`. The first element of the class attribute of every
#'   object of `cls`.
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
#' The spec of a declaration passed to a structural factory
#'
#' Grammar 6: only a property of a class takes a `default` argument, so a
#' nested declaration whose stored default is not its `const` aborts.
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
  default <- spec[["default"]]
  if (!is.null(default) && !identical(default, spec[["const"]])) {
    abort(
      "`",
      what,
      "` must not declare a `default`: only a property of a class takes one.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  spec
} # /rtemis.core::nested_spec


# %% plain_item ----
#' Whether an item declaration is stored in its container's layout
#'
#' An array or map item that is non-nullable and carries no description or
#' `def_name` is stored in the layout the released factories built: the
#' scalar's spec with `container = "array"` or `"map"`, the collection spec of
#' an instance, or the map-of-any spec.
#'
#' @param spec Named list: Item spec.
#'
#' @return Logical.
#'
#' @author EDG
#' @keywords internal
#' @noRd
plain_item <- function(spec) {
  identical(spec[["container"]], "none") &&
    !isTRUE(spec[["nullable"]]) &&
    is.null(spec[["def_name"]]) &&
    !nzchar(spec[["description"]] %||% "") &&
    (spec[["type"]] %in% SCALAR_TYPES || !is.null(spec[["target"]]))
} # /rtemis.core::plain_item


# %% take_dots ----
#' Check the `...` of a factory
#'
#' Grammar 8: `...` is empty, except for the released arguments a factory
#' accepts.
#'
#' @param dots List: The factory's `list(...)`.
#' @param allowed Character: Released argument names the factory accepts.
#' @param factory Character: Factory name, for the message.
#'
#' @return Named list: The accepted released arguments.
#'
#' @author EDG
#' @keywords internal
#' @noRd
take_dots <- function(dots, allowed, factory) {
  if (length(dots) == 0L) {
    return(list())
  }
  nms <- names(dots) %||% rep("", length(dots))
  if (!all(nzchar(nms))) {
    abort(
      "`",
      factory,
      "()` takes its arguments after `default` by name.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  unknown <- setdiff(nms, allowed)
  if (length(unknown) > 0L) {
    abort(
      "`",
      factory,
      "()` has no argument ",
      paste0("`", unknown, "`", collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (anyDuplicated(nms)) {
    abort(
      "`",
      factory,
      "()` received an argument twice.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  dots
} # /rtemis.core::take_dots


# %% spec_shape ----
#' The shape a spec declares
#'
#' One name per validation and emission branch, derived from `type`,
#' `container` and the nested fields.
#'
#' @param fields Named list of spec fields.
#'
#' @return Character, one of `"any"`, `"union"`, `"struct"`, `"object"`,
#'   `"collection"`, `"nested_array"`, `"nested_map"`, `"matrix"`, `"bag"`,
#'   `"leaf"`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
spec_shape <- function(fields) {
  container <- fields[["container"]]
  type <- fields[["type"]]
  if (identical(type, "any")) {
    return("any")
  }
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
#' Shared by every shape that becomes a JSON object. A key addresses one
#' member, so a key that is missing, empty, or repeated is rejected: R permits
#' repeated names, but `x[["a"]]` returns the first match, and
#' `{"a": 1, "a": 2}` is left undefined by RFC 8259. The rule matches the
#' emitted `propertyNames: {minLength: 1}`, so a key of spaces is valid.
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
  if (length(value) == 0L) {
    return(NULL)
  }
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
#' @param n Integer: Number of elements, members, or rows.
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
  container <- fields[["container"]]
  unit <- if (identical(container, "matrix")) {
    "row"
  } else if (identical(container, "map") || spec_shape(fields) == "bag") {
    "member"
  } else {
    "element"
  }
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


# %% validate_attributes ----
#' Validate a value's attributes against those its declaration allows
#'
#' Grammar 3: a value carries no attribute that changes what it means or how
#' it is written. A string may carry a `class`, such as the `json` class of
#' `jsonlite::toJSON()` output, because its characters are written either way.
#' An empty `names` attribute is accepted on any empty value.
#'
#' @param value Value being checked.
#' @param allowed Character: Attribute names the value may carry.
#'
#' @return NULL if valid, otherwise character error message.
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_attributes <- function(value, allowed) {
  attrs <- names(attributes(value))
  if (length(value) == 0L) {
    allowed <- c(allowed, "names")
  }
  extra <- setdiff(attrs, allowed)
  if (length(extra) == 0L) {
    return(NULL)
  }
  if ("class" %in% extra) {
    return(paste0(
      "must not be an object of class <",
      class(value)[[1L]],
      ">."
    ))
  }
  paste0(
    "must not carry the attribute",
    if (length(extra) > 1L) "s " else " ",
    paste0("`", extra, "`", collapse = ", "),
    "."
  )
} # /rtemis.core::validate_attributes


# %% validate_leaf_type ----
#' Validate the R type of a value against a scalar JSON type
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
    boolean = typeof(value) == "logical",
    integer = typeof(value) == "integer",
    number = typeof(value) %in% c("double", "integer"),
    string = typeof(value) == "character",
    TRUE
  )
  if (ok) {
    NULL
  } else {
    paste0("must be of type ", type, ".")
  }
} # /rtemis.core::validate_leaf_type


# %% validate_with_spec ----
#' Validate a property value against its spec
#'
#' Shared validator body for every factory-built property, top-level and
#' nested. Returns NULL if the value is valid, otherwise a character message,
#' per the S7 validator contract. Checks type, attributes, shape, arity,
#' missingness, bounds, and value sets, and descends into nested specs.
#'
#' Reads its fields with `[[`, so a spec carrying additional fields validates
#' against the generic ones and ignores the rest.
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
  shape <- spec_shape(fields)
  if (shape == "any") {
    return(NULL)
  }
  if (is.null(value)) {
    return(if (isTRUE(fields[["nullable"]])) NULL else "must not be NULL.")
  }
  switch(
    shape,
    union = validate_union(value, fields),
    struct = validate_struct(value, fields),
    object = validate_instance(value, fields),
    collection = validate_collection(value, fields),
    nested_array = validate_nested_array(value, fields),
    nested_map = validate_nested_map(value, fields),
    matrix = validate_matrix(value, fields),
    bag = validate_bag(value, fields),
    validate_leaf(value, fields)
  )
} # /rtemis.core::validate_with_spec


# %% validate_value ----
#' Validate a nested value against its spec
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
  validate_with_spec(value, fields)
} # /rtemis.core::validate_value


# %% validate_leaf ----
#' Validate a scalar, or an array or map of scalars, in the scalar layout
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
  container <- fields[["container"]]
  type <- fields[["type"]]
  if (is.list(value)) {
    return(paste0("must be of type ", type, ", not a list."))
  }
  msg <- validate_leaf_type(value, type)
  if (!is.null(msg)) {
    return(msg)
  }
  allowed <- c(
    if (type == "string") "class",
    if (identical(container, "map")) "names"
  )
  msg <- validate_attributes(value, allowed)
  if (!is.null(msg)) {
    return(msg)
  }
  if (length(value) == 0L) {
    if (container != "none" && identical(fields[["min_items"]], 0L)) {
      return(NULL)
    }
    # NULL is the only "unset" value; an empty vector is a value and is
    # rejected as one.
    return(
      if (isTRUE(fields[["nullable"]])) {
        "must not be empty (use NULL to leave it unset)."
      } else {
        "must not be empty."
      }
    )
  }
  if (identical(container, "map")) {
    msg <- validate_keys(value)
    if (!is.null(msg)) {
      return(msg)
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
    if (isTRUE(fields[["unique_items"]]) && anyDuplicated(unname(value))) {
      return("must have distinct elements.")
    }
  }
  if (type == "number" && !all(is.finite(value))) {
    return("must be finite.")
  }
  if (type == "string") {
    chars <- nchar(value, type = "chars")
    min_length <- fields[["min_length"]]
    if (!is.null(min_length) && any(chars < min_length)) {
      return(
        if (identical(min_length, 1L)) {
          "must not be an empty string."
        } else {
          paste0("must have at least ", min_length, " characters.")
        }
      )
    }
    max_length <- fields[["max_length"]]
    if (!is.null(max_length) && any(chars > max_length)) {
      return(paste0(
        "must have at most ",
        max_length,
        if (max_length == 1L) " character." else " characters."
      ))
    }
    pattern <- fields[["pattern"]]
    if (
      !is.null(pattern) &&
        !all(grepl(ucp_pattern(pattern), value, perl = TRUE))
    ) {
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
  enum <- fields[["enum"]]
  if (!is.null(enum) && !all(value %in% enum)) {
    return(paste0(
      "must be one of ",
      paste0("'", enum, "'", collapse = ", "),
      "."
    ))
  }
  NULL
} # /rtemis.core::validate_leaf


# %% validate_bag ----
#' Validate a map of any values
#'
#' The layout of `prop_map(prop_any())`: a named list whose values carry no
#' declared contract.
#'
#' @param value Property value being set.
#' @param fields Named list of spec fields.
#'
#' @return NULL if valid, otherwise character error message.
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_bag <- function(value, fields) {
  if (!is.list(value) || is.object(value)) {
    return("must be a list.")
  }
  msg <- validate_attributes(value, "names")
  if (!is.null(msg)) {
    return(msg)
  }
  msg <- validate_keys(value)
  if (!is.null(msg)) {
    return(msg)
  }
  msg <- validate_count(length(value), fields)
  if (!is.null(msg)) {
    return(
      if (length(value) == 0L && isTRUE(fields[["nullable"]])) {
        "must not be empty (use NULL to leave it unset)."
      } else {
        msg
      }
    )
  }
  NULL
} # /rtemis.core::validate_bag


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
    msg <- validate_with_spec(value, alt)
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
#' Validate a struct value
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
  msg <- validate_attributes(value, "names")
  if (!is.null(msg)) {
    return(msg)
  }
  msg <- validate_keys(value)
  if (!is.null(msg)) {
    return(msg)
  }
  members <- fields[["members"]]
  present <- names(value) %||% character(0L)
  missing_members <- setdiff(fields[["required"]], present)
  if (length(missing_members) > 0L) {
    return(paste0(
      "must have member",
      if (length(missing_members) > 1L) "s " else " ",
      paste0("'", missing_members, "'", collapse = ", "),
      "."
    ))
  }
  additional <- fields[["additional_members"]]
  unknown <- setdiff(present, names(members))
  if (length(unknown) > 0L && is.null(additional)) {
    return(paste0(
      "must not have member",
      if (length(unknown) > 1L) "s " else " ",
      paste0("'", unknown, "'", collapse = ", "),
      "; allowed members are ",
      paste0("'", names(members), "'", collapse = ", "),
      "."
    ))
  }
  for (i in seq_along(value)) {
    nm <- present[[i]]
    spec <- if (nm %in% names(members)) members[[nm]] else additional
    msg <- validate_with_spec(value[[i]], spec)
    if (!is.null(msg)) {
      return(paste0("member '", nm, "' ", msg))
    }
  }
  NULL
} # /rtemis.core::validate_struct


# %% validate_instance ----
#' Validate an S7 instance of exactly the declared class
#'
#' The emitted `$defs` entry of a class is closed, so an object of a subclass,
#' which may carry further properties, is rejected.
#'
#' @param value Property value being set.
#' @param fields Named list of spec fields.
#'
#' @return NULL if valid, otherwise character error message.
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_instance <- function(value, fields) {
  if (
    !S7_inherits(value) ||
      !identical(class(value)[[1L]], fields[["target_class"]])
  ) {
    return(paste0("must be a ", fields[["target"]]@name, " object."))
  }
  NULL
} # /rtemis.core::validate_instance


# %% validate_elements ----
#' Validate each element of a list against one spec
#'
#' @param value List.
#' @param fields Named list of spec fields of each element.
#' @param label Character: `"element"` or `"entry"`, for the message.
#'
#' @return NULL if valid, otherwise character error message.
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_elements <- function(value, fields, label) {
  keys <- names(value)
  for (i in seq_along(value)) {
    msg <- validate_with_spec(value[[i]], fields)
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


# %% validate_list_container ----
#' Validate the list, attributes, keys, count, and uniqueness of a container
#'
#' @param value Property value being set.
#' @param fields Named list of spec fields.
#' @param map Logical: If TRUE, the container is a map.
#'
#' @return NULL if valid, otherwise character error message.
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_list_container <- function(value, fields, map) {
  if (!is.list(value) || is.object(value)) {
    return(if (map) "must be a named list." else "must be a list.")
  }
  msg <- validate_attributes(value, if (map) "names" else character(0L))
  if (!is.null(msg)) {
    return(msg)
  }
  if (map) {
    msg <- validate_keys(value)
    if (!is.null(msg)) {
      return(msg)
    }
  }
  msg <- validate_count(length(value), fields)
  if (!is.null(msg)) {
    return(msg)
  }
  if (isTRUE(fields[["unique_items"]]) && anyDuplicated(unname(value))) {
    return("must have distinct elements.")
  }
  NULL
} # /rtemis.core::validate_list_container


# %% validate_collection ----
#' Validate an array or map of S7 instances
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
  map <- identical(fields[["container"]], "map")
  msg <- validate_list_container(value, fields, map)
  if (!is.null(msg)) {
    return(msg)
  }
  element <- fields
  element[["container"]] <- "none"
  element[["nullable"]] <- FALSE
  validate_elements(value, element, if (map) "entry" else "element")
} # /rtemis.core::validate_collection


# %% scalar_items ----
#' The scalar-layout spec of a container whose items are non-nullable scalars
#'
#' @param fields Named list of spec fields of the container.
#' @param item Named list of spec fields of the item.
#'
#' @return Named list of spec fields, or NULL when the item is not a
#'   non-nullable scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
scalar_items <- function(fields, item) {
  if (
    !item[["type"]] %in% SCALAR_TYPES ||
      !identical(item[["container"]], "none") ||
      isTRUE(item[["nullable"]])
  ) {
    return(NULL)
  }
  out <- item
  for (nm in c("container", "min_items", "max_items", "unique_items")) {
    out[nm] <- list(fields[[nm]])
  }
  out[["nullable"]] <- FALSE
  out
} # /rtemis.core::scalar_items


# %% validate_nested_array ----
#' Validate an array of a nested declaration
#'
#' Grammar 3: elements that are non-nullable scalars form an atomic vector;
#' other elements form a list.
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
  leaf <- scalar_items(fields, fields[["items"]])
  if (!is.null(leaf)) {
    return(validate_leaf(value, leaf))
  }
  msg <- validate_list_container(value, fields, map = FALSE)
  if (!is.null(msg)) {
    return(msg)
  }
  validate_elements(value, fields[["items"]], "element")
} # /rtemis.core::validate_nested_array


# %% validate_nested_map ----
#' Validate a map of a nested declaration
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
  leaf <- scalar_items(fields, fields[["values"]])
  if (!is.null(leaf)) {
    return(validate_leaf(value, leaf))
  }
  msg <- validate_list_container(value, fields, map = TRUE)
  if (!is.null(msg)) {
    return(msg)
  }
  validate_elements(value, fields[["values"]], "entry")
} # /rtemis.core::validate_nested_map


# %% validate_matrix ----
#' Validate a fixed-width matrix
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
  msg <- validate_leaf_type(value, fields[["type"]])
  if (!is.null(msg)) {
    return(msg)
  }
  msg <- validate_attributes(value, "dim")
  if (!is.null(msg)) {
    return(msg)
  }
  n_col <- fields[["ncol"]]
  if (ncol(value) != n_col) {
    return(paste0(
      "must have ",
      n_col,
      if (n_col == 1L) " column." else " columns."
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
  cells["max_items"] <- list(NULL)
  cells[["unique_items"]] <- FALSE
  msg <- validate_leaf(as.vector(value), cells)
  if (!is.null(msg)) {
    return(paste0("cells ", msg))
  }
  NULL
} # /rtemis.core::validate_matrix


# %% spec_validator ----
#' S7 validator for a spec
#'
#' @param fields Named list of spec fields.
#'
#' @return Function of one argument, per the S7 validator contract.
#'
#' @author EDG
#' @keywords internal
#' @noRd
spec_validator <- function(fields) {
  force(fields)
  function(value) validate_with_spec(value, fields)
} # /rtemis.core::spec_validator


# %% make_prop ----
#' Build the S7 property of a spec
#'
#' @param fields Named list of spec fields.
#' @param default Default value or quoted call, as given to the factory.
#'
#' @return S7 property carrying `fields` as its `spec`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
make_prop <- function(fields, default = fields[["default"]]) {
  # A default derived from `const` is in the spec, not in the factory call.
  if (is.null(default)) {
    default <- fields[["default"]]
  }
  shape <- spec_shape(fields)
  scalar_class <- function(type) {
    switch(
      type,
      boolean = class_logical,
      integer = class_integer,
      number = class_numeric,
      string = class_character,
      class_any
    )
  }
  base_class <- switch(
    shape,
    any = ,
    union = class_any,
    object = fields[["target"]],
    nested_array = ,
    nested_map = {
      item <- fields[["items"]] %||% fields[["values"]]
      if (is.null(scalar_items(fields, item))) {
        class_list
      } else {
        scalar_class(item[["type"]])
      }
    },
    struct = ,
    collection = ,
    bag = class_list,
    scalar_class(fields[["type"]])
  )
  if (is.call(default) && shape %in% c("leaf", "matrix")) {
    types <- switch(
      fields[["type"]],
      boolean = "logical",
      integer = "integer",
      number = c("integer", "double"),
      string = "character"
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
  cls <- if (isTRUE(fields[["nullable"]]) && !shape %in% c("union", "any")) {
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


# %% scalar_prop ----
#' Build a scalar declaration, or a released container form of one
#'
#' The body of the four scalar factories. The released arguments `vector`,
#' `map`, `allow_empty`, `min_items`, and `unique_items` build the declaration
#' of the canonical array or map form: `default`, `nullable`, `def_name`, and
#' `description` move to the container.
#'
#' @param type Character: Scalar type.
#' @param default Default value or quoted call.
#' @param dots List: The factory's `...`, already checked by `take_dots()`.
#' @param keywords Named list: The node's keyword arguments.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param def_name Character or NULL: Name in an emitted `$defs`.
#' @param description Character: Human-readable description.
#' @param env Environment: The factory caller's environment, where a quoted
#'   default is evaluated.
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
scalar_prop <- function(
  type,
  default,
  dots,
  keywords,
  nullable,
  def_name,
  description,
  env
) {
  vector <- dots[["vector"]] %||% FALSE
  map <- dots[["map"]] %||% FALSE
  check_logical_scalar(vector)
  check_logical_scalar(map)
  if (vector && map) {
    abort(
      "`vector` and `map` are mutually exclusive.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (!is.null(dots[["allow_empty"]])) {
    check_logical_scalar(dots[["allow_empty"]])
    if (isTRUE(dots[["allow_empty"]])) {
      keywords[["min_length"]] <- 0L
    }
  }
  container <- if (vector) {
    "array"
  } else if (map) {
    "map"
  } else {
    "none"
  }
  if (container == "none") {
    if (!is.null(dots[["min_items"]]) || !is.null(dots[["unique_items"]])) {
      abort(
        "`min_items` and `unique_items` apply with `vector` or `map`; use ",
        "prop_array() or prop_map().",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    fields <- do.call(
      new_prop_spec,
      quote = TRUE,
      c(
        list(
          type = type,
          default = default,
          nullable = nullable,
          def_name = def_name,
          description = description,
          default_env = env
        ),
        keywords
      )
    )
    return(make_prop(fields, default = default))
  }
  if (map && isTRUE(dots[["unique_items"]])) {
    abort(
      "`unique_items` applies to arrays, not maps.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  item <- do.call(new_prop_spec, c(list(type = type), keywords), quote = TRUE)
  container_prop(
    container = container,
    item = item,
    default = default,
    min_items = dots[["min_items"]] %||% 1L,
    max_items = NULL,
    unique_items = dots[["unique_items"]] %||% FALSE,
    nullable = nullable,
    def_name = def_name,
    description = description,
    env = env
  )
} # /rtemis.core::scalar_prop


# %% container_prop ----
#' Build an array or map declaration
#'
#' An item that is non-nullable and carries no `description` or `def_name` is
#' stored in the released layout: the scalar's spec with the container set, the
#' collection spec of an instance, or the bag spec of `prop_any()` values. Any
#' other item is stored as the nested `items` or `values` spec.
#'
#' @param container Character: `"array"` or `"map"`.
#' @param item Named list: Item spec.
#' @param default Default value or quoted call.
#' @param min_items,max_items Integer: Element or member count bounds.
#' @param unique_items Logical: If TRUE, array elements must be distinct.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param def_name Character or NULL: Name in an emitted `$defs`.
#' @param description Character: Human-readable description.
#' @param env Environment: Where a quoted default is evaluated.
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
container_prop <- function(
  container,
  item,
  default,
  min_items,
  max_items,
  unique_items,
  nullable,
  def_name,
  description,
  env
) {
  common <- list(
    default = default,
    nullable = nullable,
    min_items = min_items,
    max_items = max_items,
    unique_items = unique_items,
    def_name = def_name,
    description = description,
    default_env = env
  )
  any_values <- identical(item[["type"]], "any") &&
    is.null(item[["def_name"]]) &&
    !nzchar(item[["description"]])
  fields <- if (container == "map" && any_values) {
    do.call(new_prop_spec, c(list(type = "object"), common), quote = TRUE)
  } else if (plain_item(item) && item[["type"]] %in% SCALAR_TYPES) {
    keep <- c(
      "minimum",
      "maximum",
      "exclusive_minimum",
      "exclusive_maximum",
      "enum",
      "const",
      "pattern",
      "min_length",
      "max_length"
    )
    do.call(
      new_prop_spec,
      quote = TRUE,
      c(
        list(type = item[["type"]], container = container),
        item[keep],
        common
      )
    )
  } else if (plain_item(item)) {
    do.call(
      new_prop_spec,
      quote = TRUE,
      c(
        list(type = "object", container = container, target = item[["target"]]),
        common
      )
    )
  } else if (container == "array") {
    do.call(
      new_prop_spec,
      quote = TRUE,
      c(list(type = "array", container = "array", items = item), common)
    )
  } else {
    do.call(
      new_prop_spec,
      quote = TRUE,
      c(list(type = "object", container = "map", values = item), common)
    )
  }
  make_prop(fields, default = default)
} # /rtemis.core::container_prop


# %% prop_boolean ----
#' Boolean S7 property
#'
#' Declares a property whose value is one logical, `TRUE` or `FALSE`, emitted
#' as a JSON Schema `boolean`.
#'
#' @param default Logical or NULL: Default value. NULL gives no default unless
#'   `const` is given or `nullable` is TRUE, in which case the default is the
#'   constant or NULL. A quoted call is evaluated once in the caller's
#'   environment at declaration and again for each object.
#' @param ... Empty. Every argument after `default` is passed by name.
#' @param const Logical or NULL: The single allowed value.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param def_name Character or NULL: Name of the declaration in an emitted
#'   JSON Schema's `$defs`; see [class_to_JSONSchema()].
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @export
#'
#' @examples
#' verbose <- prop_boolean(TRUE, description = "Print progress.")
#' # Unset until the user chooses
#' shuffle <- prop_boolean(nullable = TRUE)
#' prop_spec(shuffle)[["default"]]
prop_boolean <- function(
  default = NULL,
  ...,
  const = NULL,
  nullable = FALSE,
  def_name = NULL,
  description = ""
) {
  dots <- take_dots(list(...), RELEASED_DOTS[["boolean"]], "prop_boolean")
  scalar_prop(
    type = "boolean",
    default = default,
    dots = dots,
    keywords = list(const = const),
    nullable = nullable,
    def_name = def_name,
    description = description,
    env = parent.frame()
  )
} # /rtemis.core::prop_boolean


# %% prop_integer ----
#' Integer S7 property
#'
#' Declares a property whose value is one R integer, emitted as a JSON Schema
#' `integer`. R integers range from -2147483647 to 2147483647; a declaration
#' that must admit larger whole numbers is a [prop_number()].
#'
#' @param default Integer or NULL: Default value. NULL gives no default unless
#'   `const` is given or `nullable` is TRUE. A quoted call is evaluated once in
#'   the caller's environment at declaration and again for each object.
#' @param ... Empty, except for the released arguments `vector`, `min_items`,
#'   and `unique_items`, which build the declaration of
#'   `prop_array(prop_integer(...))`.
#' @param enum Integer or NULL: Allowed values.
#' @param const Integer or NULL: The single allowed value.
#' @param min,max Numeric or NULL: Inclusive bounds.
#' @param exclusive_min,exclusive_max Numeric or NULL: Exclusive bounds.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param def_name Character or NULL: Name of the declaration in an emitted
#'   JSON Schema's `$defs`; see [class_to_JSONSchema()].
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @export
#'
#' @examples
#' n_iter <- prop_integer(100L, min = 1L, description = "Iterations.")
#' folds <- prop_integer(5L, enum = c(5L, 10L))
prop_integer <- function(
  default = NULL,
  ...,
  enum = NULL,
  const = NULL,
  min = NULL,
  max = NULL,
  exclusive_min = NULL,
  exclusive_max = NULL,
  nullable = FALSE,
  def_name = NULL,
  description = ""
) {
  dots <- take_dots(list(...), RELEASED_DOTS[["integer"]], "prop_integer")
  scalar_prop(
    type = "integer",
    default = default,
    dots = dots,
    keywords = list(
      enum = enum,
      const = const,
      minimum = min,
      maximum = max,
      exclusive_minimum = exclusive_min,
      exclusive_maximum = exclusive_max
    ),
    nullable = nullable,
    def_name = def_name,
    description = description,
    env = parent.frame()
  )
} # /rtemis.core::prop_integer


# %% prop_number ----
#' Number S7 property
#'
#' Declares a property whose value is one finite double or integer, emitted as
#' a JSON Schema `number`.
#'
#' @param default Numeric or NULL: Default value. NULL gives no default unless
#'   `const` is given or `nullable` is TRUE. A quoted call is evaluated once in
#'   the caller's environment at declaration and again for each object.
#' @param ... Empty, except for the released arguments `vector`, `min_items`,
#'   and `unique_items`, which build the declaration of
#'   `prop_array(prop_number(...))`.
#' @param enum Numeric or NULL: Allowed values.
#' @param const Numeric or NULL: The single allowed value.
#' @param min,max Numeric or NULL: Inclusive bounds.
#' @param exclusive_min,exclusive_max Numeric or NULL: Exclusive bounds.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param def_name Character or NULL: Name of the declaration in an emitted
#'   JSON Schema's `$defs`; see [class_to_JSONSchema()].
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @export
#'
#' @examples
#' temperature <- prop_number(0.7, min = 0, max = 2, description = "Sampling temperature.")
prop_number <- function(
  default = NULL,
  ...,
  enum = NULL,
  const = NULL,
  min = NULL,
  max = NULL,
  exclusive_min = NULL,
  exclusive_max = NULL,
  nullable = FALSE,
  def_name = NULL,
  description = ""
) {
  dots <- take_dots(list(...), RELEASED_DOTS[["number"]], "prop_number")
  scalar_prop(
    type = "number",
    default = default,
    dots = dots,
    keywords = list(
      enum = enum,
      const = const,
      minimum = min,
      maximum = max,
      exclusive_minimum = exclusive_min,
      exclusive_maximum = exclusive_max
    ),
    nullable = nullable,
    def_name = def_name,
    description = description,
    env = parent.frame()
  )
} # /rtemis.core::prop_number


# %% prop_string ----
#' String S7 property
#'
#' Declares a property whose value is one string, emitted as a JSON Schema
#' `string`. A string of spaces is valid unless `pattern` excludes it, for
#' example with `pattern = "\\S"`. A string may carry a `class`, such as the
#' `json` class of `jsonlite::toJSON()` output.
#'
#' @param default Character or NULL: Default value. NULL gives no default
#'   unless `const` is given or `nullable` is TRUE. A quoted call is evaluated
#'   once in the caller's environment at declaration and again for each
#'   object.
#' @param ... Empty, except for the released arguments `vector`, `map`,
#'   `allow_empty`, `min_items`, and `unique_items`, which build the
#'   declaration of `prop_array(prop_string(...))`,
#'   `prop_map(prop_string(...))`, or `prop_string(min_length = 0L)`.
#' @param enum Character or NULL: Allowed values.
#' @param const Character or NULL: The single allowed value.
#' @param pattern Character or NULL: Regular expression every value matches,
#'   written in the subset of ECMA-262 that PCRE shares. R evaluates it with
#'   `perl = TRUE` and the `(*UCP)` prefix.
#' @param min_length Integer \[0, Inf): Fewest characters. `0L` admits `""`.
#' @param max_length Integer or NULL: Most characters.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param def_name Character or NULL: Name of the declaration in an emitted
#'   JSON Schema's `$defs`; see [class_to_JSONSchema()].
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @export
#'
#' @examples
#' model_name <- prop_string(description = "Model name.")
#' backend <- prop_string("ollama", enum = c("ollama", "openai"))
#' api_key <- prop_string(nullable = TRUE, description = "API key.")
#' # One-letter amino acid codes, at least two residues
#' sequence <- prop_string(pattern = "^[A-Za-z*]+$", min_length = 2L)
prop_string <- function(
  default = NULL,
  ...,
  enum = NULL,
  const = NULL,
  pattern = NULL,
  min_length = 1L,
  max_length = NULL,
  nullable = FALSE,
  def_name = NULL,
  description = ""
) {
  dots <- take_dots(list(...), RELEASED_DOTS[["string"]], "prop_string")
  if (isTRUE(dots[["allow_empty"]]) && !missing(min_length)) {
    abort(
      "`allow_empty` and `min_length` are mutually exclusive; use ",
      "`min_length = 0L`.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  scalar_prop(
    type = "string",
    default = default,
    dots = dots,
    keywords = list(
      enum = enum,
      const = const,
      pattern = pattern,
      min_length = min_length,
      max_length = max_length
    ),
    nullable = nullable,
    def_name = def_name,
    description = description,
    env = parent.frame()
  )
} # /rtemis.core::prop_string


# %% prop_array ----
#' Array S7 property
#'
#' Declares a JSON Schema `array` whose every element satisfies `items`.
#' Elements that are non-nullable scalars form an atomic vector; other
#' elements form an unnamed list.
#'
#' @param items S7 property: Declaration of each element, from a `prop_*`
#'   factory, without a `default`.
#' @param ... Empty. Every argument after `items` is passed by name.
#' @param default Vector, list, or NULL: Default value, or a quoted call.
#' @param min_items Integer \[0, Inf): Fewest elements. `0L` admits an empty
#'   array.
#' @param max_items Integer or NULL: Most elements.
#' @param unique_items Logical: If TRUE, elements must be distinct.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param def_name Character or NULL: Name of the declaration in an emitted
#'   JSON Schema's `$defs`; see [class_to_JSONSchema()].
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @export
#'
#' @examples
#' tags <- prop_array(prop_string(), unique_items = TRUE, description = "Tags.")
#' # Distinct 1-based positions; an empty set is valid
#' positions <- prop_array(prop_integer(min = 1L), min_items = 0L, unique_items = TRUE)
prop_array <- function(
  items,
  ...,
  default = NULL,
  min_items = 1L,
  max_items = NULL,
  unique_items = FALSE,
  nullable = FALSE,
  def_name = NULL,
  description = ""
) {
  take_dots(list(...), character(0L), "prop_array")
  container_prop(
    container = "array",
    item = nested_spec(items, "items"),
    default = default,
    min_items = min_items,
    max_items = max_items,
    unique_items = unique_items,
    nullable = nullable,
    def_name = def_name,
    description = description,
    env = parent.frame()
  )
} # /rtemis.core::prop_array


# %% prop_map ----
#' Map S7 property
#'
#' Declares a JSON Schema `object` whose members, under any non-empty distinct
#' names, all satisfy `values`. Values that are non-nullable scalars form a
#' named atomic vector; other values form a named list.
#'
#' @param values S7 property: Declaration of each value, from a `prop_*`
#'   factory, without a `default`. [prop_any()] admits any value.
#' @param ... Empty. Every argument after `values` is passed by name.
#' @param default Named vector, named list, or NULL: Default value, or a
#'   quoted call.
#' @param min_members Integer \[0, Inf): Fewest members. `0L` admits an empty
#'   map.
#' @param max_members Integer or NULL: Most members.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param def_name Character or NULL: Name of the declaration in an emitted
#'   JSON Schema's `$defs`; see [class_to_JSONSchema()].
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @export
#'
#' @examples
#' weights <- prop_map(prop_number(min = 0, max = 1), min_members = 0L)
#' extra_headers <- prop_map(prop_any(), nullable = TRUE, description = "Extra headers.")
prop_map <- function(
  values,
  ...,
  default = NULL,
  min_members = 1L,
  max_members = NULL,
  nullable = FALSE,
  def_name = NULL,
  description = ""
) {
  take_dots(list(...), character(0L), "prop_map")
  container_prop(
    container = "map",
    item = nested_spec(values, "values"),
    default = default,
    min_items = min_members,
    max_items = max_members,
    unique_items = FALSE,
    nullable = nullable,
    def_name = def_name,
    description = description,
    env = parent.frame()
  )
} # /rtemis.core::prop_map


# %% prop_matrix ----
#' Fixed-width matrix S7 property
#'
#' Declares a JSON Schema `array` of `ncol`-element arrays, one per row, whose
#' every cell satisfies `items`. The R value is an integer or double matrix
#' with `ncol` columns and no attribute other than `dim`. Use it for a set of
#' fixed-length tuples, such as `[start, end]` ranges.
#'
#' @param items S7 property: Declaration of each cell, a non-nullable
#'   [prop_integer()] or [prop_number()] without a `default`.
#' @param ncol Integer \[1, Inf): Column count.
#' @param ... Empty. Every argument after `ncol` is passed by name.
#' @param default Matrix or NULL: Default value, or a quoted call.
#' @param min_items Integer \[0, Inf): Fewest rows. `0L` admits a matrix of
#'   zero rows.
#' @param max_items Integer or NULL: Most rows.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param def_name Character or NULL: Name of the declaration in an emitted
#'   JSON Schema's `$defs`; see [class_to_JSONSchema()].
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
  ...,
  default = NULL,
  min_items = 1L,
  max_items = NULL,
  nullable = FALSE,
  def_name = NULL,
  description = ""
) {
  take_dots(list(...), character(0L), "prop_matrix")
  cells <- nested_spec(items, "items")
  if (
    !cells[["type"]] %in% c("integer", "number") ||
      !identical(cells[["container"]], "none") ||
      isTRUE(cells[["nullable"]])
  ) {
    abort(
      "`items` must be a non-nullable prop_integer() or prop_number().",
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


# %% prop_struct ----
#' Struct S7 property
#'
#' Declares a JSON Schema `object` with declared members. The R value is a
#' named list. A struct supplies no defaults, so a member is required unless
#' it is nullable or `prop_any()`; `required` names further members that a
#' value must carry, such as a nullable member a document must state. A
#' member declares no `default`.
#'
#' @param members Named list of S7 properties: The members, each from a
#'   `prop_*` factory, without a `default`.
#' @param ... Empty. Every argument after `members` is passed by name.
#' @param default Named list or NULL: Default value, or a quoted call.
#' @param required Character or NULL: Further members a value must contain.
#' @param additional_members FALSE or S7 property: `FALSE` closes the struct;
#'   a declaration admits members beyond `members` that satisfy it, and
#'   [prop_any()] admits any.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param def_name Character or NULL: Name of the declaration in an emitted
#'   JSON Schema's `$defs`; see [class_to_JSONSchema()].
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @export
#'
#' @examples
#' variant <- prop_struct(
#'   members = list(
#'     position = prop_integer(min = 1L),
#'     note = prop_string(nullable = TRUE)
#'   ),
#'   additional_members = prop_any(),
#'   description = "Sequence variant record."
#' )
prop_struct <- function(
  members,
  ...,
  default = NULL,
  required = NULL,
  additional_members = FALSE,
  nullable = FALSE,
  def_name = NULL,
  description = ""
) {
  take_dots(list(...), character(0L), "prop_struct")
  if (
    !is.list(members) ||
      length(members) == 0L ||
      !is.null(validate_keys(members)) ||
      is.null(names(members))
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
  additional <- if (isFALSE(additional_members)) {
    NULL
  } else {
    nested_spec(additional_members, "additional_members")
  }
  # Grammar 5: a struct supplies no default, so every member that is not
  # nullable is required.
  implied <- names(specs)[
    !vapply(
      specs,
      function(s) isTRUE(s[["nullable"]]) || identical(s[["type"]], "any"),
      logical(1L)
    )
  ]
  fields <- new_prop_spec(
    type = "object",
    default = default,
    nullable = nullable,
    container = "struct",
    members = specs,
    required = union(implied, required),
    additional_members = additional,
    def_name = def_name,
    description = description,
    default_env = parent.frame()
  )
  make_prop(fields, default = default)
} # /rtemis.core::prop_struct


# %% prop_union ----
#' Union S7 property
#'
#' Declares a value that satisfies at least one of `alternatives`, emitted as
#' JSON Schema `anyOf`. The alternatives of a union of [prop_instance()]
#' declarations share a `const` property whose value differs between their
#' classes, so that a document names its class.
#'
#' @param alternatives List of S7 properties: At least two declarations, each
#'   from a `prop_*` factory, without a `default`.
#' @param ... Empty. Every argument after `alternatives` is passed by name.
#' @param default Default value, or a quoted call, or NULL.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param def_name Character or NULL: Name of the declaration in an emitted
#'   JSON Schema's `$defs`; see [class_to_JSONSchema()].
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
#'   prop_array(prop_integer(min = 1L), min_items = 0L),
#'   prop_matrix(prop_integer(min = 1L), ncol = 2L, min_items = 0L)
#' ))
prop_union <- function(
  alternatives,
  ...,
  default = NULL,
  nullable = FALSE,
  def_name = NULL,
  description = ""
) {
  take_dots(list(...), character(0L), "prop_union")
  if (!is.list(alternatives) || length(alternatives) < 2L) {
    abort(
      "`alternatives` must be a list of at least two properties.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  specs <- lapply(seq_along(alternatives), function(i) {
    nested_spec(alternatives[[i]], paste0("alternatives[[", i, "]]"))
  })
  check_instance_discriminator(specs)
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


# %% check_instance_discriminator ----
#' Check that a union of instances names its class through a constant
#'
#' @param specs List of alternative specs.
#'
#' @return NULL, invisibly. Aborts when every alternative is an instance and
#'   no property is a `const` in every class with a distinct value per class.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_instance_discriminator <- function(specs) {
  shapes <- vapply(specs, spec_shape, character(1L))
  if (!all(shapes == "object")) {
    return(invisible(NULL))
  }
  consts <- lapply(specs, function(s) {
    props <- s[["target"]]@properties
    vals <- lapply(props, function(p) {
      if (inherits(p, "S7_property")) p[["spec"]][["const"]] else NULL
    })
    vals[!vapply(vals, is.null, logical(1L))]
  })
  shared <- Reduce(intersect, lapply(consts, names))
  for (nm in shared) {
    vals <- lapply(consts, `[[`, nm)
    if (!anyDuplicated(vals)) {
      return(invisible(NULL))
    }
  }
  abort(
    "The classes of a union of prop_instance() declarations must share a ",
    "`const` property with a distinct value in each class.",
    class = c("rtemis_value_error", "rtemis_input_error")
  )
} # /rtemis.core::check_instance_discriminator


# %% prop_instance ----
#' S7 instance property
#'
#' Declares a property whose value is an object of exactly `cls`. The class
#' validates its own properties; an emitted JSON Schema defines the class once
#' in `$defs`, under its class name, and refers to it. A value that may be an
#' object of one of several classes is a [prop_union()] of their
#' `prop_instance()` declarations.
#'
#' @param cls S7 class: The class of the value.
#' @param ... Empty. Every argument after `cls` is passed by name.
#' @param default S7 object or NULL: Default value. Use a quoted call, such as
#'   `quote(Settings())`, evaluated once at declaration and again for each
#'   object.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @export
#'
#' @examples
#' Point <- S7::new_class("Point", properties = list(x = prop_number(0)))
#' Shape <- S7::new_class(
#'   "Shape",
#'   properties = list(origin = prop_instance(Point, default = quote(Point())))
#' )
#' Shape()@origin@x
prop_instance <- function(
  cls,
  ...,
  default = NULL,
  nullable = FALSE,
  description = ""
) {
  take_dots(list(...), character(0L), "prop_instance")
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
} # /rtemis.core::prop_instance


# %% prop_any ----
#' Any-value S7 property
#'
#' Declares a value with no contract, emitted as the JSON Schema `{}`. It
#' admits every value, `NULL` included, so a `prop_any()` member of a class or
#' struct is required only when named in `required`. `prop_map(prop_any())`
#' declares a named list whose keys carry no declared contract.
#'
#' @param ... Empty. Every argument is passed by name.
#' @param default Default value, a quoted call, or NULL.
#' @param description Character: Human-readable description.
#'
#' @return S7 property.
#'
#' @author EDG
#' @export
#'
#' @examples
#' Request <- S7::new_class("Request", properties = list(payload = prop_any()))
#' Request(payload = list(a = 1, b = "two"))@payload
prop_any <- function(..., default = NULL, description = "") {
  take_dots(list(...), character(0L), "prop_any")
  fields <- new_prop_spec(
    type = "any",
    default = default,
    nullable = TRUE,
    description = description,
    default_env = parent.frame()
  )
  make_prop(fields, default = default)
} # /rtemis.core::prop_any


# %% prop_spec ----
#' Get the declaration of a factory-built property
#'
#' @param property S7 property.
#'
#' @return Named list: The declaration a `prop_*` factory stored, or NULL for a
#'   property built otherwise.
#'
#' @author EDG
#' @export
#'
#' @examples
#' prop_spec(prop_number(0.5, min = 0, max = 1))[["maximum"]]
prop_spec <- function(property) {
  if (!inherits(property, "S7_property")) {
    abort(
      "`property` must be an S7 property.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  property[["spec"]]
} # /rtemis.core::prop_spec
