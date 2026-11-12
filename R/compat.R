# compat.R
# ::rtemis.core::
# 2026- EDG rtemis.org

# TOC ----
#   rtemis.core-compat
#   prop_float / prop_bag / prop_const
#   optional / enum / optional_logical_scalar
#   check_optional_*
#
# Names kept for package versions on CRAN that call them (rule: "Names kept for
# released dependents"). Each builds the declaration of its canonical form, or
# behaves as it, except where its documentation states otherwise. New and
# edited code uses the canonical forms.

# %% rtemis.core-compat ----
#' Compatibility forms
#'
#' Names kept for package versions on CRAN that call them. New code uses the
#' canonical forms:
#'
#' - `prop_float()`: [prop_number()].
#' - `prop_bag()`: `prop_map(prop_any())`, with `nullable = TRUE` as its
#'   default.
#' - `prop_const(value)`: the scalar factory of `value`'s type with
#'   `const = value`, and `min_length = 0L` for a string.
#' - `optional()`: [nullable()].
#' - `enum(values)`: `prop_string(enum = values)`. `enum()` builds a property
#'   without a declaration, so [prop_spec()] returns NULL for it, and it admits
#'   `""` when `values` lists it.
#' - `optional_logical_scalar`: `prop_boolean(nullable = TRUE)`, without a
#'   declaration.
#' - `check_optional_<x>()`: `check_nullable_<x>()`.
#'
#' @param default Default value, a quoted call, or NULL.
#' @param min,max Numeric or NULL: Inclusive bounds.
#' @param exclusive_min,exclusive_max Numeric or NULL: Exclusive bounds.
#' @param nullable Logical: If TRUE, NULL is a valid value.
#' @param vector Logical: If TRUE, the value is an array of numbers.
#' @param min_items Integer: Fewest elements of an array.
#' @param unique_items Logical: If TRUE, array elements must be distinct.
#' @param description Character: Human-readable description.
#' @param value Scalar: The constant of `prop_const()`.
#' @param type S7 base class or S7 class.
#' @param values Character: Allowed values of `enum()`.
#' @param x Value to check.
#' @param lower,upper Numeric: Inclusive bounds.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return `prop_float()`, `prop_bag()`, `prop_const()`, and `enum()`: S7
#'   property. `optional()`: S7 union. `optional_logical_scalar`: S7 property.
#'   `check_optional_*()`: called for side effects; throws an error if checks
#'   fail.
#'
#' @name rtemis.core-compat
#' @keywords internal
#' @author EDG
NULL


# %% prop_float ----
#' @rdname rtemis.core-compat
#' @export
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
  dots <- if (isTRUE(vector)) {
    list(vector = TRUE, min_items = min_items, unique_items = unique_items)
  } else {
    list(vector = vector)
  }
  scalar_prop(
    type = "number",
    default = default,
    dots = dots,
    keywords = list(
      minimum = min,
      maximum = max,
      exclusive_minimum = exclusive_min,
      exclusive_maximum = exclusive_max
    ),
    nullable = nullable,
    def_name = NULL,
    description = description,
    env = parent.frame()
  )
} # /rtemis.core::prop_float


# %% prop_bag ----
#' @rdname rtemis.core-compat
#' @export
prop_bag <- function(default = NULL, nullable = TRUE, description = "") {
  container_prop(
    container = "map",
    item = prop_spec(prop_any()),
    default = default,
    min_items = 1L,
    max_items = NULL,
    unique_items = FALSE,
    nullable = nullable,
    def_name = NULL,
    description = description,
    env = parent.frame()
  )
} # /rtemis.core::prop_bag


# %% prop_const ----
#' @rdname rtemis.core-compat
#' @export
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
  keywords <- list(const = value)
  if (type == "string") {
    keywords[["min_length"]] <- 0L
  }
  scalar_prop(
    type = type,
    default = NULL,
    dots = list(),
    keywords = keywords,
    nullable = FALSE,
    def_name = NULL,
    description = description,
    env = parent.frame()
  )
} # /rtemis.core::prop_const


# %% optional ----
#' @rdname rtemis.core-compat
#' @export
optional <- function(type) {
  nullable(type)
} # /rtemis.core::optional


# %% enum ----
#' @rdname rtemis.core-compat
#' @export
enum <- function(values, default = NULL, nullable = FALSE) {
  check_character(values)
  check_logical_scalar(nullable)
  cls <- if (nullable) new_union(NULL, class_character) else class_character
  new_property(
    class = cls,
    validator = function(value) {
      if (nullable && is.null(value)) {
        return(NULL)
      }
      if (length(value) != 1L || is.na(value)) {
        return("must be a single non-NA character scalar")
      }
      if (!value %in% values) {
        return(paste0(
          "must be one of ",
          paste(paste0('"', values, '"'), collapse = ", ")
        ))
      }
      NULL
    },
    default = default
  )
} # /rtemis.core::enum


# %% optional_logical_scalar ----
#' @rdname rtemis.core-compat
#' @format NULL
#' @export
optional_logical_scalar <- new_property(
  class = new_union(NULL, class_logical),
  validator = function(value) {
    if (!is.null(value) && (length(value) != 1L || is.na(value))) {
      return("must be NULL or a logical scalar (TRUE or FALSE)")
    }
    NULL
  }
) # /rtemis.core::optional_logical_scalar


# %% check_optional_* ----
#' @rdname rtemis.core-compat
#' @export
check_optional_character_scalar <- check_nullable_character_scalar

#' @rdname rtemis.core-compat
#' @export
check_optional_double_scalar <- check_nullable_double_scalar

#' @rdname rtemis.core-compat
#' @export
check_optional_integer_scalar <- check_nullable_integer_scalar

#' @rdname rtemis.core-compat
#' @export
check_optional_pos_integer_scalar <- check_nullable_pos_integer_scalar

#' @rdname rtemis.core-compat
#' @export
check_optional_bounded_integer_scalar <- check_nullable_bounded_integer_scalar

#' @rdname rtemis.core-compat
#' @export
check_optional_logical_scalar <- check_nullable_logical_scalar

#' @rdname rtemis.core-compat
#' @export
check_optional_prob_scalar <- check_nullable_prob_scalar

#' @rdname rtemis.core-compat
#' @export
check_optional_pos_double_scalar <- check_nullable_pos_double_scalar

#' @rdname rtemis.core-compat
#' @export
check_optional_nonneg_double_scalar <- check_nullable_nonneg_double_scalar

#' @rdname rtemis.core-compat
#' @export
check_optional_bounded_double_scalar <- check_nullable_bounded_double_scalar

#' @rdname rtemis.core-compat
#' @export
check_optional_prob_vector <- check_nullable_prob_vector

#' @rdname rtemis.core-compat
#' @export
check_optional_unit_open_vector <- check_nullable_unit_open_vector

#' @rdname rtemis.core-compat
#' @export
check_optional_pos_double_vector <- check_nullable_pos_double_vector

#' @rdname rtemis.core-compat
#' @export
check_optional_nonneg_double_vector <- check_nullable_nonneg_double_vector
