# 2026- EDG rtemis.org

# TOC ----
# Character -------------------------------------------------------------------
#   character_scalar / optional_character_scalar
# Double ----------------------------------------------------------------------
#   double_scalar / optional_double_scalar
# Integer ---------------------------------------------------------------------
#   integer_scalar / optional_integer_scalar
#   nonneg_integer_scalar / optional_nonneg_integer_scalar [0, Inf)
#   pos_integer_scalar / optional_pos_integer_scalar       (0, Inf)
# Logical ---------------------------------------------------------------------
#   logical_scalar / optional_logical_scalar
# Bounded double scalars ------------------------------------------------------
#   prob_scalar / optional_prob_scalar                   [0, 1]
#   unit_open_scalar / optional_unit_open_scalar         (0, 1)
#   pos_double_scalar / optional_pos_double_scalar       (0, Inf)
#   nonneg_double_scalar / optional_nonneg_double_scalar [0, Inf)
# Bounded double vectors ------------------------------------------------------
#   prob_vector / optional_prob_vector                   [0, 1]
#   unit_open_vector / optional_unit_open_vector         (0, 1)
#   pos_double_vector / optional_pos_double_vector       (0, Inf)
#   nonneg_double_vector / optional_nonneg_double_vector [0, Inf)
# Factory ---------------------------------------------------------------------
#   bounded_double_property

# %% Character ----
#' Non-empty character scalar S7 property
#'
#' S7 property accepting a single non-NA, non-empty (after trimming whitespace) string.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
character_scalar <- new_property(
  class_character,
  validator = function(value) {
    if (length(value) != 1L || is.na(value) || !nzchar(trimws(value))) {
      return("must be a non-empty character scalar")
    }
    NULL
  }
)


#' Optional non-empty character scalar S7 property
#'
#' S7 property accepting `NULL` or a single non-NA, non-empty (after trimming whitespace) string.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
optional_character_scalar <- new_property(
  class = new_union(NULL, class_character),
  validator = function(value) {
    if (
      !is.null(value) &&
        (length(value) != 1L || is.na(value) || !nzchar(trimws(value)))
    ) {
      return("must be NULL or a non-empty character scalar")
    }
    NULL
  }
)


# %% Double ----
#' Double scalar S7 property
#'
#' S7 property accepting a single non-NA double value.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
double_scalar <- new_property(
  class_double,
  validator = function(value) {
    if (length(value) != 1L || is.na(value)) {
      return("must be a double scalar")
    }
    NULL
  }
)


#' Optional double scalar S7 property
#'
#' S7 property accepting `NULL` or a single non-NA double value.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
optional_double_scalar <- new_property(
  class = new_union(NULL, class_double),
  validator = function(value) {
    if (!is.null(value) && (length(value) != 1L || is.na(value))) {
      return("must be NULL or a double scalar")
    }
    NULL
  }
)


# %% Integer ----
#' Integer scalar S7 property
#'
#' S7 property accepting a single non-NA integer value (must be `integer` type, e.g. `1L`).
#'
#' @return An S7 property object.
#' @author EDG
#' @export
integer_scalar <- new_property(
  class_integer,
  validator = function(value) {
    if (length(value) != 1L || is.na(value)) {
      return("must be an integer scalar (e.g. 1L)")
    }
    NULL
  }
)


#' Optional integer scalar S7 property
#'
#' S7 property accepting `NULL` or a single non-NA integer value (must be `integer` type, e.g. `1L`).
#'
#' @return An S7 property object.
#' @author EDG
#' @export
optional_integer_scalar <- new_property(
  class = new_union(NULL, class_integer),
  validator = function(value) {
    if (!is.null(value) && (length(value) != 1L || is.na(value))) {
      return("must be NULL or an integer scalar (e.g. 1L)")
    }
    NULL
  }
)


#' Non-negative integer scalar S7 property
#'
#' S7 property accepting a single non-NA integer value greater than or equal to zero,
#' i.e. in \eqn{[0, \infty)} (e.g. `0L`, `1L`).
#'
#' @return An S7 property object.
#' @author EDG
#' @export
nonneg_integer_scalar <- new_property(
  class_integer,
  validator = function(value) {
    if (length(value) != 1L || is.na(value) || value < 0L) {
      return("must be a non-negative integer scalar (>= 0, e.g. 0L)")
    }
    NULL
  }
)


#' Optional non-negative integer scalar S7 property
#'
#' S7 property accepting `NULL` or a single non-NA integer value greater than or equal to zero.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
optional_nonneg_integer_scalar <- new_property(
  class = new_union(NULL, class_integer),
  validator = function(value) {
    if (
      !is.null(value) && (length(value) != 1L || is.na(value) || value < 0L)
    ) {
      return("must be NULL or a non-negative integer scalar (>= 0, e.g. 0L)")
    }
    NULL
  }
)


#' Positive integer scalar S7 property
#'
#' S7 property accepting a single non-NA integer value strictly greater than zero (e.g. `1L`).
#'
#' @return An S7 property object.
#' @author EDG
#' @export
pos_integer_scalar <- new_property(
  class_integer,
  validator = function(value) {
    if (length(value) != 1L || is.na(value) || value <= 0L) {
      return("must be a positive integer scalar (> 0, e.g. 1L)")
    }
    NULL
  }
)


#' Optional positive integer scalar S7 property
#'
#' S7 property accepting `NULL` or a single non-NA integer value strictly greater than zero.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
optional_pos_integer_scalar <- new_property(
  class = new_union(NULL, class_integer),
  validator = function(value) {
    if (
      !is.null(value) && (length(value) != 1L || is.na(value) || value <= 0L)
    ) {
      return("must be NULL or a positive integer scalar (> 0, e.g. 1L)")
    }
    NULL
  }
)


# %% Logical ----
#' Logical scalar S7 property
#'
#' S7 property accepting a single non-NA logical value.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
logical_scalar <- new_property(
  class_logical,
  validator = function(value) {
    if (length(value) != 1L || is.na(value)) {
      return("must be a logical scalar (TRUE or FALSE)")
    }
    NULL
  }
)


#' Optional logical scalar S7 property
#'
#' S7 property accepting `NULL` or a single non-NA logical value.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
optional_logical_scalar <- new_property(
  class = new_union(NULL, class_logical),
  validator = function(value) {
    if (!is.null(value) && (length(value) != 1L || is.na(value))) {
      return("must be NULL or a logical scalar (TRUE or FALSE)")
    }
    NULL
  }
)


# %% Bounded double scalars ----
#' Probability scalar S7 property
#'
#' S7 property accepting a single finite double in \eqn{[0, 1]}.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
prob_scalar <- new_property(
  class_double,
  validator = function(value) {
    if (length(value) != 1L || is.na(value) || value < 0 || value > 1) {
      return("must be a finite double in [0, 1]")
    }
    NULL
  }
)


#' Optional probability scalar S7 property
#'
#' S7 property accepting `NULL` or a single finite double in \eqn{[0, 1]}.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
optional_prob_scalar <- new_property(
  class = new_union(NULL, class_double),
  validator = function(value) {
    if (
      !is.null(value) &&
        (length(value) != 1L || is.na(value) || value < 0 || value > 1)
    ) {
      return("must be NULL or a finite double in [0, 1]")
    }
    NULL
  }
)


#' Open-unit-interval scalar S7 property
#'
#' S7 property accepting a single finite double strictly in \eqn{(0, 1)}.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
unit_open_scalar <- new_property(
  class_double,
  validator = function(value) {
    if (length(value) != 1L || is.na(value) || value <= 0 || value >= 1) {
      return("must be a finite double in (0, 1)")
    }
    NULL
  }
)


#' Optional open-unit-interval scalar S7 property
#'
#' S7 property accepting `NULL` or a single finite double strictly in \eqn{(0, 1)}.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
optional_unit_open_scalar <- new_property(
  class = new_union(NULL, class_double),
  validator = function(value) {
    if (
      !is.null(value) &&
        (length(value) != 1L || is.na(value) || value <= 0 || value >= 1)
    ) {
      return("must be NULL or a finite double in (0, 1)")
    }
    NULL
  }
)


#' Positive double scalar S7 property
#'
#' S7 property accepting a single finite double strictly greater than zero, i.e. in \eqn{(0, \infty)}.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
pos_double_scalar <- new_property(
  class_double,
  validator = function(value) {
    if (
      length(value) != 1L || is.na(value) || !is.finite(value) || value <= 0
    ) {
      return("must be a finite positive double (> 0)")
    }
    NULL
  }
)


#' Optional positive double scalar S7 property
#'
#' S7 property accepting `NULL` or a single finite double strictly greater than zero.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
optional_pos_double_scalar <- new_property(
  class = new_union(NULL, class_double),
  validator = function(value) {
    if (
      !is.null(value) &&
        (length(value) != 1L || is.na(value) || !is.finite(value) || value <= 0)
    ) {
      return("must be NULL or a finite positive double (> 0)")
    }
    NULL
  }
)


#' Non-negative double scalar S7 property
#'
#' S7 property accepting a single finite double greater than or equal to zero, i.e. in \eqn{[0, \infty)}.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
nonneg_double_scalar <- new_property(
  class_double,
  validator = function(value) {
    if (length(value) != 1L || is.na(value) || !is.finite(value) || value < 0) {
      return("must be a finite non-negative double (>= 0)")
    }
    NULL
  }
)


#' Optional non-negative double scalar S7 property
#'
#' S7 property accepting `NULL` or a single finite double greater than or equal to zero.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
optional_nonneg_double_scalar <- new_property(
  class = new_union(NULL, class_double),
  validator = function(value) {
    if (
      !is.null(value) &&
        (length(value) != 1L || is.na(value) || !is.finite(value) || value < 0)
    ) {
      return("must be NULL or a finite non-negative double (>= 0)")
    }
    NULL
  }
)


# %% Bounded double vectors ----
#' Probability vector S7 property
#'
#' S7 property accepting a non-empty double vector with all elements in \eqn{[0, 1]} and no NAs.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
prob_vector <- new_property(
  class_double,
  validator = function(value) {
    if (length(value) == 0L) {
      return("must be a non-empty vector")
    }
    if (anyNA(value)) {
      return("must not contain NAs")
    }
    if (any(value < 0) || any(value > 1)) {
      return("all elements must be in [0, 1]")
    }
    NULL
  }
)


#' Optional probability vector S7 property
#'
#' S7 property accepting `NULL` or a non-empty double vector with all elements in \eqn{[0, 1]}
#' and no NAs.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
optional_prob_vector <- new_property(
  class = new_union(NULL, class_double),
  validator = function(value) {
    if (is.null(value)) {
      return(NULL)
    }
    if (length(value) == 0L) {
      return("must be a non-empty vector")
    }
    if (anyNA(value)) {
      return("must not contain NAs")
    }
    if (any(value < 0) || any(value > 1)) {
      return("all elements must be in [0, 1]")
    }
    NULL
  }
)


#' Open-unit-interval vector S7 property
#'
#' S7 property accepting a non-empty double vector with all elements strictly in \eqn{(0, 1)}
#' and no NAs.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
unit_open_vector <- new_property(
  class_double,
  validator = function(value) {
    if (length(value) == 0L) {
      return("must be a non-empty vector")
    }
    if (anyNA(value)) {
      return("must not contain NAs")
    }
    if (any(value <= 0) || any(value >= 1)) {
      return("all elements must be in (0, 1)")
    }
    NULL
  }
)


#' Optional open-unit-interval vector S7 property
#'
#' S7 property accepting `NULL` or a non-empty double vector with all elements strictly in
#' \eqn{(0, 1)} and no NAs.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
optional_unit_open_vector <- new_property(
  class = new_union(NULL, class_double),
  validator = function(value) {
    if (is.null(value)) {
      return(NULL)
    }
    if (length(value) == 0L) {
      return("must be a non-empty vector")
    }
    if (anyNA(value)) {
      return("must not contain NAs")
    }
    if (any(value <= 0) || any(value >= 1)) {
      return("all elements must be in (0, 1)")
    }
    NULL
  }
)


#' Positive double vector S7 property
#'
#' S7 property accepting a non-empty double vector with all elements finite, strictly greater
#' than zero, and no NAs.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
pos_double_vector <- new_property(
  class_double,
  validator = function(value) {
    if (length(value) == 0L) {
      return("must be a non-empty vector")
    }
    if (anyNA(value)) {
      return("must not contain NAs")
    }
    if (!all(is.finite(value)) || any(value <= 0)) {
      return("all elements must be finite and > 0")
    }
    NULL
  }
)


#' Optional positive double vector S7 property
#'
#' S7 property accepting `NULL` or a non-empty double vector with all elements finite,
#' strictly greater than zero, and no NAs.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
optional_pos_double_vector <- new_property(
  class = new_union(NULL, class_double),
  validator = function(value) {
    if (is.null(value)) {
      return(NULL)
    }
    if (length(value) == 0L) {
      return("must be a non-empty vector")
    }
    if (anyNA(value)) {
      return("must not contain NAs")
    }
    if (!all(is.finite(value)) || any(value <= 0)) {
      return("all elements must be finite and > 0")
    }
    NULL
  }
)


#' Non-negative double vector S7 property
#'
#' S7 property accepting a non-empty double vector with all elements finite, greater than or
#' equal to zero, and no NAs.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
nonneg_double_vector <- new_property(
  class_double,
  validator = function(value) {
    if (length(value) == 0L) {
      return("must be a non-empty vector")
    }
    if (anyNA(value)) {
      return("must not contain NAs")
    }
    if (!all(is.finite(value)) || any(value < 0)) {
      return("all elements must be finite and >= 0")
    }
    NULL
  }
)


#' Optional non-negative double vector S7 property
#'
#' S7 property accepting `NULL` or a non-empty double vector with all elements finite,
#' greater than or equal to zero, and no NAs.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
optional_nonneg_double_vector <- new_property(
  class = new_union(NULL, class_double),
  validator = function(value) {
    if (is.null(value)) {
      return(NULL)
    }
    if (length(value) == 0L) {
      return("must be a non-empty vector")
    }
    if (anyNA(value)) {
      return("must not contain NAs")
    }
    if (!all(is.finite(value)) || any(value < 0)) {
      return("all elements must be finite and >= 0")
    }
    NULL
  }
)


# %% Factory ----
#' Create a bounded double S7 property
#'
#' Returns a `new_property()` for a double scalar constrained to a given interval.
#' Useful for bounds not covered by the pre-built properties.
#'
#' @param lower Numeric scalar. Lower bound. Default `-Inf`.
#' @param upper Numeric scalar. Upper bound. Default `Inf`.
#' @param lower_open Logical scalar. If `TRUE`, lower bound is exclusive `(lower, ...]`.
#'   Default `FALSE`.
#' @param upper_open Logical scalar. If `TRUE`, upper bound is exclusive `[..., upper)`.
#'   Default `FALSE`.
#' @param nullable Logical scalar. If `TRUE`, `NULL` is also accepted. Default `FALSE`.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' # Learning rate in (0, 1]
#' lr_prop <- bounded_double_property(0, 1, lower_open = TRUE)
bounded_double_property <- function(
  lower = -Inf,
  upper = Inf,
  lower_open = FALSE,
  upper_open = FALSE,
  nullable = FALSE
) {
  lower_sym <- if (lower_open) "(" else "["
  upper_sym <- if (upper_open) ")" else "]"
  bound_desc <- paste0(
    "must be a finite double in ",
    lower_sym,
    lower,
    ", ",
    upper,
    upper_sym
  )

  check_lower <- if (lower_open) {
    function(v) v > lower
  } else {
    function(v) v >= lower
  }
  check_upper <- if (upper_open) {
    function(v) v < upper
  } else {
    function(v) v <= upper
  }

  cls <- if (nullable) new_union(NULL, class_double) else class_double

  new_property(
    class = cls,
    validator = function(value) {
      if (nullable && is.null(value)) {
        return(NULL)
      }
      if (length(value) != 1L || is.na(value) || !is.finite(value)) {
        return(paste0(bound_desc, " (must be a finite scalar)"))
      }
      if (!check_lower(value) || !check_upper(value)) {
        return(bound_desc)
      }
      NULL
    }
  )
}


# %% enum() ----
#' Create an enum S7 property
#'
#' Returns a `new_property()` for a character scalar constrained to a fixed set of allowed values.
#'
#' @param values Character: Allowed values.
#' @param default Optional Character: Default value.
#' @param nullable Logical scalar. If `TRUE`, `NULL` is also accepted. Default `FALSE`.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' type_prop <- enum(c("string", "number", "boolean"), default = "string")
enum <- function(values, default = NULL, nullable = FALSE) {
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
}


# %% optional ----
#' Create an optional S7 type
#'
#' Creates an S7 union type that allows for the specified type or `NULL`.
#'
#' This should be used when the S7 class already includes all the necessary validation for the
#' non-NULL case. Otherwise, create a new S7 property with appropriate validator using
#' `S7::new_property()`.
#'
#' @param type S7 base class or S7 class.
#' @return An S7 union type that allows for the specified type or `NULL`.
#' @author EDG
#' @export
#' @examples
#' # Create an optional character type
#' optional(S7::class_character)
optional <- function(type) {
  if (!inherits(type, "S7_base_class") && !inherits(type, "S7_class")) {
    cli::cli_abort(
      "{.var type} must be an S7 base class or S7 class."
    )
  }
  S7::new_union(NULL, type)
}
