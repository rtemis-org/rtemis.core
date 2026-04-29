# 2026- EDG rtemis.org

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
  class = new_union(class_character, NULL),
  default = NULL,
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
  class = new_union(class_double, NULL),
  default = NULL,
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
  class = new_union(class_integer, NULL),
  default = NULL,
  validator = function(value) {
    if (!is.null(value) && (length(value) != 1L || is.na(value))) {
      return("must be NULL or an integer scalar (e.g. 1L)")
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
  class = new_union(class_logical, NULL),
  default = NULL,
  validator = function(value) {
    if (!is.null(value) && (length(value) != 1L || is.na(value))) {
      return("must be NULL or a logical scalar (TRUE or FALSE)")
    }
    NULL
  }
)

# %% Bounded double ----
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
  class = new_union(class_double, NULL),
  default = NULL,
  validator = function(value) {
    if (!is.null(value) && (length(value) != 1L || is.na(value) || value < 0 || value > 1)) {
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
    if (length(value) != 1L || is.na(value) || !is.finite(value) || value <= 0) {
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
  class = new_union(class_double, NULL),
  default = NULL,
  validator = function(value) {
    if (!is.null(value) && (length(value) != 1L || is.na(value) || !is.finite(value) || value <= 0)) {
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
  class = new_union(class_double, NULL),
  default = NULL,
  validator = function(value) {
    if (!is.null(value) && (length(value) != 1L || is.na(value) || !is.finite(value) || value < 0)) {
      return("must be NULL or a finite non-negative double (>= 0)")
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
  bound_desc <- paste0("must be a finite double in ", lower_sym, lower, ", ", upper, upper_sym)

  check_lower <- if (lower_open) function(v) v > lower else function(v) v >= lower
  check_upper <- if (upper_open) function(v) v < upper else function(v) v <= upper

  cls <- if (nullable) new_union(class_double, NULL) else class_double

  new_property(
    class = cls,
    validator = function(value) {
      if (is.null(value)) return(NULL)
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
