# 2024- EDG rtemis.org

# check_* functions perform checks and throw error using cli::cli_abort if checks fail;
# do not return a value.

# TOC ----
# General checks
#   check_inherits
#   check_logical
#   check_character
#   check_floatpos
#   check_float01exc
#   check_float01inc
#   check_floatpos1
#   check_float0pos
#   check_float_neg1_1
#   check_dependencies
#   check_data.table
#   check_tabular
#   check_enum
# S7-parallel scalar checks
#   check_character_scalar
#   check_optional_character_scalar
#   check_double_scalar
#   check_optional_double_scalar
#   check_integer_scalar
#   check_optional_integer_scalar
#   check_pos_integer_scalar
#   check_optional_pos_integer_scalar
#   check_logical_scalar
#   check_optional_logical_scalar
#   check_prob_scalar
#   check_optional_prob_scalar
#   check_unit_open_scalar
#   check_pos_double_scalar
#   check_optional_pos_double_scalar
#   check_nonneg_double_scalar
#   check_optional_nonneg_double_scalar
# S7-parallel vector checks
#   check_prob_vector
#   check_optional_prob_vector
#   check_unit_open_vector
#   check_optional_unit_open_vector
#   check_pos_double_vector
#   check_optional_pos_double_vector
#   check_nonneg_double_vector
#   check_optional_nonneg_double_vector
# Legacy (superseded by check_*_scalar equivalents above)
#   check_scalar_logical
#   check_scalar_character
#   check_optional_scalar_character

# %% check_inherits ----
#' Check class of object
#'
#' @param x Object to check.
#' @param cl Character: class to check against.
#' @param allow_null Logical: If TRUE, NULL values are allowed and return early.
#' @param arg_name Character: Name of the variable for error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#'
#' @export
#'
#' @examples
#' check_inherits("papaya", "character")
#' # These will throw errors:
#' try(check_inherits(c(1, 2.5, 3.2), "integer"))
#' try(check_inherits(iris, "list"))
check_inherits <- function(
  x,
  cl,
  allow_null = TRUE,
  arg_name = deparse(substitute(x))
) {
  if (allow_null && is.null(x)) {
    return(invisible())
  }

  if (is.null(x)) {
    cli::cli_abort("{.var {arg_name}} cannot be NULL.")
  }

  if (!inherits(x, cl)) {
    cli::cli_abort(
      "{.var {arg_name}} must be of class {.cls {cl}}."
    )
  }

  invisible()
} # /rtemis.core::check_inherits


# %% check_logical ----
#' Check logical
#'
#' @param x Vector to check.
#' @param allow_null Logical: If TRUE, NULL values are allowed and return early.
#' @param arg_name Character: Name of the variable for error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_logical(c(TRUE, FALSE))
#' # Throws error:
#' try(check_logical(c(0, 1)))
check_logical <- function(
  x,
  allow_null = TRUE,
  arg_name = deparse(substitute(x))
) {
  if (allow_null && is.null(x)) {
    return(invisible())
  }

  if (is.null(x)) {
    cli::cli_abort("{.var {arg_name}} cannot be NULL.")
  }

  if (anyNA(x)) {
    cli::cli_abort("{.var {arg_name}} must not contain NAs.")
  }
  if (!is.logical(x)) {
    cli::cli_abort("{.var {arg_name}} must be logical.")
  }

  invisible()
} # /rtemis.core::check_logical


# %% check_character ----
#' Check character
#'
#' @param x Vector to check.
#' @param allow_null Logical: If TRUE, NULL values are allowed and return early.
#' @param arg_name Character: Name of the variable for error messages.
#'
#' @return Called for side effects. Throws an error if check fails.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_character("papaya")
#' # Throws error:
#' try(check_character(42L))
check_character <- function(
  x,
  allow_null = TRUE,
  arg_name = deparse(substitute(x))
) {
  if (allow_null && is.null(x)) {
    return(invisible())
  }

  if (is.null(x)) {
    cli::cli_abort("{.var {arg_name}} cannot be NULL.")
  }

  if (anyNA(x)) {
    cli::cli_abort("{.var {arg_name}} must not contain NAs.")
  }
  if (!is.character(x)) {
    cli::cli_abort("{.var {arg_name}} must be character.")
  }

  invisible()
} # /rtemis.core::check_character


# %% check_floatpos ----
#' Check positive float
#'
#' @details
#' Checking with `is.numeric()` allows integer inputs as well, which should be ok since it is
#' unlikely the function that consumes this will enforce double type only, but instead is most
#' likely to allow implicit coercion from integer to numeric.
#'
#' @param x Numeric vector.
#' @param allow_null Logical: If TRUE, NULL values are allowed and return early.
#' @param arg_name Character: Name of the variable for error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_floatpos(c(0.5, 1.5))
#' # Allows integers since they are numeric and can be coerced to double without loss of information
#' check_floatpos(c(1L, 3L))
#' # Throws error:
#' try(check_floatpos(c(-1.5, 0.5, 1.5)))
check_floatpos <- function(
  x,
  allow_null = TRUE,
  arg_name = deparse(substitute(x))
) {
  if (allow_null && is.null(x)) {
    return(invisible())
  }

  if (is.null(x)) {
    cli::cli_abort("{.var {arg_name}} cannot be NULL.")
  }

  if (!is.numeric(x)) {
    cli::cli_abort("{.var {arg_name}} must be numeric.")
  }

  if (anyNA(x)) {
    cli::cli_abort("{.var {arg_name}} must not contain NAs.")
  }

  if (any(x <= 0)) {
    cli::cli_abort("{.var {arg_name}} must be greater than 0.")
  }

  invisible()
} # /rtemis.core::check_floatpos


# %% check_float01exc ----
#' Check float between 0 and 1, exclusive
#'
#' @param x Numeric vector.
#' @param allow_null Logical: If TRUE, NULL values are allowed and return early.
#' @param arg_name Character: Name of the variable for error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_float01exc(c(0.2, 0.7))
#' # Throws error:
#' try(check_float01exc(c(0, 0.5, 1)))
check_float01exc <- function(
  x,
  allow_null = TRUE,
  arg_name = deparse(substitute(x))
) {
  if (allow_null && is.null(x)) {
    return(invisible())
  }

  if (is.null(x)) {
    cli::cli_abort("{.var {arg_name}} cannot be NULL.")
  }

  if (!is.numeric(x)) {
    cli::cli_abort("{.var {arg_name}} must be numeric.")
  }

  if (anyNA(x)) {
    cli::cli_abort("{.var {arg_name}} must not contain NAs.")
  }

  if (any(x <= 0 | x >= 1)) {
    cli::cli_abort(
      "{.var {arg_name}} must be between 0 and 1, exclusive."
    )
  }

  invisible()
} # /rtemis.core::check_float01exc


# %% check_float01inc ----
#' Check float between 0 and 1, inclusive
#'
#' @param x Numeric vector.
#' @param allow_null Logical: If TRUE, NULL values are allowed and return early.
#' @param arg_name Character: Name of the variable for error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_float01inc(0.5)
check_float01inc <- function(
  x,
  allow_null = TRUE,
  arg_name = deparse(substitute(x))
) {
  if (allow_null && is.null(x)) {
    return(invisible())
  }

  if (is.null(x)) {
    cli::cli_abort("{.var {arg_name}} cannot be NULL.")
  }

  if (!is.numeric(x)) {
    cli::cli_abort(
      "{.var {arg_name}} must be numeric. Received: {.val {x}} of class {class(x)}"
    )
  }

  if (anyNA(x)) {
    cli::cli_abort("{.var {arg_name}} must not contain NAs.")
  }

  if (any(x < 0 | x > 1)) {
    cli::cli_abort("{.var {arg_name}} must be between 0 and 1, inclusive.")
  }

  invisible()
} # /rtemis.core::check_float01inc


# %% check_floatpos1 ----
#' Check float in (0, 1]
#'
#' @param x Numeric vector.
#' @param allow_null Logical: If TRUE, NULL values are allowed and return early.
#' @param arg_name Character: Name of the variable for error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_floatpos1(c(0.5, 1))
#' # Throw error:
#' try(check_floatpos1(c(0, 0.7)))
#' try(check_floatpos1(c(0.5, 1.5)))
check_floatpos1 <- function(
  x,
  allow_null = TRUE,
  arg_name = deparse(substitute(x))
) {
  if (allow_null && is.null(x)) {
    return(invisible())
  }

  if (is.null(x)) {
    cli::cli_abort("{.var {arg_name}} cannot be NULL.")
  }

  if (!is.numeric(x)) {
    cli::cli_abort("{.var {arg_name}} must be numeric.")
  }

  if (anyNA(x)) {
    cli::cli_abort("{.var {arg_name}} must not contain NAs.")
  }

  if (any(x <= 0) || any(x > 1)) {
    cli::cli_abort(
      "{.var {arg_name}} must be greater than 0 and less or equal to 1."
    )
  }

  invisible()
} # /rtemis.core::check_floatpos1


# %% check_float0pos ----
#' Check float greater than or equal to 0
#'
#' Checks if an input is a numeric vector containing non-negative
#'   (>= 0) values and no `NA`s. It is designed to validate function arguments.
#'
#' @param x Numeric vector.
#' @param allow_null Logical: If TRUE, NULL values are allowed and return early.
#' @param arg_name Character: Name of the variable for error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_float0pos(c(0, 0.5, 1))
#' # Allows integers since they are numeric and can be coerced to double without loss of information
#' check_float0pos(c(0L, 1L))
#' # Throws error:
#' try(check_float0pos(c(-1.5, 0, 1.5)))
check_float0pos <- function(
  x,
  allow_null = TRUE,
  arg_name = deparse(substitute(x))
) {
  if (allow_null && is.null(x)) {
    return(invisible())
  }

  if (is.null(x)) {
    cli::cli_abort("{.var {arg_name}} cannot be NULL.")
  }

  if (!is.numeric(x)) {
    cli::cli_abort("{.var {arg_name}} must be numeric.")
  }

  if (anyNA(x)) {
    cli::cli_abort("{.var {arg_name}} must not contain NAs.")
  }

  if (any(x < 0)) {
    cli::cli_abort("{.var {arg_name}} must be zero or greater.")
  }

  invisible()
} # /rtemis.core::check_float0pos


# %% check_float_neg1_1 ----
#' Check float -1 <= x <= 1
#'
#' @param x Numeric vector.
#' @param allow_null Logical: If TRUE, NULL values are allowed and return early.
#' @param arg_name Character: Name of the variable for error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_float_neg1_1(c(-1, 0, 1))
#' # Throws error:
#' try(check_float_neg1_1(c(-1.5, 0, 1.5)))
check_float_neg1_1 <- function(
  x,
  allow_null = TRUE,
  arg_name = deparse(substitute(x))
) {
  if (allow_null && is.null(x)) {
    return(invisible())
  }

  if (is.null(x)) {
    cli::cli_abort("{.var {arg_name}} cannot be NULL.")
  }

  if (!is.numeric(x)) {
    cli::cli_abort("{.var {arg_name}} must be numeric.")
  }

  if (anyNA(x)) {
    cli::cli_abort("{.var {arg_name}} must not contain NAs.")
  }

  if (any(x < -1 | x > 1)) {
    cli::cli_abort("{.var {arg_name}} must be between -1 and 1, inclusive.")
  }

  invisible()
} # /rtemis.core::check_float_neg1_1


# %% check_dependencies ----
#' \pkg{rtemis.core} internal: Dependencies check
#'
#' Checks if dependencies can be loaded; names missing dependencies if not.
#'
#' @param ... List or vector of strings defining namespaces to be checked
#' @param verbosity Integer: Verbosity level.
#' Note: An error will always printed if dependencies are missing.
#' Setting this to FALSE stops it from printing
#' "Dependencies check passed".
#'
#' @return Called for side effects. Aborts and prints list of missing dependencies, if any.
#'
#' @author EDG
#'
#' @export
#'
#' @examples
#' check_dependencies("base")
#' # Throws error:
#' try(check_dependencies("zlorbglorb"))
check_dependencies <- function(..., verbosity = 0L) {
  ns <- as.list(c(...))
  err <- !sapply(ns, \(i) requireNamespace(i, quietly = TRUE))
  if (any(err)) {
    cli::cli_abort(
      paste0(
        "Please install the following ",
        ngettext(sum(err), "dependency", "dependencies"),
        ":\n",
        pastels(ns[err], bullet = "    -")
      )
    )
  } else {
    if (verbosity > 0L) msg("Dependency check passed")
  }
  invisible()
} # /rtemis.core::check_dependencies


# %% check_data.table ----
#' Check data.table
#'
#' @param x Object to check.
#' @param arg_name Character: Name of the variable for error messages.
#'
#' @return Called for side effects. Throws an error if input is not a data.table, returns x
#' invisibly otherwise.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_data.table(data.table::as.data.table(iris))
#' # Throws error:
#' try(check_data.table(iris))
check_data.table <- function(x, arg_name = deparse(substitute(x))) {
  if (!data.table::is.data.table(x)) {
    cli::cli_abort("{.var {arg_name}} must be a data.table.")
  }
  invisible(x)
} # /rtemis.core::check_data.table


# %% check_tabular ----
#' Check object is tabular
#'
#' Checks if object is of class `data.frame`, `data.table`, or `tbl_df`.
#'
#' @param x Object to check.
#'
#' @return Called for side effects. Throws an error if input is not tabular, returns x invisibly
#' otherwise.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_tabular(iris)
#' check_tabular(data.table::as.data.table(iris))
#' # Throws error:
#' try(check_tabular(matrix(1:10, ncol = 2)))
check_tabular <- function(x) {
  if (!inherits(x, c("data.frame", "data.table", "tbl_df"))) {
    cli::cli_abort(
      "{.var {deparse(substitute(x))}} must be a data.frame, data.table, or tbl_df."
    )
  }
  invisible(x)
} # /rtemis.core::check_tabular


# %% check_enum ----
#' Check if value is in set of allowed values
#'
#' Checks if a value is in a set of allowed values, and throws an error if not.
#'
#' @param x Value to check.
#' @param allowed_values Vector of allowed values.
#' @param arg_name Character: Name of the variable for error messages.
#'
#' @return Called for side effects. Throws an error if x is not in allowed_values, returns x
#'   invisibly otherwise.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_enum("apple", c("apple", "banana", "cherry"))
#' # Throws error:
#' try(check_enum("granola", c("croissant", "bagel", "scramble")))
check_enum <- function(x, allowed_values, arg_name = deparse(substitute(x))) {
  if (!x %in% allowed_values) {
    cli::cli_abort(
      "{.var {arg_name}} must be one of: {.val {allowed_values}}. Received: {.val {x}}"
    )
  }
  invisible(x)
} # /rtemis.core::check_enum


# %% S7-parallel scalar checks ----
# Naming aligns with the S7 properties in 00_S7_properties.R where matching
# scalar check helpers are implemented.
# Use these in regular function bodies for early, user-friendly argument validation.

# %% check_character_scalar ----
#' Check character scalar
#'
#' @param x Character: Value to check. Must be a single non-NA, non-empty string.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_character_scalar("hello")
#' # Throw error:
#' try(check_character_scalar(""))
#' try(check_character_scalar(NA_character_))
#' try(check_character_scalar(c("a", "b")))
check_character_scalar <- function(x, arg_name = deparse(substitute(x))) {
  if (!is.character(x)) {
    cli::cli_abort("{.var {arg_name}} must be a character string.")
  }
  if (length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    cli::cli_abort("{.var {arg_name}} must be a single non-empty string.")
  }
  invisible()
} # /rtemis.core::check_character_scalar


# %% check_optional_character_scalar ----
#' Check optional character scalar
#'
#' @param x Optional Character: Value to check. Must be `NULL` or a single non-NA, non-empty string.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_optional_character_scalar(NULL)
#' check_optional_character_scalar("hello")
#' # Throw error:
#' try(check_optional_character_scalar(""))
#' try(check_optional_character_scalar(c("a", "b")))
check_optional_character_scalar <- function(
  x,
  arg_name = deparse(substitute(x))
) {
  if (is.null(x)) {
    return(invisible())
  }
  check_character_scalar(x, arg_name = arg_name)
  invisible()
} # /rtemis.core::check_optional_character_scalar


# %% check_double_scalar ----
#' Check double scalar
#'
#' @param x Numeric: Value to check. Must be a single non-NA number (integer inputs are accepted).
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_double_scalar(3.14)
#' check_double_scalar(1L)
#' # Throw error:
#' try(check_double_scalar(NA_real_))
#' try(check_double_scalar(c(1.0, 2.0)))
check_double_scalar <- function(x, arg_name = deparse(substitute(x))) {
  if (!is.numeric(x)) {
    cli::cli_abort("{.var {arg_name}} must be numeric.")
  }
  if (length(x) != 1L || is.na(x)) {
    cli::cli_abort("{.var {arg_name}} must be a single non-NA number.")
  }
  invisible()
} # /rtemis.core::check_double_scalar


# %% check_optional_double_scalar ----
#' Check optional double scalar
#'
#' @param x Optional Numeric: Value to check. Must be `NULL` or a single non-NA number.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_optional_double_scalar(NULL)
#' check_optional_double_scalar(2.5)
#' # Throw error:
#' try(check_optional_double_scalar(NA_real_))
check_optional_double_scalar <- function(x, arg_name = deparse(substitute(x))) {
  if (is.null(x)) {
    return(invisible())
  }
  check_double_scalar(x, arg_name = arg_name)
  invisible()
} # /rtemis.core::check_optional_double_scalar


# %% check_integer_scalar ----
#' Check integer scalar
#'
#' @details
#' Accepts any single numeric value that is a whole number. Integer-typed inputs (`1L`) and
#' double-typed whole numbers (`1`, `100`) are both accepted for user convenience.
#'
#' @param x Numeric: Value to check. Must be a single non-NA whole number.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_integer_scalar(5L)
#' check_integer_scalar(100)
#' # Throw error:
#' try(check_integer_scalar(1.5))
#' try(check_integer_scalar(NA_integer_))
check_integer_scalar <- function(x, arg_name = deparse(substitute(x))) {
  if (!is.numeric(x)) {
    cli::cli_abort("{.var {arg_name}} must be numeric.")
  }
  if (length(x) != 1L || is.na(x) || !is.finite(x)) {
    cli::cli_abort("{.var {arg_name}} must be a single finite non-NA number.")
  }
  if (x != round(x)) {
    cli::cli_abort("{.var {arg_name}} must be a whole number.")
  }
  invisible()
} # /rtemis.core::check_integer_scalar


# %% check_optional_integer_scalar ----
#' Check optional integer scalar
#'
#' @param x Optional Numeric: Value to check. Must be `NULL` or a single non-NA whole number.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_optional_integer_scalar(NULL)
#' check_optional_integer_scalar(10L)
#' # Throw error:
#' try(check_optional_integer_scalar(1.5))
check_optional_integer_scalar <- function(
  x,
  arg_name = deparse(substitute(x))
) {
  if (is.null(x)) {
    return(invisible())
  }
  check_integer_scalar(x, arg_name = arg_name)
  invisible()
} # /rtemis.core::check_optional_integer_scalar


# %% check_pos_integer_scalar ----
#' Check positive integer scalar
#'
#' @details
#' Accepts any single numeric value that is a whole number strictly greater than zero.
#' Integer-typed inputs (`1L`) and double-typed whole numbers (`1`, `100`) are both accepted for
#' user convenience.
#'
#' @param x Numeric: Value to check. Must be a single non-NA whole number greater than zero.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_pos_integer_scalar(1L)
#' check_pos_integer_scalar(10)
#' # Throw error:
#' try(check_pos_integer_scalar(0))
#' try(check_pos_integer_scalar(-1L))
#' try(check_pos_integer_scalar(1.5))
check_pos_integer_scalar <- function(x, arg_name = deparse(substitute(x))) {
  check_integer_scalar(x, arg_name = arg_name)
  if (x <= 0) {
    cli::cli_abort("{.var {arg_name}} must be a whole number greater than 0.")
  }
  invisible()
} # /rtemis.core::check_pos_integer_scalar


# %% check_optional_pos_integer_scalar ----
#' Check optional positive integer scalar
#'
#' @param x Optional Numeric: Value to check. Must be `NULL` or a single non-NA whole number
#'   greater than zero.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_optional_pos_integer_scalar(NULL)
#' check_optional_pos_integer_scalar(5L)
#' # Throw error:
#' try(check_optional_pos_integer_scalar(0))
check_optional_pos_integer_scalar <- function(
  x,
  arg_name = deparse(substitute(x))
) {
  if (is.null(x)) {
    return(invisible())
  }
  check_pos_integer_scalar(x, arg_name = arg_name)
  invisible()
} # /rtemis.core::check_optional_pos_integer_scalar


# %% check_logical_scalar ----
#' Check logical scalar
#'
#' @param x Logical: Value to check. Must be a single non-NA `TRUE` or `FALSE`.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_logical_scalar(TRUE)
#' check_logical_scalar(FALSE)
#' # Throw error:
#' try(check_logical_scalar(NA))
#' try(check_logical_scalar(1L))
#' try(check_logical_scalar(c(TRUE, FALSE)))
check_logical_scalar <- function(x, arg_name = deparse(substitute(x))) {
  if (!is.logical(x)) {
    cli::cli_abort("{.var {arg_name}} must be TRUE or FALSE.")
  }
  if (length(x) != 1L || is.na(x)) {
    cli::cli_abort("{.var {arg_name}} must be a single TRUE or FALSE.")
  }
  invisible()
} # /rtemis.core::check_logical_scalar


# %% check_optional_logical_scalar ----
#' Check optional logical scalar
#'
#' @param x Optional Logical: Value to check. Must be `NULL` or a single non-NA `TRUE` or `FALSE`.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_optional_logical_scalar(NULL)
#' check_optional_logical_scalar(FALSE)
#' # Throw error:
#' try(check_optional_logical_scalar(NA))
check_optional_logical_scalar <- function(
  x,
  arg_name = deparse(substitute(x))
) {
  if (is.null(x)) {
    return(invisible())
  }
  check_logical_scalar(x, arg_name = arg_name)
  invisible()
} # /rtemis.core::check_optional_logical_scalar


# %% check_prob_scalar ----
#' Check probability scalar
#'
#' @param x Numeric: Value to check. Must be a single finite number in \eqn{[0, 1]}.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_prob_scalar(0)
#' check_prob_scalar(0.5)
#' check_prob_scalar(1)
#' # Throw error:
#' try(check_prob_scalar(1.5))
#' try(check_prob_scalar(-0.1))
check_prob_scalar <- function(x, arg_name = deparse(substitute(x))) {
  check_double_scalar(x, arg_name = arg_name)
  if (x < 0 || x > 1) {
    cli::cli_abort("{.var {arg_name}} must be in [0, 1].")
  }
  invisible()
} # /rtemis.core::check_prob_scalar


# %% check_optional_prob_scalar ----
#' Check optional probability scalar
#'
#' @param x Optional Numeric: Value to check. Must be `NULL` or a single finite number in \eqn{[0, 1]}.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_optional_prob_scalar(NULL)
#' check_optional_prob_scalar(0.5)
#' # Throw error:
#' try(check_optional_prob_scalar(2.0))
check_optional_prob_scalar <- function(x, arg_name = deparse(substitute(x))) {
  if (is.null(x)) {
    return(invisible())
  }
  check_prob_scalar(x, arg_name = arg_name)
  invisible()
} # /rtemis.core::check_optional_prob_scalar


# %% check_unit_open_scalar ----
#' Check open-unit-interval scalar
#'
#' @param x Numeric: Value to check. Must be a single finite number strictly in \eqn{(0, 1)}.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_unit_open_scalar(0.5)
#' # Throw error:
#' try(check_unit_open_scalar(0))
#' try(check_unit_open_scalar(1))
check_unit_open_scalar <- function(x, arg_name = deparse(substitute(x))) {
  check_double_scalar(x, arg_name = arg_name)
  if (x <= 0 || x >= 1) {
    cli::cli_abort("{.var {arg_name}} must be strictly in (0, 1).")
  }
  invisible()
} # /rtemis.core::check_unit_open_scalar


# %% check_pos_double_scalar ----
#' Check positive double scalar
#'
#' @param x Numeric: Value to check. Must be a single finite number strictly greater than zero.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_pos_double_scalar(0.001)
#' check_pos_double_scalar(100)
#' # Throw error:
#' try(check_pos_double_scalar(0))
#' try(check_pos_double_scalar(-1))
#' try(check_pos_double_scalar(Inf))
check_pos_double_scalar <- function(x, arg_name = deparse(substitute(x))) {
  check_double_scalar(x, arg_name = arg_name)
  if (!is.finite(x) || x <= 0) {
    cli::cli_abort("{.var {arg_name}} must be a finite number greater than 0.")
  }
  invisible()
} # /rtemis.core::check_pos_double_scalar


# %% check_optional_pos_double_scalar ----
#' Check optional positive double scalar
#'
#' @param x Optional Numeric: Value to check. Must be `NULL` or a single finite number
#'   strictly greater than zero.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_optional_pos_double_scalar(NULL)
#' check_optional_pos_double_scalar(2.5)
#' # Throw error:
#' try(check_optional_pos_double_scalar(0))
check_optional_pos_double_scalar <- function(
  x,
  arg_name = deparse(substitute(x))
) {
  if (is.null(x)) {
    return(invisible())
  }
  check_pos_double_scalar(x, arg_name = arg_name)
  invisible()
} # /rtemis.core::check_optional_pos_double_scalar


# %% check_nonneg_double_scalar ----
#' Check non-negative double scalar
#'
#' @param x Numeric: Value to check. Must be a single finite number greater than or equal to zero.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_nonneg_double_scalar(0)
#' check_nonneg_double_scalar(5)
#' # Throw error:
#' try(check_nonneg_double_scalar(-0.001))
#' try(check_nonneg_double_scalar(Inf))
check_nonneg_double_scalar <- function(x, arg_name = deparse(substitute(x))) {
  check_double_scalar(x, arg_name = arg_name)
  if (!is.finite(x) || x < 0) {
    cli::cli_abort("{.var {arg_name}} must be a finite number >= 0.")
  }
  invisible()
} # /rtemis.core::check_nonneg_double_scalar


# %% check_optional_nonneg_double_scalar ----
#' Check optional non-negative double scalar
#'
#' @param x Optional Numeric: Value to check. Must be `NULL` or a single finite number
#'   greater than or equal to zero.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_optional_nonneg_double_scalar(NULL)
#' check_optional_nonneg_double_scalar(0)
#' # Throw error:
#' try(check_optional_nonneg_double_scalar(-1))
check_optional_nonneg_double_scalar <- function(
  x,
  arg_name = deparse(substitute(x))
) {
  if (is.null(x)) {
    return(invisible())
  }
  check_nonneg_double_scalar(x, arg_name = arg_name)
  invisible()
} # /rtemis.core::check_optional_nonneg_double_scalar


# %% S7-parallel vector checks ----

# %% check_prob_vector ----
#' Check probability vector
#'
#' @param x Numeric: Value to check. Must be a non-empty vector with all elements in \eqn{[0, 1]}
#'   and no NAs.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_prob_vector(c(0, 0.5, 1))
#' # Throw error:
#' try(check_prob_vector(c(0.5, 1.5)))
#' try(check_prob_vector(c(0.5, NA)))
check_prob_vector <- function(x, arg_name = deparse(substitute(x))) {
  if (!is.numeric(x)) {
    cli::cli_abort("{.var {arg_name}} must be numeric.")
  }
  if (length(x) == 0L) {
    cli::cli_abort("{.var {arg_name}} must be non-empty.")
  }
  if (anyNA(x)) {
    cli::cli_abort("{.var {arg_name}} must not contain NAs.")
  }
  if (any(x < 0) || any(x > 1)) {
    cli::cli_abort("{.var {arg_name}} all elements must be in [0, 1].")
  }
  invisible()
} # /rtemis.core::check_prob_vector


# %% check_optional_prob_vector ----
#' Check optional probability vector
#'
#' @param x Optional Numeric: Value to check. Must be `NULL` or a non-empty vector with all
#'   elements in \eqn{[0, 1]} and no NAs.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_optional_prob_vector(NULL)
#' check_optional_prob_vector(c(0.2, 0.8))
#' # Throw error:
#' try(check_optional_prob_vector(c(0.5, 2)))
check_optional_prob_vector <- function(x, arg_name = deparse(substitute(x))) {
  if (is.null(x)) {
    return(invisible())
  }
  check_prob_vector(x, arg_name = arg_name)
  invisible()
} # /rtemis.core::check_optional_prob_vector


# %% check_unit_open_vector ----
#' Check open-unit-interval vector
#'
#' @param x Numeric: Value to check. Must be a non-empty vector with all elements strictly in
#'   \eqn{(0, 1)} and no NAs.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_unit_open_vector(c(0.2, 0.5, 0.9))
#' # Throw error:
#' try(check_unit_open_vector(c(0, 0.5)))
#' try(check_unit_open_vector(c(0.5, 1)))
check_unit_open_vector <- function(x, arg_name = deparse(substitute(x))) {
  if (!is.numeric(x)) {
    cli::cli_abort("{.var {arg_name}} must be numeric.")
  }
  if (length(x) == 0L) {
    cli::cli_abort("{.var {arg_name}} must be non-empty.")
  }
  if (anyNA(x)) {
    cli::cli_abort("{.var {arg_name}} must not contain NAs.")
  }
  if (any(x <= 0) || any(x >= 1)) {
    cli::cli_abort("{.var {arg_name}} all elements must be strictly in (0, 1).")
  }
  invisible()
} # /rtemis.core::check_unit_open_vector


# %% check_optional_unit_open_vector ----
#' Check optional open-unit-interval vector
#'
#' @param x Optional Numeric: Value to check. Must be `NULL` or a non-empty vector with all
#'   elements strictly in \eqn{(0, 1)} and no NAs.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_optional_unit_open_vector(NULL)
#' check_optional_unit_open_vector(c(0.1, 0.9))
#' # Throw error:
#' try(check_optional_unit_open_vector(c(0, 0.5)))
check_optional_unit_open_vector <- function(
  x,
  arg_name = deparse(substitute(x))
) {
  if (is.null(x)) {
    return(invisible())
  }
  check_unit_open_vector(x, arg_name = arg_name)
  invisible()
} # /rtemis.core::check_optional_unit_open_vector


# %% check_pos_double_vector ----
#' Check positive double vector
#'
#' @param x Numeric: Value to check. Must be a non-empty vector with all elements finite,
#'   strictly greater than zero, and no NAs.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_pos_double_vector(c(0.1, 1, 10))
#' # Throw error:
#' try(check_pos_double_vector(c(0, 1)))
#' try(check_pos_double_vector(c(1, Inf)))
check_pos_double_vector <- function(x, arg_name = deparse(substitute(x))) {
  if (!is.numeric(x)) {
    cli::cli_abort("{.var {arg_name}} must be numeric.")
  }
  if (length(x) == 0L) {
    cli::cli_abort("{.var {arg_name}} must be non-empty.")
  }
  if (anyNA(x)) {
    cli::cli_abort("{.var {arg_name}} must not contain NAs.")
  }
  if (!all(is.finite(x)) || any(x <= 0)) {
    cli::cli_abort("{.var {arg_name}} all elements must be finite and > 0.")
  }
  invisible()
} # /rtemis.core::check_pos_double_vector


# %% check_optional_pos_double_vector ----
#' Check optional positive double vector
#'
#' @param x Optional Numeric: Value to check. Must be `NULL` or a non-empty vector with all
#'   elements finite, strictly greater than zero, and no NAs.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_optional_pos_double_vector(NULL)
#' check_optional_pos_double_vector(c(0.5, 2))
#' # Throw error:
#' try(check_optional_pos_double_vector(c(0, 1)))
check_optional_pos_double_vector <- function(
  x,
  arg_name = deparse(substitute(x))
) {
  if (is.null(x)) {
    return(invisible())
  }
  check_pos_double_vector(x, arg_name = arg_name)
  invisible()
} # /rtemis.core::check_optional_pos_double_vector


# %% check_nonneg_double_vector ----
#' Check non-negative double vector
#'
#' @param x Numeric: Value to check. Must be a non-empty vector with all elements finite,
#'   greater than or equal to zero, and no NAs.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_nonneg_double_vector(c(0, 1, 2.5))
#' # Throw error:
#' try(check_nonneg_double_vector(c(-1, 0, 1)))
#' try(check_nonneg_double_vector(c(1, Inf)))
check_nonneg_double_vector <- function(x, arg_name = deparse(substitute(x))) {
  if (!is.numeric(x)) {
    cli::cli_abort("{.var {arg_name}} must be numeric.")
  }
  if (length(x) == 0L) {
    cli::cli_abort("{.var {arg_name}} must be non-empty.")
  }
  if (anyNA(x)) {
    cli::cli_abort("{.var {arg_name}} must not contain NAs.")
  }
  if (!all(is.finite(x)) || any(x < 0)) {
    cli::cli_abort("{.var {arg_name}} all elements must be finite and >= 0.")
  }
  invisible()
} # /rtemis.core::check_nonneg_double_vector


# %% check_optional_nonneg_double_vector ----
#' Check optional non-negative double vector
#'
#' @param x Optional Numeric: Value to check. Must be `NULL` or a non-empty vector with all
#'   elements finite, greater than or equal to zero, and no NAs.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects. Throws an error if checks fail.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_optional_nonneg_double_vector(NULL)
#' check_optional_nonneg_double_vector(c(0, 1, 5))
#' # Throw error:
#' try(check_optional_nonneg_double_vector(c(-1, 0)))
check_optional_nonneg_double_vector <- function(
  x,
  arg_name = deparse(substitute(x))
) {
  if (is.null(x)) {
    return(invisible())
  }
  check_nonneg_double_vector(x, arg_name = arg_name)
  invisible()
} # /rtemis.core::check_optional_nonneg_double_vector


# %% Legacy - to be removed ----
# check_scalar_* are superseded by the check_*_scalar equivalents above.

# %% check_scalar_logical ----
#' Check Scalar Logical
#'
#' @param x Logical: Value to check.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_scalar_logical(TRUE, "my_arg")
#' # Throw error:
#' try(check_scalar_logical(c(TRUE, FALSE), "my_arg"))
#' try(check_scalar_logical(NA, "my_arg"))
check_scalar_logical <- function(x, arg_name = deparse(substitute(x))) {
  check_logical(x, allow_null = FALSE, arg_name = arg_name)
  if (length(x) != 1L) {
    cli::cli_abort("{.var {arg_name}} must be a single TRUE or FALSE value.")
  }
  invisible()
} # /rtemis.core::check_scalar_logical


# %% check_scalar_character ----
#' Check Scalar Character
#'
#' @param x Character: Value to check.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_scalar_character("hello", "my_arg")
#' # Throw error:
#' try(check_scalar_character(c("hello", "world"), "my_arg"))
#' try(check_scalar_character(123, "my_arg"))
check_scalar_character <- function(x, arg_name = deparse(substitute(x))) {
  check_character(x, allow_null = FALSE, arg_name = arg_name)
  if (length(x) != 1L) {
    cli::cli_abort("{.var {arg_name}} must be a single string.")
  }
  invisible()
} # /rtemis.core::check_scalar_character


# %% check_optional_scalar_character ----
#' Check Optional Scalar Character
#'
#' @param x Optional Character: Value to check.
#' @param arg_name Character: Argument name to use in error messages.
#'
#' @return Called for side effects.
#'
#' @author EDG
#' @export
#'
#' @examples
#' check_optional_scalar_character(NULL, "my_arg")
#' check_optional_scalar_character("hello", "my_arg")
#' # Throw error:
#' try(check_optional_scalar_character(c("hello", "world"), "my_arg"))
#' try(check_optional_scalar_character(123, "my_arg"))
check_optional_scalar_character <- function(
  x,
  arg_name = deparse(substitute(x))
) {
  check_character(x, allow_null = TRUE, arg_name = arg_name)
  if (!is.null(x) && length(x) != 1L) {
    cli::cli_abort("{.var {arg_name}} must be NULL or a single string.")
  }
  invisible()
} # /rtemis.core::check_optional_scalar_character
