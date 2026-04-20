# 2024- EDG rtemis.org

# check_* functions perform checks and throw error using cli::cli_abort if checks fail;
# do not return a value.

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
#' check_scalar_logical(TRUE, "my_arg") # Passes
#' check_scalar_logical(FALSE, "my_arg") # Passes
#' # Throw error:
#' try(check_scalar_logical(c(TRUE, FALSE), "my_arg"))
#' try(check_scalar_logical(NA, "my_arg"))
#' try(check_scalar_logical("TRUE", "my_arg"))
check_scalar_logical <- function(x, arg_name) {
  check_logical(x, allow_null = FALSE, arg_name = arg_name)
  if (length(x) != 1L) {
    cli::cli_abort("{.var {arg_name}} must be TRUE or FALSE. Use one value.")
  }
  invisible()
}


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
#' check_optional_scalar_character(NULL, "my_arg") # Passes
#' check_optional_scalar_character("hello", "my_arg") # Passes
#' # Throw error:
#' try(check_optional_scalar_character(c("hello", "world"), "my_arg"))
#' try(check_optional_scalar_character(123, "my_arg"))
check_optional_scalar_character <- function(x, arg_name) {
  check_character(x, allow_null = TRUE, arg_name = arg_name)
  if (!is.null(x) && length(x) != 1L) {
    cli::cli_abort(
      "{.var {arg_name}} must be NULL or a single string."
    )
  }
  invisible()
}
