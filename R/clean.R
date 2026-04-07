# clean.R
# ::rtemis.strict::
# 2024- EDG rtemis.org

# clean_* functions perform checks and return clean inputs.

# %% clean_int ----
#' Clean integer input
#'
#' @details
#' The goal is to return an integer vector.
#' If the input is integer, it is returned as is.
#' If the input is numeric, it is coerced to integer only if the numeric values are integers,
#' otherwise an error is thrown.
#'
#' @param x Double or integer vector to check.
#' @param xname Character: Name of the variable for error messages.
#'
#' @return Integer vector.
#' @author EDG
#'
#' @export
#'
#' @examples
#' clean_int(6L)
#' clean_int(3)
#' # clean_int(12.1) # Error
#' clean_int(c(3, 5, 7))
#' # clean_int(c(3, 5, 7.01)) # Error
clean_int <- function(x, xname = deparse(substitute(x))) {
  if (is.integer(x)) {
    return(x)
  } else if (is.numeric(x)) {
    if (all(x %% 1 == 0)) {
      return(as.integer(x))
    } else {
      cli::cli_abort("{.var {xname}} must be integer.")
    }
  } else if (is.null(x)) {
    return(NULL)
  }
  cli::cli_abort("{.var {xname}} must be integer.")
} # /rtemis.strict::clean_int


# %% clean_posint ----
#' Check positive integer
#'
#' @param x Integer vector.
#' @param allow_na Logical: If TRUE, NAs are excluded before checking. If FALSE (default),
#'   NAs trigger an error.
#' @param xname Character: Name of the variable for error messages.
#'
#' @return Integer vector of positive values.
#'
#' @author EDG
#' @export
#'
#' @examples
#' clean_posint(5)
clean_posint <- function(x, allow_na = FALSE, xname = deparse(substitute(x))) {
  if (is.null(x)) {
    return(NULL)
  }

  if (!allow_na && anyNA(x)) {
    cli::cli_abort("{.var {xname}} must not contain NAs.")
  } else {
    x <- na.exclude(x)
  }

  if (any(x <= 0)) {
    cli::cli_abort("{.var {xname}} must contain only positive integers.")
  }

  clean_int(x, xname = xname)
} # /rtemis.strict::clean_posint
