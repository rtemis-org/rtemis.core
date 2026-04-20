# 2024- EDG rtemis.org

# clean_* functions perform checks if needed and return clean inputs.

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
} # /rtemis.core::clean_int


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
} # /rtemis.core::clean_posint


#' Clean names
#'
#' Clean character vector by replacing all symbols and sequences of symbols with single
#' underscores, ensuring no name begins or ends with a symbol
#'
#' @param x Character vector.
#' @param prefix_digits Character: prefix to add to names beginning with a
#' digit. Set to NA to skip.
#'
#' @return Character vector.
#'
#' @author EDG
#' @export
#'
#' @examples
#' x <- c("Patient ID", "_Date-of-Birth", "SBP (mmHg)")
#' x
#' clean_names(x)
clean_names <- function(x, prefix_digits = "V_") {
  xc <- gsub("[^[:alnum:]]{1,}", "_", x)
  xc <- gsub("^_|_$", "", xc)
  if (!is.na(prefix_digits)) {
    sn_idi <- grep("^[0-9]", xc)
    xc[sn_idi] <- paste0(prefix_digits, xc[sn_idi])
  }
  xc
}


#' Clean column names
#'
#' Clean column names by replacing all spaces and punctuation with a single underscore
#'
#' @param x Character vector or matrix with colnames or any object with `names()` method.
#'
#' @return Character vector.
#'
#' @author EDG
#' @export
#'
#' @examples
#' clean_colnames(iris)
clean_colnames <- function(x) {
  if (!inherits(x, "character")) {
    x <- if (inherits(x, "matrix")) colnames(x) else names(x)
  }
  clean_names(x)
}
