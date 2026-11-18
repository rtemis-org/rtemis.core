# 2015- EDG rtemis.org

#' Format Numbers for Printing
#'
#' Fixed decimal places, otherwise scientific notation
#'
#' Formats numbers to `decimal_places` decimal places. A nonzero number that
#' rounds to zero at `decimal_places` (e.g. .0032 at 2 decimal places) is
#' written in scientific notation with up to 2 significant figures, so it does
#' not read as zero. An exact zero is written with `decimal_places` zeros. A
#' magnitude at or above `hi` is written in scientific notation with
#' `decimal_places` significant figures.
#' This function can be used to format numbers in plots, on the console, in logs, etc.
#'
#' @param x Vector of numbers
#' @param decimal_places Integer: Return this many decimal places.
#' @param hi Float: Threshold at or above which scientific notation is used.
#' @param as_numeric Logical: If TRUE, convert to numeric before returning,
#' for output used as numbers rather than printed. Numbers then print without
#' trailing zeros: 1.2035 becomes "1.20" with `as_numeric = FALSE`, and 1.2
#' with `as_numeric = TRUE`.
#' @return Formatted number
#'
#' @author EDG
#' @export
#'
#' @examples
#' x <- .34876549
#' ddSci(x)
#' # "0.35"
#' x <- .00000000457823
#' ddSci(x)
#' # "4.6e-09"
ddSci <- function(x, decimal_places = 2, hi = 1e06, as_numeric = FALSE) {
  if (is.null(x)) {
    if (as_numeric) {
      return(NULL)
    } else {
      return("NULL")
    }
  }
  # Do not format factors, characters, or integers.
  if (is.factor(x) || is.character(x)) {
    return(as.character(x))
  }
  if (is.integer(x)) {
    return(x)
  }

  x <- as.list(unlist(x))

  x <- lapply(x, as.numeric)

  xf <- list()

  for (i in seq(x)) {
    if (is.na(x[[i]])) {
      xf[[i]] <- NA
    } else {
      if (x[[i]] == 0) {
        # always give requested decimal places
        xf[[i]] <- format(0, nsmall = decimal_places)
      } else {
        if (abs(x[[i]]) >= hi) {
          xf[[i]] <- format(
            round(x[[i]], decimal_places),
            scientific = TRUE,
            digits = decimal_places,
            nsmall = decimal_places
          )
        } else {
          xf[[i]] <- if (round(x[[i]], decimal_places) != 0) {
            format(round(x[[i]], decimal_places), nsmall = decimal_places)
          } else {
            format(x[[i]], scientific = TRUE, digits = 2)
          }
        }
      }
    }
  }
  xf <- as.character(xf)
  if (as_numeric) {
    xf <- as.numeric(xf)
  }
  xf
}
