# 2024- EDG rtemis.org

# %% match_arg ----
#' Match Arguments Ignoring Case
#'
#' @param x Character: Argument to match.
#' @param choices Character vector: Choices to match against.
#'
#' @return Character: Matched argument.
#'
#' @author EDG
#' @export
#'
#' @examples
#' match_arg("papaya", c("AppleExtreme", "SuperBanana", "PapayaMaster"))
match_arg <- function(x, choices) {
  out <- match.arg(tolower(x), tolower(choices))
  grep(out, choices, value = TRUE, ignore.case = TRUE)
} # /rtemis.core::match_arg


# %% abbreviate_class ----
#' Abbreviate object class name
#'
#' @param x Object.
#' @param n Integer: Minimum abbreviation length.
#'
#' @return Character: Abbreviated class wrapped in angle brackets.
#'
#' @author EDG
#' @export
#'
#' @examples
#' abbreviate_class(iris)
#' abbreviate_class(iris, n = 3)
abbreviate_class <- function(x, n = 4L) {
  paste0("<", abbreviate(class(x)[1], minlength = n), ">")
} # /rtemis.core::abbreviate_class
