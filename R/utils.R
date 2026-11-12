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


# %% optional ----
#' Create an optional S7 type
#'
#' Creates an S7 union type that allows for the specified type or `NULL`.
#'
#' Use it when the S7 class already validates the non-NULL case. For a value
#' with its own constraints, use a `prop_*` factory with `nullable = TRUE`.
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
    abort(
      "`type` must be an S7 base class or S7 class.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  S7::new_union(NULL, type)
}
