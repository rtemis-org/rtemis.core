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


# %% nullable ----
#' Create a nullable S7 type
#'
#' Creates an S7 union type that admits the specified type or `NULL`, with
#' `NULL` first, so that S7 prototypes a property of this type to `NULL`.
#'
#' Use it when the S7 class already validates the non-NULL case. A value with
#' its own constraints is declared with a `prop_*` factory and
#' `nullable = TRUE`.
#'
#' @param type S7 base class or S7 class.
#' @return S7 union of `NULL` and `type`.
#' @author EDG
#' @export
#' @examples
#' nullable(S7::class_character)
nullable <- function(type) {
  if (!inherits(type, "S7_base_class") && !inherits(type, "S7_class")) {
    abort(
      "`type` must be an S7 base class or S7 class.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  S7::new_union(NULL, type)
} # /rtemis.core::nullable
