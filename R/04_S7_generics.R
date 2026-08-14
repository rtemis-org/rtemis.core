# 2026- EDG rtemis.org

#' String representation
#'
#' @param x Object to represent as a string.
#' @param ... Additional arguments passed to methods.
#'
#' @return Character string representation of the object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' S7::method(repr, S7::class_character) <- function(x, ...) {
#'   paste0("<chr> \"", x, "\"")
#' }
#' cat(repr("hello"))
repr <- new_generic("repr", "x")
