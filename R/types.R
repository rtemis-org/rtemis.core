# %% optional
#' Create an optional S7 type
#'
#' Creates an S7 union type that allows for the specified type or `NULL`.
#'
#' @param type S7 class
#' @return An S7 union type that allows for the specified type or `NULL`.
#' @author EDG
#' @export
#' @examples
#' # Create an optional character type
#' optional(S7::class_character)
optional <- function(type) {
  check_inherits(type, "S7_base_class", allow_null = FALSE)
  S7::new_union(type, class_missing)
}
