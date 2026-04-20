# 2026- EDG rtemis.org

# %% optional ----
#' Create an optional S7 type
#'
#' Creates an S7 union type that allows for the specified type or `NULL`.
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
    cli::cli_abort(
      "{.var type} must be an S7 base class or S7 class."
    )
  }
  S7::new_union(type, NULL)
}
