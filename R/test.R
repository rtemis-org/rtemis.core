# 2024- EDG rtemis.org

# test_* functions perform checks and return TRUE/FALSE; do not throw errors.

# %% test_inherits ----
#' Check class of object
#'
#' @param x Object to check
#' @param cl Character: class to check against
#'
#' @return Logical
#'
#' @author EDG
#' @export
#'
#' @examples
#' test_inherits("papaya", "character") # TRUE
#' test_inherits(c(1, 2.5, 3.2), "integer")
#' test_inherits(iris, "list") # FALSE, compare to is_check(iris, is.list)
test_inherits <- function(x, cl) {
  if (!inherits(x, cl)) {
    input <- deparse(substitute(x))
    cli::cli_alert_danger("{.var {input}} is not {.cls {cl}}")
    return(FALSE)
  }
  TRUE
} # /rtemis.core::test_inherits
