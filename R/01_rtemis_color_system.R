# 2016- EDG rtemis.org

#' rtemis Colors
#'
#' A named vector of colors used in the rtemis ecosystem, provided as hex strings.
#'
#' @return Named character vector of hex color codes.
#'
#' @author EDG
#' @export
#'
#' @examples
#' rtemis_colors[["teal"]]
rtemis_colors <- c(
  teal = "#6CA3A0",
  orange = "#F08904",
  magenta = "#BE2E5F",
  light_blue = "#B3CFE8",
  green = "#0F6A66",
  light_orange = "#FDB808",
  red = "#EA384A",
  blue = "#466D96",
  juniper = "#526551",
  pink = "#F384FF",
  dark_magenta = "#7D0830",
  dark_blue = "#375D86",
  light_mauve = "#B1A7B3",
  purple = "#7364F2",
  terracotta = "#895140"
)

# Internal colors
col_highlight <- rtemis_colors[["teal"]]
col_suggest <- rtemis_colors[["orange"]]
col_object <- rtemis_colors[["teal"]]

# Log-level colors — used by info() / warn() / success() / debug() / abort()
col_info <- rtemis_colors[["blue"]]
col_warn <- rtemis_colors[["light_orange"]]
col_error <- rtemis_colors[["red"]]
col_success <- "#00cc8f"
col_debug <- "#808080"
