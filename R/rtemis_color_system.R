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
  light_orange = "#FDB808",
  orange = "#F08904",
  magenta = "#BE2E5F",
  green = "#526551",
  light_blue = "#B3CFE8",
  blue = "#466D96",
  dark_blue = "#375D86",
  red = "#EA384A",
  light_mauve = "#ECBDC3",
  pink = "#F384FF"
)

# Internal colors
col_highlight <- rtemis_colors[["orange"]]
col_suggest <- rtemis_colors[["orange"]]
col_object <- rtemis_colors[["green"]]
