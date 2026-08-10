# 2026- EDG rtemis.org

# HTML construction for the rtemis ecosystem.
#
# Elements are character strings, not a node tree: `fmt(output_type = "html")`
# already emits markup as text, and every consumer in the ecosystem wants a
# string to embed or send over the wire. Building strings directly keeps one
# representation instead of two.
#
# Text children are escaped and markup children are not, which is the whole
# reason a class rides along: a bare string is untrusted text, `html_raw()`
# marks a string that is already markup, and `html_tag()` marks what it built.
# Without that distinction, composing an element from an element would either
# double-escape the inner markup or leave user-supplied text unescaped.

# %% html_escape ----
#' Escape text for inclusion in HTML
#'
#' @param x Character: Text to escape. Coerced with `as.character()`.
#' @param attribute Logical: If TRUE, also escape the quote characters, which
#' must be escaped inside an attribute value but not in element text.
#'
#' @return Character: Escaped text, carrying no class.
#'
#' @author EDG
#' @export
#' @examples
#' html_escape("a < b & c")
#' html_escape('say "hi"', attribute = TRUE)
html_escape <- function(x, attribute = FALSE) {
  x <- as.character(x)
  # Ampersand first: escaping it after the others would re-escape the
  # ampersands they introduce.
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  if (attribute) {
    x <- gsub('"', "&quot;", x, fixed = TRUE)
    x <- gsub("'", "&#39;", x, fixed = TRUE)
  }
  x
} # rtemis.core::html_escape


# %% html_raw ----
#' Mark a string as HTML
#'
#' Declares that `x` is already markup, so it is embedded verbatim rather than
#' escaped when it becomes the child of an element. The counterpart to
#' [html_escape()]: use it for markup you assembled yourself, never for text
#' that came from outside.
#'
#' @param x Character: Markup.
#'
#' @return Character of class `rtemis_html`.
#'
#' @author EDG
#' @export
#' @examples
#' html_span(html_raw("<em>emphasis</em>"))
#' # Without the marker the same string is text, and escaped:
#' html_span("<em>emphasis</em>")
html_raw <- function(x) {
  structure(as.character(x), class = c("rtemis_html", "character"))
} # rtemis.core::html_raw


# %% .html_children ----
#' Render element children to escaped or verbatim strings
#'
#' Recurses into lists so that `html_ul(lapply(x, html_li))` works, and drops
#' NULL so an optional child can be written as a conditional returning NULL.
#'
#' Reports whether each rendered child is an element, which `html_tag()` needs
#' for its layout rule and cannot recover afterwards: a list-wrapped child is
#' one entry of `...` but may render to several strings.
#'
#' @param x List: Children.
#'
#' @return List of `text` (Character vector, carrying no class) and
#' `is_element` (Logical vector of the same length).
#'
#' @author EDG
#' @keywords internal
#' @noRd
.html_children <- function(x) {
  text <- character()
  is_element <- logical()
  for (child in x) {
    if (is.null(child)) {
      next
    }
    if (inherits(child, "rtemis_html")) {
      element <- inherits(child, "rtemis_html_element")
      # `unclass()` and not `as.character()`: the value is already character,
      # and the class must come off so it does not ride into the result.
      child <- unclass(child)
      text <- c(text, child)
      is_element <- c(is_element, rep(element, length(child)))
    } else if (is.list(child)) {
      nested <- .html_children(child)
      text <- c(text, nested[["text"]])
      is_element <- c(is_element, nested[["is_element"]])
    } else {
      child <- html_escape(child)
      text <- c(text, child)
      is_element <- c(is_element, rep(FALSE, length(child)))
    }
  }
  list(text = text, is_element = is_element)
} # rtemis.core::.html_children


# %% html_tag ----
#' Build an HTML element
#'
#' Children given as `...` are escaped unless marked with [html_raw()] or
#' produced by another `html_*` constructor. Lists are flattened, so children
#' can be built with `lapply()`, and NULL children are dropped.
#'
#' A tag holding a single text child renders on one line; anything else renders
#' as an indented block, and a child's own line breaks are indented with it, so
#' indentation always tracks nesting depth.
#'
#' @param name Character: Element name, e.g. "div".
#' @param ... Children: Character, numbers, `rtemis_html` objects, or lists of
#' those.
#' @param class Optional Character: `class` attribute.
#' @param style Optional Character: `style` attribute.
#' @param id Optional Character: `id` attribute.
#'
#' @return Character of class `rtemis_html_element` and `rtemis_html`.
#'
#' @author EDG
#' @export
#' @examples
#' html_tag("section", "Body text", class = "intro")
#' html_tag("ul", lapply(c("one", "two"), html_li))
html_tag <- function(name, ..., class = NULL, style = NULL, id = NULL) {
  rendered <- .html_children(list(...))
  children <- rendered[["text"]]
  attribs <- c(id = id, class = class, style = style)
  attribs_text <- if (length(attribs) > 0L) {
    paste0(
      " ",
      paste0(
        names(attribs),
        "=\"",
        html_escape(attribs, attribute = TRUE),
        "\"",
        collapse = " "
      )
    )
  } else {
    ""
  }
  open <- paste0("<", name, attribs_text, ">")
  close <- paste0("</", name, ">")
  # One text child stays inline; an element child always opens a block, so that
  # nesting is visible in the output rather than collapsed onto one line.
  inline <- length(children) == 1L && !rendered[["is_element"]][[1L]]
  out <- if (length(children) == 0L) {
    paste0(open, close)
  } else if (inline) {
    paste0(open, children, close)
  } else {
    paste0(
      open,
      "\n  ",
      paste0(gsub("\n", "\n  ", children, fixed = TRUE), collapse = "\n  "),
      "\n",
      close
    )
  }
  structure(
    out,
    class = c("rtemis_html_element", "rtemis_html", "character")
  )
} # rtemis.core::html_tag


# %% html_div ----
#' Build a `div` element
#'
#' @inheritParams html_tag
#'
#' @return Character of class `rtemis_html_element` and `rtemis_html`.
#'
#' @author EDG
#' @export
#' @examples
#' html_div("Contents", class = "panel")
html_div <- function(..., class = NULL, style = NULL, id = NULL) {
  html_tag("div", ..., class = class, style = style, id = id)
} # rtemis.core::html_div


# %% html_p ----
#' Build a `p` element
#'
#' @inheritParams html_tag
#'
#' @return Character of class `rtemis_html_element` and `rtemis_html`.
#'
#' @author EDG
#' @export
#' @examples
#' html_p("A paragraph.")
html_p <- function(..., class = NULL, style = NULL, id = NULL) {
  html_tag("p", ..., class = class, style = style, id = id)
} # rtemis.core::html_p


# %% html_span ----
#' Build a `span` element
#'
#' @inheritParams html_tag
#'
#' @return Character of class `rtemis_html_element` and `rtemis_html`.
#'
#' @author EDG
#' @export
#' @examples
#' html_span("Inline text", style = "color: #16A0AC;")
html_span <- function(..., class = NULL, style = NULL, id = NULL) {
  html_tag("span", ..., class = class, style = style, id = id)
} # rtemis.core::html_span


# %% html_strong ----
#' Build a `strong` element
#'
#' @inheritParams html_tag
#'
#' @return Character of class `rtemis_html_element` and `rtemis_html`.
#'
#' @author EDG
#' @export
#' @examples
#' html_strong(42L)
html_strong <- function(..., class = NULL, style = NULL, id = NULL) {
  html_tag("strong", ..., class = class, style = style, id = id)
} # rtemis.core::html_strong


# %% html_ul ----
#' Build a `ul` element
#'
#' @inheritParams html_tag
#'
#' @return Character of class `rtemis_html_element` and `rtemis_html`.
#'
#' @author EDG
#' @export
#' @examples
#' html_ul(html_li("one"), html_li("two"))
html_ul <- function(..., class = NULL, style = NULL, id = NULL) {
  html_tag("ul", ..., class = class, style = style, id = id)
} # rtemis.core::html_ul


# %% html_li ----
#' Build a `li` element
#'
#' @inheritParams html_tag
#'
#' @return Character of class `rtemis_html_element` and `rtemis_html`.
#'
#' @author EDG
#' @export
#' @examples
#' html_li("An item")
html_li <- function(..., class = NULL, style = NULL, id = NULL) {
  html_tag("li", ..., class = class, style = style, id = id)
} # rtemis.core::html_li
