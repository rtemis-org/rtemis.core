# 2026- EDG rtemis.org

# Logging and dual-channel error signaling for the rtemis ecosystem.
#
# Built on top of `msg()` and `fmt()` - zero external dependencies (no cli,
# no rlang, no logger). Two design goals:
#
#   1. Operator-side log output goes through `msg()` so it picks up the
#      shared datetime + caller prefix, ANSI styling, and verbosity gate.
#   2. `abort()` is dual-channel: it emits a styled console message AND
#      signals a condition whose `message` field is plain text - safe to
#      serialize into a JSON wire frame, HTML, or anywhere ANSI escape
#      codes would be rendered literally.

# Color presets (col_info, col_warn, col_error, col_success, col_debug) live
# in `rtemis_color_system.R` alongside the other internal palette entries.

# %% Glyphs ----------------------------------------------------------------------------------------
#
# Unicode prefix glyphs for log-level lines. Escaped (`\uXXXX`) so the
# source file stays pure ASCII and parses identically on every platform
# and locale - never paste literal multi-byte characters into R sources.
#
# `glyph_warn` is plain `!` rather than `\u26A0` because `\u26A0` is rendered
# double-width on most emoji-aware terminals, which breaks alignment.

glyph_info <- "\u2139" # info ('i' in a circle)
glyph_success <- "\u2714" # heavy check mark
glyph_warn <- "!" # plain ASCII bang (single-cell, unambiguous)
glyph_error <- "\u2716" # heavy multiplication x
glyph_debug <- "\u203A" # single right-pointing angle quote

# %% Verbosity -------------------------------------------------------------------------------------

#' Resolve the current logging verbosity
#'
#' Reads `getOption("<package>.verbosity")` first when `package` is supplied,
#' falling back to `getOption("rtemis.verbosity")`, and finally to `1L`.
#' Levels: `0L` silent, `1L` info/warn/success/abort console echo, `2L`
#' includes debug.
#'
#' @param package Character or NULL: Optional package-specific override.
#'
#' @return Integer scalar verbosity level.
#'
#' @author EDG
#' @export
#'
#' @examples
#' get_verbosity()
get_verbosity <- function(package = NULL) {
  if (!is.null(package)) {
    pkg_v <- getOption(paste0(package, ".verbosity"))
    if (!is.null(pkg_v)) {
      return(as.integer(pkg_v))
    }
  }
  as.integer(getOption("rtemis.verbosity", 1L))
}


# %% Internal helpers ------------------------------------------------------------------------------

# Compose message parts the same way as paste0(), flattening vector inputs
# element-wise to match paste() semantics. Applies `strip_ansi()` to the
# result so wire-bound text (e.g. `abort()`'s condition message) is
# guaranteed plain regardless of caller input. This costs one cheap gsub
# and protects against the common case of wrapping a foreign condition
# whose `conditionMessage()` contains ANSI escapes from cli / rlang /
# etc. Operator-side styling is applied separately by the log wrappers.
.compose_plain <- function(parts) {
  if (length(parts) == 0L) {
    return("")
  }
  flat <- unlist(lapply(parts, as.character), use.names = FALSE)
  strip_ansi(paste(flat, collapse = ""))
}

#' Strip ANSI escape sequences from a string
#'
#' Removes the SGR / CSI escapes commonly produced by `fmt()` (and by any
#' other tool that writes coloured terminal output). Safe to call on plain
#' input - returns it unchanged.
#'
#' @param x Character: Input.
#'
#' @return Character: `x` with ANSI escapes removed.
#'
#' @author EDG
#' @export
#'
#' @examples
#' strip_ansi(fmt("hi", col = "red"))
strip_ansi <- function(x) {
  gsub("\033\\[[0-9;]*m", "", x, perl = TRUE)
}


# %% info() ----------------------------------------------------------------------------------------

#' Informational log message
#'
#' Styled informational message, routed through `msg()` so it carries the
#' shared datetime + caller prefix. Fires when verbosity is at least `1L`.
#'
#' @param ... Message components, concatenated with no separator.
#' @param verbosity Integer or NULL: Overrides `get_verbosity()` when supplied.
#' @param package Character or NULL: Package name for verbosity override
#'   lookup (e.g. `"rtemis.server"`).
#'
#' @return Invisible NULL.
#'
#' @author EDG
#' @export
#'
#' @examples
#' info("Server started on port ", 8080L)
info <- function(..., verbosity = NULL, package = NULL) {
  v <- verbosity %||% get_verbosity(package)
  if (v < 1L) {
    return(invisible(NULL))
  }
  # `caller_id = 2L` skips info()'s own frame so the bracket shows the
  # function that called info(), not info itself - the glyph already
  # identifies this as an info line.
  msg(
    glyph_info,
    " ",
    ...,
    sep = "",
    format_fn = function(x) fmt(x, col = col_info),
    caller_id = 2L,
    verbosity = 1L
  )
  invisible(NULL)
}


# %% warn() ----------------------------------------------------------------------------------------

#' Warning log message
#'
#' Styled non-fatal message. By default emits a soft styled message via
#' `msg()`; with `use_warning = TRUE`, calls `warning()` so callers can
#' catch via `tryCatch(..., warning = handler)`.
#'
#' @param ... Message components, concatenated with no separator.
#' @param use_warning Logical: If TRUE, signal an R `warning` condition
#'   instead of (or in addition to) writing a styled message.
#' @param verbosity Integer or NULL: Overrides `get_verbosity()` when supplied.
#' @param package Character or NULL: Package name for verbosity override.
#'
#' @return Invisible NULL.
#'
#' @author EDG
#' @export
#'
#' @examples
#' warn("Disk usage at ", 92L, "%")
warn <- function(..., use_warning = FALSE, verbosity = NULL, package = NULL) {
  v <- verbosity %||% get_verbosity(package)
  if (use_warning) {
    # Emit a real warning() so handlers can catch it. Use the plain text
    # so warnings logged elsewhere don't contain ANSI escapes.
    warning(.compose_plain(list(...)), call. = FALSE)
  } else if (v >= 1L) {
    msg(
      glyph_warn,
      " ",
      ...,
      sep = "",
      format_fn = function(x) fmt(x, col = col_warn, bold = TRUE),
      caller_id = 2L,
      verbosity = 1L
    )
  }
  invisible(NULL)
}


# %% success() -------------------------------------------------------------------------------------

#' Success log message
#'
#' Styled success message. Fires when verbosity is at least `1L`.
#'
#' @param ... Message components, concatenated with no separator.
#' @param verbosity Integer or NULL: Overrides `get_verbosity()` when supplied.
#' @param package Character or NULL: Package name for verbosity override.
#'
#' @return Invisible NULL.
#'
#' @author EDG
#' @export
#'
#' @examples
#' success("Job ", "abc123", " complete")
success <- function(..., verbosity = NULL, package = NULL) {
  v <- verbosity %||% get_verbosity(package)
  if (v < 1L) {
    return(invisible(NULL))
  }
  msg(
    glyph_success,
    " ",
    ...,
    sep = "",
    format_fn = function(x) fmt(x, col = col_success, bold = TRUE),
    caller_id = 2L,
    verbosity = 1L
  )
  invisible(NULL)
}


# %% dbg() -----------------------------------------------------------------------------------------

#' Debug log message
#'
#' Muted log message gated at verbosity `>= 2L`. Use for development /
#' troubleshooting output that should not appear in normal operation.
#'
#' Named `dbg()` rather than `debug()` to avoid shadowing
#' [base::debug()] - the R debugger entry point.
#'
#' @param ... Message components, concatenated with no separator.
#' @param verbosity Integer or NULL: Overrides `get_verbosity()` when supplied.
#' @param package Character or NULL: Package name for verbosity override.
#'
#' @return Invisible NULL.
#'
#' @author EDG
#' @export
#'
#' @examples
#' dbg("payload bytes: ", 1234L)
dbg <- function(..., verbosity = NULL, package = NULL) {
  v <- verbosity %||% get_verbosity(package)
  if (v < 2L) {
    return(invisible(NULL))
  }
  msg(
    glyph_debug,
    " ",
    ...,
    sep = "",
    format_fn = function(x) fmt(x, col = col_debug, muted = TRUE),
    caller_id = 2L,
    verbosity = 1L
  )
  invisible(NULL)
}


# %% abort() ---------------------------------------------------------------------------------------

#' Dual-channel error signal
#'
#' Signals a condition AND optionally writes a styled error message to the
#' operator console. The condition's `message` field is **plain text** with
#' all ANSI escapes stripped, so it is safe to serialize into JSON, HTML, or
#' any other ANSI-unaware sink (e.g. browser-side error display).
#'
#' Use `class` to add wire-protocol-specific condition classes that callers
#' can catch via `tryCatch()`. The base classes `"rtemis_error"`, `"error"`,
#' and `"condition"` are always added.
#'
#' @param ... Message components, concatenated with no separator.
#' @param class Character vector: Additional condition classes (prepended
#'   to the base `c("rtemis_error", "error", "condition")`).
#' @param parent Condition or NULL: Wrapped parent condition. Its message is
#'   echoed to the console (when verbosity allows) and stored on the
#'   signalled condition as `$parent`.
#' @param verbosity Integer or NULL: Overrides `get_verbosity()` for the
#'   console echo. Set to `0L` to suppress the echo without affecting the
#'   signalled condition.
#' @param package Character or NULL: Package name for verbosity override.
#'
#' @return Does not return - always signals a condition via `stop()`.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' abort("Could not parse ", "hyperparameters", ".",
#'       class = "rtemislive_invalid_params")
#' }
abort <- function(
  ...,
  class = NULL,
  parent = NULL,
  verbosity = NULL,
  package = NULL
) {
  plain_text <- .compose_plain(list(...))
  v <- verbosity %||% get_verbosity(package)
  if (v >= 1L) {
    # Glyph is only on the operator console echo - the wire-bound
    # `plain_text` (the condition's $message) stays bare so the browser
    # error display doesn't carry a stray prefix character.
    msg(
      glyph_error,
      " ",
      plain_text,
      sep = "",
      format_fn = function(x) fmt(x, col = col_error, bold = TRUE),
      caller_id = 2L,
      verbosity = 1L
    )
    if (!is.null(parent)) {
      parent_msg <- tryCatch(
        conditionMessage(parent),
        error = function(e) as.character(parent)
      )
      msg(
        "  caused by: ",
        strip_ansi(parent_msg),
        sep = "",
        format_fn = function(x) fmt(x, col = col_error, muted = TRUE),
        caller = NA_character_,
        verbosity = 1L
      )
    }
  }
  cond <- structure(
    class = c(class, "rtemis_error", "error", "condition"),
    list(message = plain_text, parent = parent, call = NULL)
  )
  stop(cond)
}


# %% Null-coalescing op (local to avoid depending on rlang) ----------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a
