# 2026- EDG rtemis.org

# Nested progress for the rtemis ecosystem - replaces cli::cli_progress_along.
#
# Design:
#   * Explicit handle API (`progress_begin()` / `progress_update()` /
#     `progress_end()`) plus a `progress_lapply()` near-drop-in wrapper.
#     Pure R (no ALTREP), so there is no auto-ticking `for` iterator.
#   * Nesting via a stack of active handles in `.rtemis_core_state`;
#     the console shows ONE status line with a breadcrumb of all levels
#     (multi-line ANSI cursor movement is unreliable in RStudio/knitr).
#   * Sink integration: when a message sink is set (see `set_msg_sink()`),
#     every event is forwarded as a structured envelope with
#     `level = "progress"` and node fields, and console rendering is
#     skipped. Sink events fire regardless of verbosity - verbosity gates
#     only the console renderer.

# %% Glyphs and spinner designs --------------------------------------------------------------------
#
# Escaped (`\uXXXX`) so the source stays pure ASCII - see `R/log.R`.

glyph_progress_sep <- "\u203A" # single right-pointing angle quote
glyph_ellipsis <- "\u2026" # horizontal ellipsis

# Shared pulse: a yellow -> orange ping-pong ramp advanced one step per
# actual (post-throttle) draw. Ping-pong (not sawtooth) so it reads as
# pulsing, not blinking. The orange endpoint is rtemis_colors[["orange"]],
# hardcoded because this file is collated before `rtemis_color_system.R`.
.progress_pulse_colors <- local({
  ramp <- colorRampPalette(c("#FFD858", "#F08904"))(5L)
  c(ramp, rev(ramp)[-c(1L, 5L)])
})

# Each design is a list of `frames` (glyphs) and `colors` (hex ramp); the
# frame counter indexes both with independent recycling, so designs with
# co-prime lengths (10 frames x 8 colors) repeat only every 40 draws.
.progress_spinners <- list(
  # braille dots
  dots = list(
    frames = c(
      "\u280B",
      "\u2819",
      "\u2839",
      "\u2838",
      "\u283C",
      "\u2834",
      "\u2826",
      "\u2827",
      "\u2807",
      "\u280F"
    ),
    colors = .progress_pulse_colors
  ),
  # static dot (black circle) - color-only animation, no shape motion
  dot = list(
    frames = "\u25CF",
    colors = .progress_pulse_colors
  ),
  # quadrant blocks
  blocks = list(
    frames = c("\u2596", "\u2598", "\u259D", "\u2597"),
    colors = .progress_pulse_colors
  )
)

# Resolve the active spinner design; unknown names fall back to "dots".
.progress_get_spinner <- function() {
  name <- getOption("rtemis.progress_spinner", "dots")
  .progress_spinners[[name]] %||% .progress_spinners[["dots"]]
}


# Format the step counter for display: `current/total` when determinate,
# bare `current` when indeterminate (total NA or 0).
.progress_counts <- function(current, total) {
  if (!is.na(total) && total > 0L) {
    paste0(current, "/", total)
  } else {
    as.character(current)
  }
}


# %% .format_hms() ----------------------------------------------------------------------------------

#' Format seconds as `M:SS` / `H:MM:SS`
#'
#' @param seconds Numeric: Duration in seconds.
#'
#' @return Character: `"0:41"`, `"1:02:41"`, or `"?"` for non-finite input.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.format_hms <- function(seconds) {
  if (!is.finite(seconds)) {
    return("?")
  }
  s <- as.integer(round(seconds))
  h <- s %/% 3600L
  m <- (s %% 3600L) %/% 60L
  sec <- s %% 60L
  if (h > 0L) {
    sprintf("%d:%02d:%02d", h, m, sec)
  } else {
    sprintf("%d:%02d", m, sec)
  }
}


# %% Sink path --------------------------------------------------------------------------------------

#' Forward a progress event to the message sink
#'
#' Builds the enriched envelope (`level = "progress"` plus node fields) and
#' hands it to `.msg_to_sink()`. Fires regardless of verbosity.
#'
#' @param handle `rtemis_progress` handle.
#' @param status Character: `"start"`, `"update"`, `"done"`, `"error"`, or
#'   `"aborted"`.
#'
#' @return Logical: TRUE if a sink consumed the event, FALSE if no sink is
#'   registered.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.progress_to_sink <- function(handle, status) {
  counts <- .progress_counts(handle[["current"]], handle[["total"]])
  .msg_to_sink(
    text = paste0(handle[["label"]], " ", counts),
    caller = NA_character_,
    ts = datetime(),
    level = "progress",
    fields = list(
      node_id = handle[["id"]],
      parent_id = handle[["parent_id"]],
      kind = handle[["kind"]],
      status = status,
      current = handle[["current"]],
      total = handle[["total"]]
    )
  )
}


# %% Console renderer -------------------------------------------------------------------------------

#' Render the progress status line
#'
#' Pure function (unit-testable): composes the single status line for the
#' given stack of handles - spinner glyph, breadcrumb of `label current/total`
#' per level, ETA for the innermost level. Degrades under `width` in tiers:
#' drop ETA, then truncate the breadcrumb from the left with an ellipsis.
#'
#' @param stack List of `rtemis_progress` handles, outermost first.
#' @param frame Integer: Spinner frame counter (indexes glyphs and colors
#'   with independent recycling).
#' @param width Integer: Maximum visible width of the line.
#' @param output_type Character: `"ansi"`, `"html"`, or `"plain"`.
#' @param now Numeric: Current elapsed wall-clock time, as
#'   `proc.time()[["elapsed"]]`.
#'
#' @return Character: The status line (no `\r`, no trailing padding).
#'
#' @author EDG
#' @keywords internal
#' @noRd
.progress_render <- function(stack, frame, width, output_type, now) {
  spinner <- .progress_get_spinner()
  glyph <- spinner[["frames"]][[(frame %% length(spinner[["frames"]])) + 1L]]
  col <- spinner[["colors"]][[(frame %% length(spinner[["colors"]])) + 1L]]
  dot <- fmt(glyph, col = col, output_type = output_type)
  sep <- gray(
    paste0(" ", glyph_progress_sep, " "),
    output_type = output_type
  )
  levels <- vapply(
    stack,
    function(h) {
      counts <- .progress_counts(h[["current"]], h[["total"]])
      paste0(
        h[["label"]],
        " ",
        fmt(counts, bold = TRUE, output_type = output_type)
      )
    },
    character(1L)
  )
  # Innermost level: ETA when determinate and in flight, elapsed time when
  # indeterminate (total NA or 0).
  inner <- stack[[length(stack)]]
  tail_info <- NULL
  if (is.na(inner[["total"]]) || inner[["total"]] == 0L) {
    tail_info <- gray(
      .format_hms(now - inner[["t_start"]]),
      output_type = output_type
    )
  } else if (inner[["current"]] > 0L && inner[["current"]] < inner[["total"]]) {
    eta <- (now - inner[["t_start"]]) /
      inner[["current"]] *
      (inner[["total"]] - inner[["current"]])
    tail_info <- gray(
      paste0("ETA ", .format_hms(eta)),
      output_type = output_type
    )
  }
  compose <- function(lvls, truncated, with_tail) {
    paste0(
      dot,
      " ",
      if (truncated) {
        gray(paste0(glyph_ellipsis, " "), output_type = output_type)
      },
      paste(lvls, collapse = sep),
      if (with_tail && !is.null(tail_info)) paste0(" ", tail_info)
    )
  }
  visible_width <- function(x) nchar(strip_ansi(x), type = "width")
  line <- compose(levels, truncated = FALSE, with_tail = TRUE)
  if (visible_width(line) <= width) {
    return(line)
  }
  # Tier 1: drop ETA / elapsed.
  line <- compose(levels, truncated = FALSE, with_tail = FALSE)
  if (visible_width(line) <= width) {
    return(line)
  }
  # Tier 2: drop outer levels from the left.
  for (k in seq_len(length(levels) - 1L)) {
    line <- compose(
      levels[(k + 1L):length(levels)],
      truncated = TRUE,
      with_tail = FALSE
    )
    if (visible_width(line) <= width) {
      return(line)
    }
  }
  # Last resort: hard-truncate the plain-text innermost line so no ANSI
  # escape is cut mid-sequence.
  plain_line <- strip_ansi(
    compose(
      levels[[length(levels)]],
      truncated = length(levels) > 1L,
      with_tail = FALSE
    )
  )
  substr(plain_line, 1L, width)
}


#' Draw the progress status line to the console
#'
#' Side-effecting wrapper around `.progress_render()`: closes any pending
#' `msgstart()` line, rewrites the status line in place with `\r`, and pads
#' with trailing spaces to the previous frame's visible width so leftover
#' characters are overprinted (`\033[K` is unreliable in the RStudio
#' console). Advances the spinner frame and updates draw state.
#'
#' @return NULL invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.progress_draw <- function() {
  stack <- .rtemis_core_state[["progress_stack"]]
  if (length(stack) == 0L) {
    return(invisible(NULL))
  }
  .close_open_line()
  frame <- .rtemis_core_state[["progress_spinner_frame"]]
  now <- proc.time()[["elapsed"]]
  # Cap below the console width: a wrapped line breaks `\r` clearing.
  width <- max(20L, as.integer(getOption("width", 80L)) - 1L)
  line <- .progress_render(
    stack,
    frame = frame,
    width = width,
    output_type = stack[[length(stack)]][["output_type"]],
    now = now
  )
  new_width <- nchar(strip_ansi(line), type = "width")
  pad <- max(0L, .rtemis_core_state[["progress_last_width"]] - new_width)
  message(paste0("\r", line, strrep(" ", pad)), appendLF = FALSE)
  .rtemis_core_state[["progress_spinner_frame"]] <- frame + 1L
  .rtemis_core_state[["progress_visible"]] <- TRUE
  .rtemis_core_state[["progress_last_width"]] <- new_width
  .rtemis_core_state[["progress_last_draw"]] <- now
  invisible(NULL)
}


#' Print the permanent completion line for a finished handle
#'
#' @param handle `rtemis_progress` handle (already closed).
#' @param status Character: `"done"`, `"error"`, or `"aborted"`.
#'
#' @return NULL invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.progress_completion_line <- function(handle, status) {
  elapsed <- proc.time()[["elapsed"]] - handle[["t_start"]]
  counts <- .progress_counts(handle[["current"]], handle[["total"]])
  if (status == "done") {
    glyph <- glyph_success
    col <- col_success
    verb <- "done in"
  } else {
    glyph <- glyph_error
    col <- col_error
    verb <- paste(status, "after")
  }
  msg0(
    glyph,
    " ",
    handle[["label"]],
    " ",
    counts,
    " ",
    verb,
    " ",
    .format_hms(elapsed),
    caller = NA_character_,
    format_fn = function(x) {
      fmt(x, col = col, output_type = handle[["output_type"]])
    },
    verbosity = 1L
  )
  invisible(NULL)
}


# %% progress_begin() -------------------------------------------------------------------------------

#' Begin a progress node
#'
#' Creates a progress handle, pushes it on the active-progress stack (nesting
#' under the current innermost handle, if any), emits a `status = "start"`
#' sink event when a message sink is set (see [set_msg_sink()]), and renders
#' the console status line otherwise.
#'
#' @details
#' The console renderer writes a single status line rewritten in place, with
#' a color-pulsing spinner and a breadcrumb of all active levels, e.g.
#' `Outer resamples 2/5 > Tuning 7/30 ETA 0:41`. Non-`"ansi"` output gets one
#' begin line and one completion line instead (no line rewriting). Two
#' options control rendering:
#'
#' - `rtemis.progress_throttle`: Minimum seconds between redraws and between
#'   sink `"update"` events (default `0.1`). Begin/end events always fire;
#'   set to `0` to emit every tick.
#' - `rtemis.progress_spinner`: Spinner design - one of `"dots"` (braille
#'   dots, default), `"dot"` (static dot, color-only animation), or
#'   `"blocks"` (quadrant blocks). All designs pulse yellow to orange.
#'
#' Sink events fire regardless of verbosity; verbosity gates only the
#' console renderer.
#'
#' @param total Integer: Total number of steps, or `NA` for indeterminate
#'   progress. Must be non-negative.
#' @param label Character: Display label for this progress node.
#' @param kind Character: Node kind forwarded in the sink envelope (e.g.
#'   `"tune"`, `"grid_cell"`).
#' @param id Character or NULL: Node id for the sink envelope. Auto-generated
#'   (`"pb1"`, `"pb2"`, ...) when NULL; pass an execution-graph node id to
#'   correlate progress with other node events.
#' @param verbosity Integer or NULL: Overrides `get_verbosity()` when
#'   supplied. Gates only the console renderer, never sink events.
#' @param package Character or NULL: Package name for verbosity override
#'   lookup (e.g. `"rtemis"`).
#' @param output_type Character or NULL: `"ansi"`, `"html"`, or `"plain"`;
#'   resolved via [get_output_type()] when NULL (`"ansi"` in interactive
#'   sessions, `"plain"` otherwise). Only `"ansi"` animates.
#'
#' @return An `rtemis_progress` handle (environment), invisibly. Pass it to
#'   [progress_update()] and [progress_end()].
#'
#' @author EDG
#' @family progress
#' @export
#'
#' @examples
#' h <- progress_begin(3L, label = "Fitting", output_type = "plain")
#' progress_update(h)
#' progress_end(h)
progress_begin <- function(
  total,
  label = "Progress",
  kind = "progress",
  id = NULL,
  verbosity = NULL,
  package = NULL,
  output_type = NULL
) {
  if (length(total) != 1L) {
    abort(
      "`total` must be a single non-negative number or NA, not length ",
      length(total),
      ".",
      class = c("rtemis_length_error", "rtemis_input_error")
    )
  }
  if (!is.na(total) && (!is.numeric(total) || total < 0)) {
    abort(
      "`total` must be a single non-negative number or NA.",
      class = c("rtemis_range_error", "rtemis_input_error")
    )
  }
  total <- as.integer(total)
  if (!is.character(label) || length(label) != 1L) {
    abort(
      "`label` must be a single character string.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (is.null(id)) {
    n <- .rtemis_core_state[["progress_id_counter"]] + 1L
    .rtemis_core_state[["progress_id_counter"]] <- n
    id <- paste0("pb", n)
  } else if (!is.character(id) || length(id) != 1L) {
    abort(
      "`id` must be a single character string or NULL.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  stack <- .rtemis_core_state[["progress_stack"]]
  parent_id <- if (length(stack) > 0L) {
    stack[[length(stack)]][["id"]]
  } else {
    NA_character_
  }
  handle <- new.env(parent = emptyenv())
  handle[["id"]] <- id
  handle[["parent_id"]] <- parent_id
  handle[["label"]] <- label
  handle[["kind"]] <- kind
  handle[["total"]] <- total
  handle[["current"]] <- 0L
  handle[["t_start"]] <- proc.time()[["elapsed"]]
  handle[["t_last_sink"]] <- handle[["t_start"]]
  handle[["closed"]] <- FALSE
  handle[["v"]] <- verbosity %||% get_verbosity(package)
  handle[["output_type"]] <- get_output_type(output_type)
  class(handle) <- "rtemis_progress"
  .rtemis_core_state[["progress_stack"]] <- c(stack, list(handle))

  # Sink path consumes the event and suppresses console rendering; fires
  # regardless of verbosity.
  if (.progress_to_sink(handle, "start")) {
    return(invisible(handle))
  }
  if (handle[["v"]] >= 1L) {
    if (handle[["output_type"]] == "ansi") {
      .progress_draw()
    } else {
      msg0(
        label,
        " started",
        if (!is.na(total)) paste0(" (total: ", total, ")"),
        caller = NA_character_,
        verbosity = 1L
      )
    }
  }
  invisible(handle)
}


# %% progress_update() ------------------------------------------------------------------------------

#' Update a progress node
#'
#' Advances (or sets) the step counter of an active handle. Emits a throttled
#' `status = "update"` sink event when a message sink is set, or redraws the
#' console status line otherwise. No-op on a closed handle.
#'
#' @param handle `rtemis_progress` handle from [progress_begin()].
#' @param current Integer or NULL: Set the counter to this value. When NULL,
#'   the counter advances by `add`. Clamped to `[0, total]` for determinate
#'   handles.
#' @param add Integer: Increment when `current` is NULL.
#' @param label Character or NULL: Update the display label.
#' @param force Logical: If TRUE, bypass the redraw/sink throttle
#'   (`getOption("rtemis.progress_throttle", 0.1)` seconds).
#'
#' @return The handle, invisibly.
#'
#' @author EDG
#' @family progress
#' @export
#'
#' @examples
#' h <- progress_begin(3L, label = "Fitting", output_type = "plain")
#' progress_update(h)
#' progress_update(h, current = 3L)
#' progress_end(h)
progress_update <- function(
  handle,
  current = NULL,
  add = 1L,
  label = NULL,
  force = FALSE
) {
  if (!inherits(handle, "rtemis_progress")) {
    abort(
      "`handle` must be an rtemis_progress handle from progress_begin().",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (isTRUE(handle[["closed"]])) {
    return(invisible(handle))
  }
  if (!is.null(label)) {
    handle[["label"]] <- label
  }
  cur <- if (is.null(current)) {
    handle[["current"]] + as.integer(add)
  } else {
    as.integer(current)
  }
  cur <- max(0L, cur)
  if (!is.na(handle[["total"]])) {
    cur <- min(cur, handle[["total"]])
  }
  handle[["current"]] <- cur
  now <- proc.time()[["elapsed"]]
  throttle <- as.numeric(getOption("rtemis.progress_throttle", 0.1))
  at_end <- !is.na(handle[["total"]]) && cur >= handle[["total"]]
  if (!is.null(.rtemis_core_state[["msg_sink"]])) {
    # Throttle sink "update" events too, so tight loops don't flood the
    # consumer (e.g. a WebSocket); the final total always fires.
    if (force || at_end || now - handle[["t_last_sink"]] >= throttle) {
      .progress_to_sink(handle, "update")
      handle[["t_last_sink"]] <- now
    }
    return(invisible(handle))
  }
  if (handle[["v"]] >= 1L && handle[["output_type"]] == "ansi") {
    if (
      force ||
        at_end ||
        now - .rtemis_core_state[["progress_last_draw"]] >= throttle
    ) {
      .progress_draw()
    }
  }
  invisible(handle)
}


# %% progress_end() ---------------------------------------------------------------------------------

#' End a progress node
#'
#' Closes the handle, pops it off the active-progress stack, and emits a
#' final sink event or console output. Ending a handle that is not the
#' innermost auto-closes everything nested above it as `"aborted"` (their
#' sink events fire), so the stack stays consistent. No-op on an already
#' closed handle.
#'
#' In `"ansi"` mode, only the outermost end prints a permanent completion
#' line (`label n/total done in 0:41`); inner-level ends just redraw the
#' remaining breadcrumb, so tight nested loops don't spam the console.
#' Non-`"ansi"` output prints a completion line per handle.
#'
#' @param handle `rtemis_progress` handle from [progress_begin()].
#' @param status Character: `"done"`, `"error"`, or `"aborted"`.
#'
#' @return NULL invisibly.
#'
#' @author EDG
#' @family progress
#' @export
#'
#' @examples
#' h <- progress_begin(2L, label = "Fitting", output_type = "plain")
#' progress_update(h)
#' progress_end(h)
progress_end <- function(handle, status = c("done", "error", "aborted")) {
  status <- match.arg(status)
  if (!inherits(handle, "rtemis_progress")) {
    abort(
      "`handle` must be an rtemis_progress handle from progress_begin().",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (isTRUE(handle[["closed"]])) {
    return(invisible(NULL))
  }
  stack <- .rtemis_core_state[["progress_stack"]]
  pos <- Position(function(h) identical(h, handle), stack)
  if (!is.na(pos) && pos < length(stack)) {
    for (above in rev(stack[(pos + 1L):length(stack)])) {
      dbg(
        "progress_end: auto-closing nested progress '",
        above[["label"]],
        "' as aborted."
      )
      progress_end(above, status = "aborted")
    }
  }
  handle[["closed"]] <- TRUE
  stack <- .rtemis_core_state[["progress_stack"]]
  pos <- Position(function(h) identical(h, handle), stack)
  if (!is.na(pos)) {
    .rtemis_core_state[["progress_stack"]] <- stack[-pos]
  }
  if (.progress_to_sink(handle, status)) {
    return(invisible(NULL))
  }
  if (handle[["v"]] >= 1L) {
    if (handle[["output_type"]] == "ansi") {
      .clear_progress_line()
      if (length(.rtemis_core_state[["progress_stack"]]) == 0L) {
        .progress_completion_line(handle, status)
      } else {
        .progress_draw()
      }
    } else {
      .progress_completion_line(handle, status)
    }
  }
  invisible(NULL)
}


# %% progress_lapply() ------------------------------------------------------------------------------

#' `lapply` with progress
#'
#' Drop-in replacement for [lapply()] that reports progress after each
#' element: begins a progress node, ticks once per element, and ends the
#' node. Nested calls produce a nested breadcrumb on the console (and
#' `parent_id`-chained sink events). If `fn` throws, the node is ended with
#' `status = "error"` before the condition propagates, so nested wrappers
#' unwind cleanly.
#'
#' @param x Vector or list: Elements to iterate over, as in [lapply()].
#' @param fn Function: Applied to each element of `x`.
#' @param ... Additional arguments passed to `fn`.
#' @param label Character: Display label for the progress node.
#' @param kind Character: Node kind forwarded in the sink envelope.
#' @param verbosity Integer or NULL: Overrides `get_verbosity()` when
#'   supplied. Gates only the console renderer, never sink events.
#' @param package Character or NULL: Package name for verbosity override
#'   lookup.
#' @param output_type Character or NULL: `"ansi"`, `"html"`, or `"plain"`;
#'   resolved via [get_output_type()] when NULL.
#'
#' @return List: Exactly what `lapply(x, fn, ...)` returns.
#'
#' @author EDG
#' @family progress
#' @export
#'
#' @examples
#' sqrts <- progress_lapply(
#'   1:4,
#'   sqrt,
#'   label = "Computing",
#'   output_type = "plain"
#' )
progress_lapply <- function(
  x,
  fn,
  ...,
  label = "Processing",
  kind = "progress",
  verbosity = NULL,
  package = NULL,
  output_type = NULL
) {
  fn <- match.fun(fn)
  handle <- progress_begin(
    length(x),
    label = label,
    kind = kind,
    verbosity = verbosity,
    package = package,
    output_type = output_type
  )
  completed <- FALSE
  on.exit(
    if (!completed) {
      progress_end(handle, status = "error")
    },
    add = TRUE
  )
  out <- lapply(
    x,
    function(el, ...) {
      res <- fn(el, ...)
      progress_update(handle)
      res
    },
    ...
  )
  completed <- TRUE
  progress_end(handle, status = "done")
  out
}
