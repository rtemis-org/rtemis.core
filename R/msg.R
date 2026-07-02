# 2016- EDG rtemis.org

# Internal package state. `msg_sink` holds the optional message sink (NULL =
# console output, the default). When set to a function,
# msg()/msg0()/msgstart()/msgdone() route their structured output through it
# instead of writing to the console. Used by `rtemis.server` to forward
# training messages over a WebSocket. See `set_msg_sink()`.
.rtemis_core_state <- new.env(parent = emptyenv())
.rtemis_core_state[["msg_sink"]] <- NULL
# `line_open` tracks whether the console cursor is parked mid-line by a
# `msgstart()` that has not yet been closed. `msgdone()` closes such a line in
# place (appending the checkmark); any other console writer (`msg()`, `msg0()`)
# breaks to a fresh line first, so an error stamp emitted via `abort()` after a
# `msgstart()` never collides with the pending text. Only the console path
# touches this; the sink path leaves it FALSE.
.rtemis_core_state[["line_open"]] <- FALSE
# Progress state (see `R/progress.R`). `progress_stack` holds the active
# progress handles outermost-first; `progress_visible`/`progress_last_width`
# track whether a `\r`-rewritten status line is currently on screen and how
# wide it was (so it can be cleared by overprinting - `\033[K` is unreliable
# in the RStudio console); `progress_last_draw` is the wall-clock time of the
# last console draw (throttling); `progress_id_counter` feeds auto-generated
# handle ids; `progress_spinner_frame` advances once per actual draw;
# `progress_drawing` marks that the message currently being signalled is our
# own frame/clear write, so the foreign-output handler installed by
# `progress_lapply()` does not react to it.
.rtemis_core_state[["progress_stack"]] <- list()
.rtemis_core_state[["progress_visible"]] <- FALSE
.rtemis_core_state[["progress_last_width"]] <- 0L
.rtemis_core_state[["progress_last_draw"]] <- 0
.rtemis_core_state[["progress_id_counter"]] <- 0L
.rtemis_core_state[["progress_spinner_frame"]] <- 0L
.rtemis_core_state[["progress_drawing"]] <- FALSE


#' Write a progress frame or clear sequence to the console
#'
#' Wraps `message(appendLF = FALSE)` with the `progress_drawing` flag set, so
#' the message condition it signals is recognizable as our own output by the
#' foreign-output handler in `progress_lapply()` (which must not clear the
#' line in response to the very write that draws it).
#'
#' @param text Character: Raw text to write (may contain `\r` and ANSI codes).
#'
#' @return NULL invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.progress_write <- function(text) {
  old <- .rtemis_core_state[["progress_drawing"]]
  .rtemis_core_state[["progress_drawing"]] <- TRUE
  on.exit(.rtemis_core_state[["progress_drawing"]] <- old, add = TRUE)
  message(text, appendLF = FALSE)
  invisible(NULL)
}


#' Clear a visible progress status line before writing
#'
#' If a progress status line is on screen (see `.progress_draw()`), overprint
#' it with spaces and return the cursor to column 1 so the next console write
#' starts on a clean line. The status line reappears on the next
#' `progress_update()`. No-op otherwise.
#'
#' State is reset before writing so a message handler that calls back into
#' this function (see `progress_lapply()`'s foreign-output handler) finds
#' `progress_visible` already FALSE and no-ops instead of recursing.
#'
#' @return NULL invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.clear_progress_line <- function() {
  if (isTRUE(.rtemis_core_state[["progress_visible"]])) {
    width <- .rtemis_core_state[["progress_last_width"]]
    .rtemis_core_state[["progress_visible"]] <- FALSE
    .rtemis_core_state[["progress_last_width"]] <- 0L
    .progress_write(paste0("\r", strrep(" ", width), "\r"))
  }
  invisible(NULL)
}


#' Break a pending `msgstart()` line before writing
#'
#' If a `msgstart()` left the console cursor mid-line, emit a newline and clear
#' the flag so the next message starts clean. No-op otherwise.
#'
#' @return NULL invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.close_open_line <- function() {
  if (isTRUE(.rtemis_core_state[["line_open"]])) {
    message("")
    .rtemis_core_state[["line_open"]] <- FALSE
  }
  invisible(NULL)
}

#' Get current date and time
#'
#' Used by `msgdatetime()` and `log_to_file()`.
#'
#' @param datetime_format Character: Format for the date and time.
#'
#' @return Character: Formatted date and time.
#'
#' @author EDG
#' @keywords internal
#' @export
#'
#' @examples
#' datetime()
datetime <- function(datetime_format = "%Y-%m-%d %H:%M:%S") {
  format(Sys.time(), datetime_format)
}


#' Dispatch to the registered message sink, if any
#'
#' Internal helper used by `msg()`, `msg0()`, `msgstart()`, `msgdone()`.
#' Returns TRUE if a sink consumed the event (caller should skip the console
#' output path), FALSE if no sink is registered (caller should write to console
#' as usual).
#'
#' @param text Character: the formatted message text (no datetime prefix).
#' @param caller Character or NA: calling function name from `format_caller()`.
#' @param ts Character: formatted timestamp from `datetime()`.
#' @param level Character: one of `"info"`, `"start"`, `"done"`, `"progress"`.
#' @param fields List: additional fields appended to the envelope (e.g. the
#'   progress fields `node_id`/`parent_id`/`kind`/`status`/`current`/`total`).
#'
#' @return Logical scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.msg_to_sink <- function(text, caller, ts, level, fields = list()) {
  sink <- .rtemis_core_state[["msg_sink"]]
  if (is.null(sink)) {
    return(FALSE)
  }
  sink(c(list(text = text, caller = caller, ts = ts, level = level), fields))
  TRUE
}

#' Message datetime()
#'
#' @param datetime_format Character: Format for the date and time.
#'
#' @details
#' Used by msg(), msg0(), msgstart()
#'
#' @return Character: Formatted date and time.
#'
#' @author EDG
#' @keywords internal
#' @noRd
msgdatetime <- function(datetime_format = "%Y-%m-%d %H:%M:%S") {
  message(gray(paste0(datetime(), " ")), appendLF = FALSE)
}

suggest <- function(..., output_type = NULL) {
  message <- paste(...)
  output_type <- get_output_type(output_type)
  .clear_progress_line()
  cat(fmt(
    paste0("Suggestion: ", message, "\n"),
    col = col_suggest,
    output_type = output_type
  ))
}

#' Format the calling function name for message provenance
#'
#' @param call_stack List: Call stack from `sys.calls()`.
#' @param call_depth Integer: Depth of the system call path to print.
#' @param caller_id Integer: Which function in the call stack to print.
#' @param max_char Integer: Maximum number of characters for the caller label.
#'
#' @return Character or `NA`: Formatted caller label.
#'
#' @author EDG
#' @keywords internal
#' @noRd
format_caller <- function(call_stack, call_depth, caller_id, max_char = 30L) {
  stack.length <- length(call_stack)
  if (stack.length < 2) {
    caller <- NA
  } else {
    call_depth <- call_depth + caller_id
    if (call_depth > stack.length) {
      call_depth <- stack.length
    }
    caller <- paste(
      lapply(
        rev(seq(call_depth)[-seq(caller_id)]),
        function(i) rev(call_stack)[[i]][[1]]
      ),
      collapse = ">>"
    )
  }
  # do.call and similar will change the call stack, it will contain the full
  # function definition instead of the name alone
  # Capture S7 method calls
  if (!is.na(caller) && substr(caller, 1, 8) == "`method(") {
    caller <- sub("`method\\(([^,]+),.*\\)`", "\\1", caller)
  }
  if (is.function(caller)) {
    # Try to get function name from call stack context
    caller <- tryCatch(
      {
        # Get the original call stack element as character
        call_str <- deparse(rev(call_stack)[[rev(seq(call_depth)[
          -seq(caller_id)
        ])[1]]])
        # Extract function name from the call
        fn_match <- regexpr("^[a-zA-Z_][a-zA-Z0-9_\\.]*", call_str)
        if (fn_match > 0) {
          regmatches(call_str, fn_match)
        } else {
          "(fn)"
        }
      },
      error = function(e) "(fn)"
    )
  }
  if (is.character(caller) && !is.na(caller)) {
    # A useful caller label is a function reference: bare symbol (`foo`),
    # namespaced (`pkg::foo`, `pkg:::foo`), field/slot access
    # (`obj$method`, `obj@slot`), optionally chained via `>>`. Anything
    # containing whitespace, parens, or braces is an inline expression
    # (anonymous function literal, `do.call`-style invocation, etc.) -
    # not a useful name to print. Drop the bracket entirely in that case.
    if (grepl("[[:space:](){}]", caller)) {
      caller <- NA_character_
    } else if (nchar(caller) > 30L) {
      caller <- paste0(substr(caller, 1L, 27L), "...")
    }
  }
  caller
}


#' Message with provenance
#'
#' Print message to output with a prefix including data and time, and calling function or full
#' call stack
#'
#' If `msg` is called directly from the console, it will print `[interactive>]` in place of
#'   the call stack.
#' `msg0`, similar to `paste0`, is `msg(..., sep = "")`
#'
#'
#' @param ... Message to print
#' @param caller Character: Name of calling function
#' @param call_depth Integer: Print the system call path of this depth.
#' @param caller_id Integer: Which function in the call stack to print
#' @param newline_pre Logical: If TRUE begin with a new line.
#' @param newline Logical: If TRUE end with a new line.
#' @param format_fn Function: Formatting function to use on the message text.
#' @param sep Character: Use to separate objects in `...`
#' @param verbosity Integer: Verbosity level of the message. If 0L, does not print anything and
#' returns NULL, invisibly.
#'
#' @return If verbosity > 0L, returns a list with call, message, and date, invisibly, otherwise
#' returns NULL invisibly.
#'
#' @author EDG
#' @export
#'
#' @examples
#' msg("Hello")
msg <- function(
  ...,
  caller = NULL,
  call_depth = 1L,
  caller_id = 1L,
  newline_pre = FALSE,
  newline = TRUE,
  format_fn = plain,
  sep = " ",
  verbosity = 1L
) {
  if (verbosity < 1L) {
    return(invisible(NULL))
  }
  if (is.null(caller)) {
    call_stack <- as.list(sys.calls())
    caller <- format_caller(call_stack, call_depth, caller_id)
  } # / get caller

  txt <- Filter(Negate(is.null), list(...))
  text <- paste(txt, collapse = sep)

  # Sink path: hand structured event to registered sink, skip console.
  if (.msg_to_sink(text, caller, datetime(), "info")) {
    return(invisible(NULL))
  }

  .clear_progress_line()
  .close_open_line()
  if (newline_pre) {
    message("")
  }
  msgdatetime()
  message(
    format_fn(text),
    appendLF = FALSE
  )
  # `rtemis.show_caller` is a global escape hatch for users who prefer
  # bracket-free output. Default TRUE preserves existing behavior; set
  # `options(rtemis.show_caller = FALSE)` in .Rprofile to disable for
  # all sessions.
  show_caller <- length(caller) > 0L &&
    !is.na(caller) &&
    nzchar(caller) &&
    isTRUE(getOption("rtemis.show_caller", TRUE))
  if (show_caller) {
    message(plain(gray(paste0(" [", caller, "]"))))
  } else if (newline) {
    message("")
  }
} # /rtemis::msg


#' @rdname msg
#'
#' @export
msg0 <- function(
  ...,
  caller = NULL,
  call_depth = 1,
  caller_id = 1,
  newline_pre = FALSE,
  newline = TRUE,
  format_fn = plain,
  sep = "",
  verbosity = 1L
) {
  if (verbosity < 1L) {
    return(invisible(NULL))
  }
  if (is.null(caller)) {
    call_stack <- as.list(sys.calls())
    caller <- format_caller(call_stack, call_depth, caller_id)
  }

  txt <- Filter(Negate(is.null), list(...))
  text <- paste(txt, collapse = sep)

  if (.msg_to_sink(text, caller, datetime(), "info")) {
    return(invisible(NULL))
  }

  .clear_progress_line()
  .close_open_line()
  if (newline_pre) {
    message("")
  }
  msgdatetime()
  message(
    format_fn(text),
    appendLF = FALSE
  )
  show_caller <- length(caller) > 0L &&
    !is.na(caller) &&
    nzchar(caller) &&
    isTRUE(getOption("rtemis.show_caller", TRUE))
  if (show_caller) {
    message(plain(gray(paste0(" [", caller, "]"))))
  } else if (newline) {
    message("")
  }
} #


#' Pad-cat
#'
#' Pad and concatenate two strings, with optional newline.
#'
#' @param left Character: Left string to pad and print.
#' @param right Character: Right string to print after left.
#' @param pad Integer: Total width to pad the left string to.
#' @param newline Logical: If TRUE, print a newline after the right string.
#'
#' @return NULL invisibly
#'
#' @author EDG
#' @keywords internal
#' @noRd
pcat <- function(left, right, pad = 17, newline = TRUE) {
  cat(pad_string(left, target = pad), right)
  if (newline) cat("\n")
}


#' Left-pad a string to a target width
#'
#' @author EDG
#' @keywords internal
#' @export
#' @param x Character: String to pad.
#' @param target Integer: Target total width.
#' @param char Character: Padding character.
#'
#' @return Character: `x` left-padded with `char` to width `target`.
#'
#' @examples
#' pad_string("hi", target = 6L)
pad_string <- function(x, target = 17, char = " ") {
  leftpad <- max(0, target - max(0, nchar(x)))
  paste0(strrep(char, leftpad), x)
}


#' msgstart
#'
#' @inheritParams msg
#'
#' @details
#' Avoid `msgstart()`/`msgdone()` pairs that span [progress_update()] calls:
#' a progress redraw closes the pending line, so the checkmark printed by
#' `msgdone()` lands on a fresh line instead of completing the original one.
#' Prefer `msg()` for messages emitted inside progress loops.
#'
#' @return NULL invisibly
#'
#' @author EDG
#' @export
#'
#' @examples
#' msgstart("Starting process...")
#' msgdone("Process complete.")
msgstart <- function(
  ...,
  newline_pre = FALSE,
  sep = ""
) {
  txt <- Filter(Negate(is.null), list(...))
  text <- paste(txt, collapse = sep)

  if (.msg_to_sink(text, NA_character_, datetime(), "start")) {
    return(invisible(NULL))
  }

  .clear_progress_line()
  if (newline_pre) {
    message()
  }
  msgdatetime()
  message(plain(text), appendLF = FALSE)
  .rtemis_core_state[["line_open"]] <- TRUE
}


#' msgdone
#'
#' @inheritParams msg
#'
#' @return NULL invisibly
#'
#' @author EDG
#' @export
#'
#' @examples
#' msgstart("Starting process...")
#' msgdone("Process complete")
msgdone <- function(caller = NULL, call_depth = 1, caller_id = 1, sep = " ") {
  if (is.null(caller)) {
    call_stack <- as.list(sys.calls())
    caller <- format_caller(call_stack, call_depth, caller_id)
  }

  if (.msg_to_sink("done", caller, datetime(), "done")) {
    return(invisible(NULL))
  }

  .clear_progress_line()
  message(" ", appendLF = FALSE)
  yay(end = "")
  message(gray(paste0("[", caller, "]\n")), appendLF = FALSE)
  .rtemis_core_state[["line_open"]] <- FALSE
}


# %% Message sink API ---------------------------------------------------------

#' Set the rtemis message sink
#'
#' When set, `msg()`, `msg0()`, `msgstart()`, and `msgdone()` forward their
#' structured output through `sink` instead of writing to the R console. Used
#' by `rtemis.server` to capture training-time messages and forward them over a
#' WebSocket connection. Pass `NULL` to restore default console output.
#'
#' The sink function is called once per message with a single argument: a list
#' with fields
#'
#' - `text`: character. The formatted message body (no datetime prefix).
#' - `caller`: character or `NA`. Calling function as identified by
#'   `format_caller()`.
#' - `ts`: character. Formatted timestamp (`"%Y-%m-%d %H:%M:%S"`).
#' - `level`: character. One of `"info"` (`msg`/`msg0`), `"start"`
#'   (`msgstart`), `"done"` (`msgdone`), or `"progress"`
#'   ([progress_begin()] / [progress_update()] / [progress_end()]).
#'
#' Producers may include **additional** fields in the list, which sinks should
#' ignore when not understood. In particular, the progress API in this package
#' and `rtemis`'s training observability (see its `specs/observability.md`)
#' emit execution-graph node events through the sink with these extra fields:
#'
#' - `node_id`: character. Unique id of the execution-graph node.
#' - `parent_id`: character or `NA`. Parent node id (for nesting).
#' - `kind`: character. Node kind (e.g. `"tune"`, `"grid_cell"`, `"train_alg"`).
#' - `status`: character. `"start"`, `"update"`, `"done"`, `"error"`, or
#'   `"aborted"`.
#' - `current`, `total`: integer or `NA`. Progress counters.
#'
#' These are additive; sinks that only read the base fields keep working.
#' Progress events fire regardless of verbosity (verbosity gates only the
#' console renderer), and `"update"` events are throttled via
#' `getOption("rtemis.progress_throttle")` - see [progress_begin()].
#'
#' When a sink is set, the console output path is **skipped** for affected
#' calls. Errors thrown by the sink propagate to the caller of `msg()`.
#'
#' @param sink Function or `NULL`.
#'
#' @return Previous sink (function or `NULL`), invisibly.
#'
#' @author EDG
#' @export
#'
#' @seealso [get_msg_sink()], [with_msg_sink()].
#'
#' @examples
#' captured <- list()
#' set_msg_sink(function(m) captured[[length(captured) + 1L]] <<- m)
#' # msg("hello world")        # would append to `captured`
#' set_msg_sink(NULL)          # restore console output
set_msg_sink <- function(sink) {
  if (!is.null(sink) && !is.function(sink)) {
    abort("`sink` must be a function or NULL.")
  }
  old <- .rtemis_core_state[["msg_sink"]]
  .rtemis_core_state[["msg_sink"]] <- sink
  invisible(old)
}


#' Get the current rtemis message sink
#'
#' @return The currently registered sink function, or `NULL` if none is set.
#'
#' @author EDG
#' @export
#'
#' @seealso [set_msg_sink()], [with_msg_sink()].
get_msg_sink <- function() {
  .rtemis_core_state[["msg_sink"]]
}


#' Run code with a temporary message sink
#'
#' Sets `sink` for the duration of `code`, restoring the previous sink on exit
#' (including on error). Useful in tests and for short-lived capture.
#'
#' @param sink Sink function or `NULL`.
#' @param code Code to run.
#'
#' @return The value returned by `code`.
#'
#' @author EDG
#' @export
#'
#' @seealso [set_msg_sink()], [get_msg_sink()].
#'
#' @examples
#' captured <- list()
#' with_msg_sink(
#'   function(m) captured[[length(captured) + 1L]] <<- m,
#'   {
#'     # any msg() / msg0() / msgstart() / msgdone() calls in here are captured
#'   }
#' )
with_msg_sink <- function(sink, code) {
  old <- set_msg_sink(sink)
  on.exit(set_msg_sink(old), add = TRUE)
  force(code)
}
