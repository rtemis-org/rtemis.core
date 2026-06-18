# 2016- EDG rtemis.org

# Internal package state. `msg_sink` holds the optional message sink (NULL =
# console output, the default). When set to a function,
# msg()/msg0()/msgstart()/msgdone() route their structured output through it
# instead of writing to the console. Used by `rtemis.server` to forward
# training messages over a WebSocket. See `set_msg_sink()`.
.rtemis_core_state <- new.env(parent = emptyenv())
.rtemis_core_state[["msg_sink"]] <- NULL

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
#' @param level Character: one of `"info"`, `"start"`, `"done"`.
#'
#' @return Logical scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.msg_to_sink <- function(text, caller, ts, level) {
  sink <- .rtemis_core_state[["msg_sink"]]
  if (is.null(sink)) {
    return(FALSE)
  }
  sink(list(text = text, caller = caller, ts = ts, level = level))
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
  lpad <- max(0, pad - 1 - max(0, nchar(left)))
  cat(pad_string(left), right)
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

  if (newline_pre) {
    message()
  }
  msgdatetime()
  message(plain(text), appendLF = FALSE)
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

  message(" ", appendLF = FALSE)
  yay(end = "")
  message(gray(paste0("[", caller, "]\n")), appendLF = FALSE)
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
#'   (`msgstart`), or `"done"` (`msgdone`).
#'
#' Producers may include **additional** fields in the list, which sinks should
#' ignore when not understood. In particular, `rtemis`'s training observability
#' (see its `specs/observability.md`) emits execution-graph node events through
#' the sink with these extra fields:
#'
#' - `node_id`: character. Unique id of the execution-graph node.
#' - `parent_id`: character or `NA`. Parent node id (for nesting).
#' - `kind`: character. Node kind (e.g. `"tune"`, `"grid_cell"`, `"train_alg"`).
#' - `status`: character. `"start"`, `"done"`, `"error"`, or `"aborted"`.
#' - `current`, `total`: integer or `NULL`. Progress counters.
#'
#' These are additive; sinks that only read the base fields keep working.
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
