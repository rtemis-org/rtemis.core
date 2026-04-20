# 2016- EDG rtemis.org

# used by msgdatetime, log_to_file
datetime <- function(datetime_format = "%Y-%m-%d %H:%M:%S") {
  format(Sys.time(), datetime_format)
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
  if (is.character(caller)) {
    if (nchar(caller) > 30) caller <- paste0(substr(caller, 1, 27), "...")
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
  if (newline_pre) {
    message("")
  }
  msgdatetime()
  message(
    format_fn(paste(txt, collapse = sep)),
    appendLF = FALSE
  )
  if (!is.null(caller) && !is.na(caller) && nchar(caller) > 0L) {
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
  if (newline_pre) {
    message("")
  }
  msgdatetime()
  message(
    format_fn(paste(txt, collapse = sep)),
    appendLF = FALSE
  )
  if (!is.null(caller) && !is.na(caller) && nchar(caller) > 0L) {
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
#'
#' @examples
#' {
#'   msg("Hello")
#'   pcat("super", "wow")
#'   pcat(NULL, "oooo")
#' }
pcat <- function(left, right, pad = 17, newline = TRUE) {
  lpad <- max(0, pad - 1 - max(0, nchar(left)))
  cat(pad_string(left), right)
  if (newline) cat("\n")
}


#' @author EDG
#' @keywords internal
#' @noRd
pad_string <- function(x, target = 17, char = " ") {
  lpad <- max(0, target - max(0, nchar(x)))
  paste0(
    paste(rep(char, lpad), collapse = ""),
    x
  )
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
  if (newline_pre) {
    message()
  }
  msgdatetime()
  message(plain(paste(txt, collapse = sep)), appendLF = FALSE)
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
  message(" ", appendLF = FALSE)
  yay(end = "")
  message(gray(paste0("[", caller, "]\n")), appendLF = FALSE)
}
