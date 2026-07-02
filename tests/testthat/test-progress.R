# test-progress.R

# Internal accessors (tests run against the installed namespace).
.state <- rtemis.core:::.rtemis_core_state
.format_hms <- rtemis.core:::.format_hms
.progress_render <- rtemis.core:::.progress_render
.progress_spinners <- rtemis.core:::.progress_spinners

# Reset shared progress state so tests are order-independent.
reset_progress_state <- function() {
  .state[["progress_stack"]] <- list()
  .state[["progress_visible"]] <- FALSE
  .state[["progress_last_width"]] <- 0L
  .state[["progress_last_draw"]] <- 0
  .state[["progress_spinner_frame"]] <- 0L
}

# Fake renderer input: `.progress_render()` only reads fields via `[[`.
fake_handle <- function(label, current, total, t_start = 0) {
  list(label = label, current = current, total = total, t_start = t_start)
}

glyph_dots1 <- "\u280B" # first braille frame
glyph_dot <- "\u25CF" # static dot
sep <- "\u203A"
ellipsis <- "\u2026"


# .format_hms ----
test_that(".format_hms() formats seconds, minutes, hours", {
  expect_equal(.format_hms(0), "0:00")
  expect_equal(.format_hms(41), "0:41")
  expect_equal(.format_hms(61), "1:01")
  expect_equal(.format_hms(3761), "1:02:41")
  expect_equal(.format_hms(59.6), "1:00")
})

test_that(".format_hms() returns '?' for non-finite input", {
  expect_equal(.format_hms(Inf), "?")
  expect_equal(.format_hms(NA_real_), "?")
  expect_equal(.format_hms(NaN), "?")
})


# Spinner designs ----
test_that(".progress_spinners registry has frames and pulse colors", {
  expect_named(.progress_spinners, c("dots", "dot", "blocks"))
  for (design in .progress_spinners) {
    expect_true(length(design[["frames"]]) >= 1L)
    expect_length(design[["colors"]], 8L)
    # Ping-pong ramp: yellow endpoint first, orange endpoint mid-cycle.
    expect_equal(toupper(design[["colors"]][[1L]]), "#FFD858")
    expect_equal(toupper(design[["colors"]][[5L]]), "#F08904")
  }
  expect_length(.progress_spinners[["dots"]][["frames"]], 10L)
  expect_length(.progress_spinners[["dot"]][["frames"]], 1L)
})


# .progress_render ----
test_that(".progress_render() composes a nested breadcrumb with ETA", {
  stack <- list(
    fake_handle("Outer", 2L, 5L),
    fake_handle("Tuning", 7L, 30L, t_start = 0)
  )
  line <- .progress_render(
    stack,
    frame = 0L,
    width = 100L,
    output_type = "plain",
    now = 10
  )
  # elapsed 10s over 7/30 steps -> ETA 10 / 7 * 23 = 32.86 -> 0:33
  expect_equal(
    line,
    paste0(glyph_dots1, " Outer 2/5 ", sep, " Tuning 7/30 ETA 0:33")
  )
})

test_that(".progress_render() renders indeterminate totals with elapsed time", {
  stack <- list(fake_handle("Scanning", 7L, NA_integer_, t_start = 0))
  line <- .progress_render(
    stack,
    frame = 0L,
    width = 100L,
    output_type = "plain",
    now = 12
  )
  expect_equal(line, paste0(glyph_dots1, " Scanning 7 0:12"))
  # total = 0 renders the same way
  stack0 <- list(fake_handle("Scanning", 0L, 0L, t_start = 0))
  line0 <- .progress_render(
    stack0,
    frame = 0L,
    width = 100L,
    output_type = "plain",
    now = 12
  )
  expect_false(grepl("/", line0, fixed = TRUE))
})

test_that(".progress_render() cycles spinner glyphs and pulse colors", {
  stack <- list(fake_handle("Work", 1L, 5L))
  render_at <- function(frame, output_type = "plain") {
    .progress_render(
      stack,
      frame = frame,
      width = 100L,
      output_type = output_type,
      now = 1
    )
  }
  # Glyphs cycle with period 10 (plain output isolates the glyph).
  expect_false(identical(render_at(0L), render_at(1L)))
  expect_identical(render_at(0L), render_at(10L))
  # Colors cycle with period 8: #FFD858 = rgb(255, 216, 88) at frame 0,
  # #F08904 = rgb(240, 137, 4) at frame 4 (ansi output carries the color).
  expect_match(render_at(0L, "ansi"), "38;2;255;216;88", fixed = TRUE)
  expect_match(render_at(4L, "ansi"), "38;2;240;137;4", fixed = TRUE)
  expect_match(render_at(8L, "ansi"), "38;2;255;216;88", fixed = TRUE)
})

test_that("rtemis.progress_spinner option selects the design", {
  stack <- list(fake_handle("Work", 1L, 5L))
  op <- options(rtemis.progress_spinner = "dot")
  on.exit(options(op), add = TRUE)
  for (frame in 0:3) {
    line <- .progress_render(
      stack,
      frame = frame,
      width = 100L,
      output_type = "plain",
      now = 1
    )
    expect_equal(substr(line, 1L, 1L), glyph_dot)
  }
  options(rtemis.progress_spinner = "no_such_design")
  line <- .progress_render(
    stack,
    frame = 0L,
    width = 100L,
    output_type = "plain",
    now = 1
  )
  expect_equal(substr(line, 1L, 1L), glyph_dots1)
})

test_that(".progress_render() degrades under narrow widths", {
  stack <- list(
    fake_handle("Outer resamples", 2L, 5L),
    fake_handle("Tuning", 7L, 30L, t_start = 0)
  )
  full <- .progress_render(
    stack,
    frame = 0L,
    width = 100L,
    output_type = "plain",
    now = 10
  )
  expect_match(full, "ETA", fixed = TRUE)
  # Tier 1: ETA dropped.
  no_eta <- .progress_render(
    stack,
    frame = 0L,
    width = 40L,
    output_type = "plain",
    now = 10
  )
  expect_false(grepl("ETA", no_eta, fixed = TRUE))
  expect_match(no_eta, "Outer resamples", fixed = TRUE)
  # Tier 2: outer level dropped, ellipsis marker added.
  truncated <- .progress_render(
    stack,
    frame = 0L,
    width = 25L,
    output_type = "plain",
    now = 10
  )
  expect_false(grepl("Outer", truncated, fixed = TRUE))
  expect_match(truncated, ellipsis, fixed = TRUE)
  expect_match(truncated, "Tuning 7/30", fixed = TRUE)
  # Last resort: hard truncation to width.
  tiny <- .progress_render(
    stack,
    frame = 0L,
    width = 10L,
    output_type = "plain",
    now = 10
  )
  expect_lte(nchar(tiny), 10L)
})


# Console output: ansi ----
test_that("ansi mode rewrites one status line and prints a completion line", {
  reset_progress_state()
  op <- options(rtemis.progress_throttle = 0)
  on.exit(options(op), add = TRUE)
  msgs <- capture_messages({
    h <- progress_begin(2L, label = "Work", output_type = "ansi")
    progress_update(h)
    progress_update(h)
    progress_end(h)
  })
  raw <- paste(msgs, collapse = "")
  stripped <- strip_ansi(raw)
  expect_match(raw, "\r", fixed = TRUE)
  expect_match(stripped, "Work 1/2", fixed = TRUE)
  expect_match(stripped, "Work 2/2", fixed = TRUE)
  expect_match(stripped, "done in", fixed = TRUE)
  expect_length(.state[["progress_stack"]], 0L)
  expect_false(.state[["progress_visible"]])
})

test_that("inner progress_end() does not print a completion line in ansi mode", {
  reset_progress_state()
  op <- options(rtemis.progress_throttle = 0)
  on.exit(options(op), add = TRUE)
  msgs <- capture_messages({
    outer <- progress_begin(2L, label = "Outer", output_type = "ansi")
    inner <- progress_begin(2L, label = "Inner", output_type = "ansi")
    progress_update(inner)
    progress_end(inner)
    progress_update(outer)
    progress_update(outer)
    progress_end(outer)
  })
  stripped <- strip_ansi(paste(msgs, collapse = ""))
  # Nested breadcrumb was drawn.
  expect_match(
    stripped,
    paste0("Outer 0/2 ", sep, " Inner 1/2"),
    fixed = TRUE
  )
  # Only the outermost end prints "done in".
  expect_length(gregexpr("done in", stripped, fixed = TRUE)[[1L]], 1L)
  expect_match(stripped, "Outer 2/2 done in", fixed = TRUE)
})


# Console output: plain ----
test_that("plain mode prints begin and completion lines, no line rewriting", {
  reset_progress_state()
  msgs <- capture_messages({
    h <- progress_begin(2L, label = "Work", output_type = "plain")
    progress_update(h)
    progress_update(h)
    progress_end(h)
  })
  raw <- paste(msgs, collapse = "")
  stripped <- strip_ansi(raw)
  expect_false(grepl("\r", raw, fixed = TRUE))
  expect_match(stripped, "Work started (total: 2)", fixed = TRUE)
  expect_match(stripped, "Work 2/2 done in", fixed = TRUE)
})

test_that("plain mode reports error status", {
  reset_progress_state()
  msgs <- capture_messages({
    h <- progress_begin(2L, label = "Work", output_type = "plain")
    progress_update(h)
    progress_end(h, status = "error")
  })
  stripped <- strip_ansi(paste(msgs, collapse = ""))
  expect_match(stripped, "Work 1/2 error after", fixed = TRUE)
})

test_that("verbosity 0 silences console output", {
  reset_progress_state()
  expect_silent({
    h <- progress_begin(
      2L,
      label = "Quiet",
      verbosity = 0L,
      output_type = "ansi"
    )
    progress_update(h, force = TRUE)
    progress_end(h)
  })
})


# Sink envelopes ----
test_that("progress events emit enriched sink envelopes with parent chaining", {
  reset_progress_state()
  op <- options(rtemis.progress_throttle = 0)
  on.exit(options(op), add = TRUE)
  captured <- list()
  with_msg_sink(
    function(m) captured[[length(captured) + 1L]] <<- m,
    expect_silent({
      outer <- progress_begin(
        2L,
        label = "Outer",
        kind = "resample",
        output_type = "plain"
      )
      inner <- progress_begin(
        3L,
        label = "Inner",
        id = "node42",
        output_type = "plain"
      )
      progress_update(inner)
      progress_end(inner)
      progress_update(outer)
      progress_end(outer)
    })
  )
  expect_length(captured, 6L)
  statuses <- vapply(captured, function(m) m[["status"]], character(1L))
  expect_equal(
    statuses,
    c("start", "start", "update", "done", "update", "done")
  )
  levels <- vapply(captured, function(m) m[["level"]], character(1L))
  expect_true(all(levels == "progress"))
  # id override and parent chaining
  expect_equal(captured[[2L]][["node_id"]], "node42")
  expect_equal(captured[[2L]][["parent_id"]], captured[[1L]][["node_id"]])
  expect_true(is.na(captured[[1L]][["parent_id"]]))
  # counters and kind
  expect_equal(captured[[1L]][["kind"]], "resample")
  expect_equal(captured[[3L]][["current"]], 1L)
  expect_equal(captured[[3L]][["total"]], 3L)
  expect_equal(captured[[3L]][["text"]], "Inner 1/3")
})

test_that("sink events fire regardless of verbosity", {
  reset_progress_state()
  op <- options(rtemis.progress_throttle = 0)
  on.exit(options(op), add = TRUE)
  captured <- list()
  with_msg_sink(
    function(m) captured[[length(captured) + 1L]] <<- m,
    {
      h <- progress_begin(
        2L,
        label = "Quiet",
        verbosity = 0L,
        output_type = "plain"
      )
      progress_update(h)
      progress_end(h)
    }
  )
  expect_length(captured, 3L)
})

test_that("sink update events are throttled; begin/end/final always fire", {
  reset_progress_state()
  op <- options(rtemis.progress_throttle = 60)
  on.exit(options(op), add = TRUE)
  captured <- list()
  with_msg_sink(
    function(m) captured[[length(captured) + 1L]] <<- m,
    {
      h <- progress_begin(10L, label = "Fast", output_type = "plain")
      for (i in 1:5) {
        progress_update(h)
      }
      progress_update(h, force = TRUE)
      progress_update(h, current = 10L)
      progress_end(h)
    }
  )
  statuses <- vapply(captured, function(m) m[["status"]], character(1L))
  # 5 throttled updates dropped; forced update, final-total update, and
  # begin/end all fire.
  expect_equal(statuses, c("start", "update", "update", "done"))
  expect_equal(captured[[3L]][["current"]], 10L)
})


# Handle behavior and edge cases ----
test_that("progress_begin() validates inputs", {
  expect_error(progress_begin(-1L), class = "rtemis_range_error")
  expect_error(progress_begin(c(1L, 2L)), class = "rtemis_length_error")
  expect_error(
    progress_begin(2L, label = 1L),
    class = "rtemis_type_error"
  )
  expect_error(
    progress_begin(2L, id = 1L),
    class = "rtemis_type_error"
  )
  reset_progress_state()
})

test_that("update clamps to [0, total] and no-ops on closed handles", {
  reset_progress_state()
  h <- progress_begin(
    3L,
    label = "Clamp",
    verbosity = 0L,
    output_type = "plain"
  )
  progress_update(h, current = 99L)
  expect_equal(h[["current"]], 3L)
  progress_update(h, current = -5L)
  expect_equal(h[["current"]], 0L)
  progress_end(h)
  expect_true(h[["closed"]])
  # No-ops after close:
  expect_silent(progress_update(h))
  expect_equal(h[["current"]], 0L)
  expect_null(progress_end(h))
})

test_that("update rejects a foreign handle", {
  expect_error(progress_update(list()), class = "rtemis_type_error")
  expect_error(progress_end(list()), class = "rtemis_type_error")
})

test_that("ending a non-top handle auto-closes nested handles as aborted", {
  reset_progress_state()
  op <- options(rtemis.progress_throttle = 0)
  on.exit(options(op), add = TRUE)
  captured <- list()
  with_msg_sink(
    function(m) captured[[length(captured) + 1L]] <<- m,
    {
      outer <- progress_begin(2L, label = "Outer", output_type = "plain")
      inner <- progress_begin(2L, label = "Inner", output_type = "plain")
      progress_end(outer)
      expect_true(inner[["closed"]])
    }
  )
  statuses <- vapply(captured, function(m) m[["status"]], character(1L))
  labels <- vapply(
    captured,
    function(m) sub(" .*", "", m[["text"]]),
    character(1L)
  )
  # Inner aborts before outer completes.
  expect_equal(statuses, c("start", "start", "aborted", "done"))
  expect_equal(labels, c("Outer", "Inner", "Inner", "Outer"))
  expect_length(.state[["progress_stack"]], 0L)
})


# progress_lapply ----
test_that("progress_lapply() returns exactly what lapply() returns", {
  reset_progress_state()
  x <- c(a = 1, b = 4, c = 9)
  expect_identical(
    progress_lapply(x, sqrt, verbosity = 0L, output_type = "plain"),
    lapply(x, sqrt)
  )
})

test_that("progress_lapply() forwards ... to fn", {
  reset_progress_state()
  expect_identical(
    progress_lapply(
      1:3,
      function(x, mult) x * mult,
      mult = 10L,
      verbosity = 0L,
      output_type = "plain"
    ),
    lapply(1:3, function(x, mult) x * mult, mult = 10L)
  )
})

test_that("progress_lapply() handles empty input", {
  reset_progress_state()
  expect_identical(
    progress_lapply(list(), sqrt, verbosity = 0L, output_type = "plain"),
    list()
  )
  expect_length(.state[["progress_stack"]], 0L)
})

test_that("progress_lapply() ends with error status when fn throws", {
  reset_progress_state()
  op <- options(rtemis.progress_throttle = 0)
  on.exit(options(op), add = TRUE)
  captured <- list()
  with_msg_sink(
    function(m) captured[[length(captured) + 1L]] <<- m,
    expect_error(
      progress_lapply(
        1:3,
        function(x) if (x == 2L) stop("boom") else x,
        label = "Boom",
        verbosity = 0L,
        output_type = "plain"
      ),
      "boom"
    )
  )
  statuses <- vapply(captured, function(m) m[["status"]], character(1L))
  expect_equal(statuses[[length(statuses)]], "error")
  expect_length(.state[["progress_stack"]], 0L)
})

test_that("nested progress_lapply() chains parent ids", {
  reset_progress_state()
  op <- options(rtemis.progress_throttle = 0)
  on.exit(options(op), add = TRUE)
  captured <- list()
  with_msg_sink(
    function(m) captured[[length(captured) + 1L]] <<- m,
    progress_lapply(
      1:2,
      function(i) {
        progress_lapply(
          1:2,
          identity,
          label = "Inner",
          output_type = "plain"
        )
      },
      label = "Outer",
      output_type = "plain"
    )
  )
  starts <- Filter(function(m) m[["status"]] == "start", captured)
  expect_length(starts, 3L)
  outer_id <- starts[[1L]][["node_id"]]
  expect_true(is.na(starts[[1L]][["parent_id"]]))
  expect_equal(starts[[2L]][["parent_id"]], outer_id)
  expect_equal(starts[[3L]][["parent_id"]], outer_id)
})


# Interplay with msg() ----
test_that("msg() during an active status line clears it first", {
  reset_progress_state()
  op <- options(rtemis.progress_throttle = 0)
  on.exit(options(op), add = TRUE)
  msgs <- capture_messages({
    h <- progress_begin(3L, label = "Work", output_type = "ansi")
    progress_update(h)
    msg("mid-loop note", caller = "tester")
    progress_end(h)
  })
  # A clear frame (\r + spaces + \r) precedes the msg output.
  expect_true(any(grepl("^\r +\r$", msgs)))
  clear_idx <- which(grepl("^\r +\r$", msgs))[[1L]]
  note_idx <- which(grepl("mid-loop note", msgs, fixed = TRUE))[[1L]]
  expect_lt(clear_idx, note_idx)
  expect_false(.state[["progress_visible"]])
  reset_progress_state()
})
