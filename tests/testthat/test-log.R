# test-log.R

# Run `body` with `opts` set; restore previous values on exit.
with_opts <- function(opts, body) {
  old <- options(opts)
  on.exit(options(old), add = TRUE)
  force(body)
}

`%||%` <- function(a, b) if (is.null(a)) b else a


# get_verbosity ----
test_that("get_verbosity defaults to 1L", {
  with_opts(list(rtemis.verbosity = NULL), {
    expect_identical(get_verbosity(), 1L)
  })
})

test_that("get_verbosity reads rtemis.verbosity option", {
  with_opts(list(rtemis.verbosity = 2L), {
    expect_identical(get_verbosity(), 2L)
  })
})

test_that("get_verbosity prefers package override when supplied", {
  with_opts(list(rtemis.verbosity = 1L, rtemis.server.verbosity = 0L), {
    expect_identical(get_verbosity("rtemis.server"), 0L)
  })
})

test_that("get_verbosity falls back to global when no package override", {
  with_opts(list(rtemis.verbosity = 2L, rtemis.server.verbosity = NULL), {
    expect_identical(get_verbosity("rtemis.server"), 2L)
  })
})


# strip_ansi ----
test_that("strip_ansi removes SGR escapes", {
  styled <- fmt("hello", col = "red")
  expect_true(grepl("\033\\[", styled))
  expect_identical(strip_ansi(styled), "hello")
})

test_that("strip_ansi leaves plain text unchanged", {
  expect_identical(strip_ansi("plain text"), "plain text")
})


# info / warn / success / debug ----
test_that("info prints when verbosity >= 1", {
  with_opts(list(rtemis.verbosity = 1L), {
    expect_message(info("hello"), "hello")
  })
})

test_that("info is silent at verbosity 0", {
  with_opts(list(rtemis.verbosity = 0L), {
    expect_no_message(info("hello"))
  })
})

test_that("info verbosity argument overrides option", {
  with_opts(list(rtemis.verbosity = 0L), {
    expect_message(info("hello", verbosity = 1L), "hello")
  })
})

test_that("warn prints styled message by default", {
  with_opts(list(rtemis.verbosity = 1L), {
    expect_message(warn("careful"), "careful")
  })
})

test_that("warn(use_warning = TRUE) signals an R warning", {
  expect_warning(warn("real warn", use_warning = TRUE), "real warn")
})

test_that("warn(use_warning = TRUE) yields plain message", {
  w <- tryCatch(
    warn("a ", "b", use_warning = TRUE),
    warning = function(e) conditionMessage(e)
  )
  expect_identical(w, "a b")
  expect_false(grepl("\033\\[", w))
})

test_that("success prints when verbosity >= 1", {
  with_opts(list(rtemis.verbosity = 1L), {
    expect_message(success("done"), "done")
  })
})

test_that("dbg is silent at verbosity 1", {
  with_opts(list(rtemis.verbosity = 1L), {
    expect_no_message(dbg("trace"))
  })
})

test_that("dbg prints at verbosity >= 2", {
  with_opts(list(rtemis.verbosity = 2L), {
    expect_message(dbg("trace"), "trace")
  })
})


# abort ----
test_that("abort signals an error", {
  expect_error(abort("boom", verbosity = 0L))
})

test_that("abort condition carries supplied class plus base classes", {
  cond <- tryCatch(
    abort("boom", class = "my_wire_error", verbosity = 0L),
    condition = function(e) e
  )
  expect_true(inherits(cond, "my_wire_error"))
  expect_true(inherits(cond, "rtemis_error"))
  expect_true(inherits(cond, "error"))
  expect_true(inherits(cond, "condition"))
})

test_that("abort condition class can be caught selectively", {
  caught <- tryCatch(
    abort("only this", class = "my_wire_error", verbosity = 0L),
    my_wire_error = function(e) "caught_specifically",
    error = function(e) "caught_generically"
  )
  expect_identical(caught, "caught_specifically")
})

test_that("abort message is plain (no ANSI)", {
  cond <- tryCatch(
    abort("plain text only", verbosity = 0L),
    condition = function(e) e
  )
  expect_identical(conditionMessage(cond), "plain text only")
  expect_false(grepl("\033\\[", conditionMessage(cond)))
})

test_that("abort wire message does not include the operator-side glyph", {
  # The error glyph is prepended only to the console echo; the condition
  # message (which travels over the wire / into a browser UI) must remain
  # bare so downstream consumers don't need to strip a decorative prefix.
  cond <- tryCatch(
    abort("bare", verbosity = 0L),
    condition = function(e) e
  )
  expect_false(grepl(glyph_error, conditionMessage(cond), fixed = TRUE))
})

test_that("abort message is plain when caller wraps a parent with ANSI", {
  # Parent conditions from foreign packages (cli, rlang, etc.) may contain
  # ANSI in their message. Our wrap message itself is plain — the parent
  # ANSI only matters for the *console echo* (handled in a separate test).
  inner <- structure(
    class = c("error", "condition"),
    list(message = fmt("inner", col = "red"), call = NULL)
  )
  cond <- tryCatch(
    abort("outer", parent = inner, verbosity = 0L),
    condition = function(e) e
  )
  expect_identical(conditionMessage(cond), "outer")
})

test_that("abort propagates parent condition", {
  inner <- structure(
    class = c("inner_err", "error", "condition"),
    list(message = "inner reason", call = NULL)
  )
  cond <- tryCatch(
    abort("outer wrap", parent = inner, verbosity = 0L),
    condition = function(e) e
  )
  expect_identical(cond$parent, inner)
})

test_that("abort writes console output when verbosity >= 1", {
  # The console echo carries the most-specific class name (or "rtemis_error"
  # when class = NULL), NOT the human message - the human message is left
  # to R's default error printer so the two channels don't duplicate.
  with_opts(list(rtemis.verbosity = 1L), {
    expect_message(
      expect_error(abort("loud boom")),
      "rtemis_error"
    )
    expect_message(
      expect_error(abort("typed boom", class = "my_specific_error")),
      "my_specific_error"
    )
  })
})

test_that("abort suppresses console output at verbosity 0", {
  with_opts(list(rtemis.verbosity = 0L), {
    expect_no_message(expect_error(abort("quiet boom")))
  })
})
