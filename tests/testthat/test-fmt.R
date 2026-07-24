# test-fmt.R

.valid_output_type <- rtemis.core:::.valid_output_type

# Run `code` with `vars` set in the environment, restoring the previous values
# (or unsetting again) on exit. `NA` means "unset for the duration": these tests
# must not inherit `RTEMIS_OUTPUT_TYPE` or `NO_COLOR` from the developer's
# shell, and they restore anything they change themselves.
with_envvars <- function(vars, code) {
  old <- Sys.getenv(names(vars), unset = NA_character_, names = TRUE)
  on.exit(
    {
      restore <- old[!is.na(old)]
      if (length(restore) > 0L) {
        do.call(Sys.setenv, as.list(restore))
      }
      drop <- names(old)[is.na(old)]
      if (length(drop) > 0L) {
        Sys.unsetenv(drop)
      }
    },
    add = TRUE
  )
  set <- vars[!is.na(vars)]
  if (length(set) > 0L) {
    do.call(Sys.setenv, as.list(set))
  }
  unset <- names(vars)[is.na(vars)]
  if (length(unset) > 0L) {
    Sys.unsetenv(unset)
  }
  force(code)
}

# Nothing set anywhere: the starting point for most tests below.
clean_env <- c(RTEMIS_OUTPUT_TYPE = NA_character_, NO_COLOR = NA_character_)


# .valid_output_type ----

test_that(".valid_output_type accepts the three known types", {
  expect_equal(.valid_output_type("ansi"), "ansi")
  expect_equal(.valid_output_type("html"), "html")
  expect_equal(.valid_output_type("plain"), "plain")
})

test_that(".valid_output_type returns NULL for anything else", {
  expect_null(.valid_output_type("AnSi"))
  expect_null(.valid_output_type("colour"))
  expect_null(.valid_output_type(""))
  expect_null(.valid_output_type(NULL))
  expect_null(.valid_output_type(NA_character_))
  expect_null(.valid_output_type(c("ansi", "plain")))
  expect_null(.valid_output_type(1L))
})


# get_output_type ----

test_that("explicit output_type and filename win over everything", {
  op <- options(rtemis.output_type = "plain")
  on.exit(options(op), add = TRUE)
  with_envvars(c(RTEMIS_OUTPUT_TYPE = "plain", NO_COLOR = "1"), {
    expect_equal(get_output_type("ansi"), "ansi")
    # `filename` outranks even the argument: file output is never styled.
    expect_equal(get_output_type("ansi", filename = "out.txt"), "plain")
    expect_equal(get_output_type(filename = "out.txt"), "plain")
  })
})

test_that("rtemis.output_type option is honored in non-interactive sessions", {
  skip_if(interactive(), "resolution below the option is session-dependent")
  op <- options(rtemis.output_type = "ansi")
  on.exit(options(op), add = TRUE)
  with_envvars(clean_env, {
    expect_equal(get_output_type(), "ansi")
    options(rtemis.output_type = "html")
    expect_equal(get_output_type(), "html")
  })
})

test_that("rtemis.output_type option forces plain in any session", {
  op <- options(rtemis.output_type = "plain")
  on.exit(options(op), add = TRUE)
  with_envvars(clean_env, {
    expect_equal(get_output_type(), "plain")
  })
})

test_that("RTEMIS_OUTPUT_TYPE is honored when the option is unset", {
  op <- options(rtemis.output_type = NULL)
  on.exit(options(op), add = TRUE)
  with_envvars(c(RTEMIS_OUTPUT_TYPE = "ansi", NO_COLOR = NA_character_), {
    expect_equal(get_output_type(), "ansi")
  })
})

test_that("the option beats the environment variable", {
  op <- options(rtemis.output_type = "plain")
  on.exit(options(op), add = TRUE)
  with_envvars(c(RTEMIS_OUTPUT_TYPE = "ansi", NO_COLOR = NA_character_), {
    expect_equal(get_output_type(), "plain")
  })
})

test_that("NO_COLOR forces plain, but RTEMIS_OUTPUT_TYPE outranks it", {
  op <- options(rtemis.output_type = NULL)
  on.exit(options(op), add = TRUE)
  with_envvars(c(RTEMIS_OUTPUT_TYPE = NA_character_, NO_COLOR = "1"), {
    expect_equal(get_output_type(), "plain")
    # An rtemis-scoped request is more specific than the generic convention.
    Sys.setenv(RTEMIS_OUTPUT_TYPE = "ansi")
    expect_equal(get_output_type(), "ansi")
    # An empty NO_COLOR counts as unset, per no-color.org.
    Sys.unsetenv("RTEMIS_OUTPUT_TYPE")
    Sys.setenv(NO_COLOR = "")
    expect_equal(get_output_type(), if (interactive()) "ansi" else "plain")
  })
})

test_that("unrecognized option and environment values are ignored, not errors", {
  skip_if(interactive(), "the fallback below is session-dependent")
  op <- options(rtemis.output_type = "AnSi")
  on.exit(options(op), add = TRUE)
  with_envvars(c(RTEMIS_OUTPUT_TYPE = "colour", NO_COLOR = NA_character_), {
    expect_equal(get_output_type(), "plain")
  })
})

test_that("an invalid explicit output_type still errors", {
  expect_error(get_output_type("colour"))
})

test_that("the default remains plain when nothing is set", {
  skip_if(interactive(), "interactive sessions default to ansi")
  op <- options(rtemis.output_type = NULL)
  on.exit(options(op), add = TRUE)
  with_envvars(clean_env, {
    expect_equal(get_output_type(), "plain")
  })
})


# fmt() through the resolved type ----

test_that("fmt() emits ANSI codes non-interactively when the option is set", {
  op <- options(rtemis.output_type = "ansi")
  on.exit(options(op), add = TRUE)
  with_envvars(clean_env, {
    expect_true(grepl("\033[", fmt("x", col = "red"), fixed = TRUE))

    options(rtemis.output_type = "plain")
    expect_equal(fmt("x", col = "red"), "x")

    options(rtemis.output_type = "html")
    expect_true(grepl("<span", fmt("x", col = "red"), fixed = TRUE))
  })
})
