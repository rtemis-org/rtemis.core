# test-prop_factory_compat.R

library(S7)

# The forms released versions call, each against its canonical form (rule:
# "Names kept for released dependents").

.same_spec <- function(released, canonical) {
  expect_identical(prop_spec(released), prop_spec(canonical))
}

.same_schema <- function(released, canonical) {
  emit <- function(p) {
    Cls <- new_class("CP_Emit", properties = list(x = p))
    class_to_JSONSchema(Cls, id = "https://example.org/cp.json")
  }
  expect_identical(emit(released), emit(canonical))
}


# %% Released formals ----
test_that("the compatibility factories keep their released formals", {
  expect_identical(
    names(formals(prop_float)),
    c(
      "default",
      "min",
      "max",
      "exclusive_min",
      "exclusive_max",
      "nullable",
      "vector",
      "min_items",
      "unique_items",
      "description"
    )
  )
  expect_identical(
    names(formals(prop_bag)),
    c("default", "nullable", "description")
  )
  expect_true(formals(prop_bag)[["nullable"]])
  expect_identical(names(formals(prop_const)), c("value", "description"))
})


# %% Declarations ----
test_that("prop_float builds the declaration of prop_number", {
  .same_spec(
    prop_float(0.5, min = 0, max = 1, description = "p"),
    prop_number(0.5, min = 0, max = 1, description = "p")
  )
  .same_spec(
    prop_float(
      quote(c(0, 1)),
      vector = TRUE,
      min_items = 2L,
      nullable = TRUE
    ),
    prop_array(
      prop_number(),
      default = quote(c(0, 1)),
      min_items = 2L,
      nullable = TRUE
    )
  )
})

test_that("vector, map, and allow_empty build the canonical declarations", {
  .same_spec(
    prop_string(nullable = TRUE, vector = TRUE, unique_items = TRUE),
    prop_array(prop_string(), unique_items = TRUE, nullable = TRUE)
  )
  .same_spec(
    prop_integer(quote(1:2), vector = TRUE, min_items = 2L),
    prop_array(prop_integer(), default = quote(1:2), min_items = 2L)
  )
  .same_spec(
    prop_string(enum = c("a", "b"), map = TRUE, nullable = TRUE),
    prop_map(prop_string(enum = c("a", "b")), nullable = TRUE)
  )
  .same_spec(
    prop_string(allow_empty = TRUE, description = "Message content"),
    prop_string(min_length = 0L, description = "Message content")
  )
  .same_schema(
    prop_string(nullable = TRUE, vector = TRUE),
    prop_array(prop_string(), nullable = TRUE)
  )
})

test_that("prop_bag builds the declaration of prop_map(prop_any())", {
  .same_spec(prop_bag(), prop_map(prop_any(), nullable = TRUE))
  .same_spec(
    prop_bag(quote(list(a = 1)), nullable = FALSE, description = "d"),
    prop_map(prop_any(), default = quote(list(a = 1)), description = "d")
  )
  .same_schema(prop_bag(), prop_map(prop_any(), nullable = TRUE))
})

test_that("prop_const builds the declaration of a const", {
  .same_spec(prop_const("x"), prop_string(const = "x", min_length = 0L))
  .same_spec(prop_const(""), prop_string(const = "", min_length = 0L))
  .same_spec(prop_const(2L), prop_integer(const = 2L))
  .same_spec(prop_const(0.5), prop_number(const = 0.5))
  .same_spec(prop_const(TRUE), prop_boolean(const = TRUE))
  json <- to_JSON(prop_spec(prop_const("x"))[c("type", "const")])
  expect_identical(json, '{"type":"string","const":"x"}')
})

test_that("a quoted default resolves in the caller's environment", {
  k <- c(0, 1)
  p <- prop_float(quote(identity(k)), vector = TRUE, min_items = 2L)
  expect_identical(prop_spec(p)[["default"]], c(0, 1))
  b <- prop_bag(quote(list(z = k)))
  expect_identical(prop_spec(b)[["default"]], list(z = c(0, 1)))
  s <- prop_string(quote(c("a", "b")), vector = TRUE)
  expect_identical(prop_spec(s)[["default"]], c("a", "b"))
})


# %% Released arguments ----
test_that("`...` accepts only the released arguments of each factory", {
  expect_error(prop_integer(map = TRUE), "no argument `map`")
  expect_error(prop_number(allow_empty = TRUE), "no argument `allow_empty`")
  expect_error(prop_string(max_items = 2L), "no argument `max_items`")
  expect_error(prop_string(map = TRUE, unique_items = TRUE), "maps")
  expect_error(prop_string(min_items = 2L), "apply with `vector` or `map`")
  expect_error(
    prop_string(allow_empty = TRUE, min_length = 2L),
    "mutually exclusive"
  )
})


# %% Non-declaration forms ----
test_that("optional() is nullable()", {
  expect_identical(optional(S7::class_character), nullable(S7::class_character))
})

test_that("enum() and optional_logical_scalar admit the canonical values", {
  Cls <- new_class(
    "CP_enum",
    properties = list(
      e = enum(c("a", "b"), default = "a"),
      l = optional_logical_scalar
    )
  )
  expect_identical(Cls()@e, "a")
  expect_null(Cls()@l)
  expect_identical(Cls(e = "b", l = TRUE)@l, TRUE)
  expect_error(Cls(e = "c"))
  expect_error(Cls(l = NA))
  expect_null(prop_spec(enum(c("a", "b"))))
  expect_null(prop_spec(optional_logical_scalar))
})

test_that("each check_optional_*() is its check_nullable_*()", {
  exports <- getNamespaceExports("rtemis.core")
  released <- setdiff(
    grep("^check_optional_", exports, value = TRUE),
    "check_optional_scalar_character"
  )
  expect_length(released, 14L)
  for (nm in released) {
    canonical <- sub("^check_optional_", "check_nullable_", nm)
    expect_identical(
      getExportedValue("rtemis.core", nm),
      getExportedValue("rtemis.core", canonical),
      label = nm
    )
  }
  expect_error(
    check_optional_character_scalar(""),
    class = "rtemis_input_error"
  )
  expect_invisible(check_optional_scalar_character(""))
})
