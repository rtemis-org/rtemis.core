# test-00_S7_prop_factories.R

library(S7)

# One test class per property, so S7 validation fires on construction.
.PC <- local({
  list(
    bool = new_class("PC_bool", properties = list(x = prop_boolean())),
    int = new_class(
      "PC_int",
      properties = list(x = prop_integer(default = 1L, min = 1L))
    ),
    flt = new_class(
      "PC_flt",
      properties = list(x = prop_float(default = 0.3, min = 0, max = 2))
    ),
    flt_exc = new_class(
      "PC_flt_exc",
      properties = list(
        x = prop_float(default = 0.5, exclusive_min = 0, exclusive_max = 1)
      )
    ),
    flt_null = new_class(
      "PC_flt_null",
      properties = list(x = prop_float(min = 0, max = 2, nullable = TRUE))
    ),
    str = new_class("PC_str", properties = list(x = prop_string())),
    str_null = new_class(
      "PC_str_null",
      properties = list(x = prop_string(nullable = TRUE))
    ),
    str_empty = new_class(
      "PC_str_empty",
      properties = list(x = prop_string(default = "", allow_empty = TRUE))
    ),
    str_enum = new_class(
      "PC_str_enum",
      properties = list(
        x = prop_string(default = "a", enum = c("a", "b"))
      )
    ),
    str_vec = new_class(
      "PC_str_vec",
      properties = list(
        x = prop_string(
          default = c("a", "b"),
          vector = TRUE,
          min_items = 2L,
          unique_items = TRUE
        )
      )
    ),
    str_map = new_class(
      "PC_str_map",
      properties = list(x = prop_string(default = c(a = "1"), map = TRUE))
    ),
    bag = new_class("PC_bag", properties = list(x = prop_bag())),
    const = new_class(
      "PC_const",
      properties = list(x = prop_const("object"))
    )
  )
})


# %% prop_boolean ----
test_that("prop_boolean accepts logical scalars and rejects everything else", {
  expect_false(.PC$bool()@x)
  expect_true(.PC$bool(x = TRUE)@x)
  expect_error(.PC$bool(x = NA))
  expect_error(.PC$bool(x = c(TRUE, FALSE)))
  expect_error(.PC$bool(x = 1))
  expect_error(.PC$bool(x = NULL))
})


# %% prop_integer ----
test_that("prop_integer requires integer type", {
  expect_identical(.PC$int()@x, 1L)
  expect_identical(.PC$int(x = 5L)@x, 5L)
  # A double is not an integer, even when whole.
  expect_error(.PC$int(x = 5))
  expect_error(.PC$int(x = 0L))
  expect_error(.PC$int(x = NA_integer_))
})


# %% prop_float ----
test_that("prop_float enforces inclusive bounds", {
  expect_identical(.PC$flt()@x, 0.3)
  # Zero is inside [0, 2] -- the case a "positive" check would wrongly reject.
  expect_identical(.PC$flt(x = 0)@x, 0)
  expect_identical(.PC$flt(x = 2)@x, 2)
  expect_error(.PC$flt(x = -0.001))
  expect_error(.PC$flt(x = 2.001))
  expect_error(.PC$flt(x = Inf))
  expect_error(.PC$flt(x = NA_real_))
  expect_error(.PC$flt(x = c(0.1, 0.2)))
  expect_error(.PC$flt(x = NULL))
})

test_that("prop_float enforces exclusive bounds", {
  expect_identical(.PC$flt_exc(x = 0.001)@x, 0.001)
  expect_error(.PC$flt_exc(x = 0))
  expect_error(.PC$flt_exc(x = 1))
})

test_that("prop_float accepts integer values", {
  expect_identical(.PC$flt(x = 1L)@x, 1L)
})


# %% nullable ----
test_that("nullable properties prototype to NULL, not an empty vector", {
  expect_null(.PC$flt_null()@x)
  expect_null(.PC$str_null()@x)
  expect_identical(.PC$flt_null(x = 0)@x, 0)
  # An empty vector is a value, not "unset", and is rejected as one.
  expect_error(.PC$flt_null(x = double()))
  expect_error(.PC$str_null(x = character()))
})


# %% prop_string ----
test_that("prop_string rejects empty and whitespace-only strings by default", {
  expect_identical(.PC$str(x = "a")@x, "a")
  expect_error(.PC$str(x = ""))
  expect_error(.PC$str(x = "   "))
  expect_error(.PC$str(x = NA_character_))
  expect_error(.PC$str(x = c("a", "b")))
  # A property with no default is unset, and an unset non-nullable string
  # fails on construction rather than silently holding character(0).
  expect_error(.PC$str())
})

test_that("prop_string allow_empty admits the empty string", {
  expect_identical(.PC$str_empty()@x, "")
  expect_identical(.PC$str_empty(x = "a")@x, "a")
})

test_that("prop_string enum restricts values", {
  expect_identical(.PC$str_enum(x = "b")@x, "b")
  expect_error(.PC$str_enum(x = "c"))
})

test_that("prop_string vector enforces arity and uniqueness", {
  expect_identical(.PC$str_vec(x = c("a", "b", "c"))@x, c("a", "b", "c"))
  expect_error(.PC$str_vec(x = "a"))
  expect_error(.PC$str_vec(x = c("a", "a")))
})

test_that("prop_string map requires names", {
  expect_identical(.PC$str_map(x = c(a = "1", b = "2"))@x, c(a = "1", b = "2"))
  expect_error(.PC$str_map(x = c("1", "2")))
})


# %% prop_bag ----
test_that("prop_bag carries a named list as one value", {
  expect_null(.PC$bag()@x)
  expect_identical(.PC$bag(x = list(a = 1, b = 2))@x, list(a = 1, b = 2))
  expect_error(.PC$bag(x = "a"))
})


# %% prop_const ----
test_that("prop_const fixes a property to one value", {
  expect_identical(.PC$const()@x, "object")
  expect_identical(.PC$const(x = "object")@x, "object")
  expect_error(.PC$const(x = "array"))
})


# %% Declaration-time validation ----
test_that("a default that violates its own spec fails at declaration", {
  expect_error(prop_float(default = 5, min = 0, max = 2))
  expect_error(prop_float(default = 5, exclusive_max = 5))
  expect_error(prop_string(default = "c", enum = c("a", "b")))
  expect_error(prop_string(default = ""))
  expect_error(prop_integer(default = 0L, min = 1L))
})

test_that("invalid factory arguments are rejected", {
  expect_error(prop_string(vector = TRUE, map = TRUE))
  expect_error(prop_float(nullable = NA))
  expect_error(prop_float(description = c("a", "b")))
  expect_error(prop_const(c("a", "b")))
  expect_error(prop_const(NA))
})


# %% prop_spec ----
test_that("prop_spec exposes the declaration", {
  spec <- prop_spec(prop_float(
    default = 0.3,
    min = 0,
    max = 2,
    description = "Sampling temperature"
  ))
  expect_identical(spec[["type"]], "number")
  expect_identical(spec[["default"]], 0.3)
  expect_identical(spec[["minimum"]], 0)
  expect_identical(spec[["maximum"]], 2)
  expect_identical(spec[["description"]], "Sampling temperature")
  expect_false(spec[["nullable"]])
  expect_identical(spec[["container"]], "none")
})

test_that("prop_spec returns NULL for a hand-written property", {
  expect_null(prop_spec(character_scalar))
})
