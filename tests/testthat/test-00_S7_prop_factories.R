# test-00_S7_prop_factories.R

library(S7)

# One test class per property, so S7 validation fires on construction.
.PC <- local({
  list(
    bool = new_class("PC_bool", properties = list(x = prop_boolean(FALSE))),
    bool_req = new_class("PC_bool_req", properties = list(x = prop_boolean())),
    int = new_class(
      "PC_int",
      properties = list(x = prop_integer(1L, min = 1L))
    ),
    int_enum = new_class(
      "PC_int_enum",
      properties = list(x = prop_integer(5L, enum = c(5L, 10L)))
    ),
    num = new_class(
      "PC_num",
      properties = list(x = prop_number(0.3, min = 0, max = 2))
    ),
    num_exc = new_class(
      "PC_num_exc",
      properties = list(
        x = prop_number(0.5, exclusive_min = 0, exclusive_max = 1)
      )
    ),
    num_null = new_class(
      "PC_num_null",
      properties = list(x = prop_number(min = 0, max = 2, nullable = TRUE))
    ),
    str = new_class("PC_str", properties = list(x = prop_string())),
    str_null = new_class(
      "PC_str_null",
      properties = list(x = prop_string(nullable = TRUE))
    ),
    str_empty = new_class(
      "PC_str_empty",
      properties = list(x = prop_string("", min_length = 0L))
    ),
    str_enum = new_class(
      "PC_str_enum",
      properties = list(x = prop_string("a", enum = c("a", "b")))
    ),
    str_len = new_class(
      "PC_str_len",
      properties = list(x = prop_string("ab", max_length = 3L))
    ),
    str_const = new_class(
      "PC_str_const",
      properties = list(x = prop_string(const = "object"))
    ),
    bool_const_null = new_class(
      "PC_bool_const_null",
      properties = list(x = prop_boolean(const = TRUE, nullable = TRUE))
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

test_that("prop_boolean has no default unless given, constant, or nullable", {
  expect_null(prop_spec(prop_boolean())[["default"]])
  expect_error(.PC$bool_req(), "must not be empty")
  expect_true(.PC$bool_req(x = TRUE)@x)
  expect_null(prop_spec(prop_boolean(nullable = TRUE))[["default"]])
  expect_true(prop_spec(prop_boolean(const = TRUE))[["default"]])
})

test_that("a constant is the default of a nullable declaration", {
  expect_true(.PC$bool_const_null()@x)
  expect_null(.PC$bool_const_null(x = NULL)@x)
  expect_error(.PC$bool_const_null(x = FALSE), "must be TRUE")
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

test_that("prop_integer enum restricts values", {
  expect_identical(.PC$int_enum(x = 10L)@x, 10L)
  expect_error(.PC$int_enum(x = 7L), "one of")
  expect_error(prop_integer(enum = c(1, 2)), "integer values")
})


# %% prop_number ----
test_that("prop_number enforces inclusive bounds", {
  expect_identical(.PC$num()@x, 0.3)
  expect_identical(.PC$num(x = 0)@x, 0)
  expect_identical(.PC$num(x = 2)@x, 2)
  expect_error(.PC$num(x = -0.001))
  expect_error(.PC$num(x = 2.001))
  expect_error(.PC$num(x = Inf))
  expect_error(.PC$num(x = NA_real_))
  expect_error(.PC$num(x = c(0.1, 0.2)))
  expect_error(.PC$num(x = NULL))
})

test_that("prop_number enforces exclusive bounds", {
  expect_identical(.PC$num_exc(x = 0.001)@x, 0.001)
  expect_error(.PC$num_exc(x = 0))
  expect_error(.PC$num_exc(x = 1))
})

test_that("prop_number accepts integer values", {
  expect_identical(.PC$num(x = 1L)@x, 1L)
})


# %% Exact R values ----
test_that("scalars carry no attributes, except a class on a string", {
  expect_error(.PC$int(x = factor("a")), "class <factor>")
  expect_error(.PC$num(x = as.Date("2020-01-01")), "class <Date>")
  expect_error(.PC$num(x = c(a = 1)), "attribute `names`")
  expect_error(.PC$num(x = matrix(1)), "attribute `dim`")
  json <- structure("{\"a\":1}", class = "json")
  expect_identical(.PC$str(x = json)@x, json)
})


# %% nullable ----
test_that("nullable properties prototype to NULL, not an empty vector", {
  expect_null(.PC$num_null()@x)
  expect_null(.PC$str_null()@x)
  expect_identical(.PC$num_null(x = 0)@x, 0)
  # An empty vector is a value, not "unset", and is rejected as one.
  expect_error(.PC$num_null(x = double()))
  expect_error(.PC$str_null(x = character()))
})


# %% prop_string ----
test_that("prop_string rejects the empty string and admits blank strings", {
  expect_identical(.PC$str(x = "a")@x, "a")
  expect_identical(.PC$str(x = "   ")@x, "   ")
  expect_error(.PC$str(x = ""), "empty string")
  expect_error(.PC$str(x = NA_character_))
  expect_error(.PC$str(x = c("a", "b")))
  # A property with no default is unset, and an unset non-nullable string
  # fails on construction rather than silently holding character(0).
  expect_error(.PC$str())
})

test_that("min_length 0 admits the empty string", {
  expect_identical(.PC$str_empty()@x, "")
  expect_identical(.PC$str_empty(x = "a")@x, "a")
})

test_that("max_length bounds the character count", {
  expect_identical(.PC$str_len(x = "abc")@x, "abc")
  expect_error(.PC$str_len(x = "abcd"), "at most 3 characters")
  expect_error(prop_string(min_length = 3L, max_length = 2L), "at least")
})

test_that("prop_string enum restricts values", {
  expect_identical(.PC$str_enum(x = "b")@x, "b")
  expect_error(.PC$str_enum(x = "c"))
})

test_that("a pattern excludes blank strings when it says so", {
  Cls <- new_class(
    "PC_str_pattern",
    properties = list(x = prop_string(pattern = "\\S"))
  )
  expect_identical(Cls(x = "a")@x, "a")
  expect_error(Cls(x = "  "), "pattern")
  # (*UCP): a no-break space is a space, as in ECMA-262.
  expect_error(Cls(x = "\u00a0"), "pattern")
})


# %% const ----
test_that("const fixes a property to one value and is its default", {
  expect_identical(.PC$str_const()@x, "object")
  expect_identical(.PC$str_const(x = "object")@x, "object")
  expect_error(.PC$str_const(x = "array"))
})


# %% Independent keywords ----
test_that("enum members and const satisfy the other keywords", {
  expect_error(prop_string(enum = c("", "a")), "`enum` member")
  expect_no_error(prop_string(enum = c("", "a"), min_length = 0L))
  expect_error(prop_string(const = ""), "`const`")
  expect_no_error(prop_string(const = "", min_length = 0L))
  expect_error(prop_integer(enum = c(0L, 5L), min = 1L), "`enum` member")
  expect_error(prop_string(enum = "a", const = "a"), "mutually exclusive")
})


# %% Arguments by name ----
test_that("arguments after `default` are passed by name", {
  expect_error(prop_string("a", "b"), "by name", class = "rtemis_input_error")
  expect_error(prop_boolean(TRUE, FALSE), "by name")
  expect_error(prop_integer(map = TRUE), "no argument `map`")
  expect_error(prop_boolean(vector = TRUE), "no argument `vector`")
})


# %% Declaration-time validation ----
test_that("a default that violates its own spec fails at declaration", {
  expect_error(prop_number(5, min = 0, max = 2))
  expect_error(prop_number(5, exclusive_max = 5))
  expect_error(prop_string("c", enum = c("a", "b")))
  expect_error(prop_string(""))
  expect_error(prop_integer(0L, min = 1L))
})

test_that("invalid factory arguments are rejected", {
  expect_error(prop_number(nullable = NA))
  expect_error(prop_number(description = c("a", "b")))
  expect_error(prop_integer(def_name = "1bad"), "def_name")
  expect_error(prop_string(pattern = "("), "valid regular expression")
})


# %% prop_spec ----
test_that("prop_spec exposes the declaration", {
  spec <- prop_spec(prop_number(
    0.3,
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
  expect_null(prop_spec(S7::new_property(S7::class_character)))
})

test_that("prop_spec rejects a non-property", {
  expect_error(prop_spec(1), class = "rtemis_type_error")
  expect_error(prop_spec("a"), class = "rtemis_type_error")
  expect_error(prop_spec(list(spec = 1)), class = "rtemis_type_error")
})
