# test-structural_props.R

library(S7)

# %% Fixtures ----
.SP <- local({
  Point <- new_class(
    "SP_Point",
    properties = list(
      x = prop_float(description = "Horizontal coordinate."),
      label = prop_string(default = "", allow_empty = TRUE)
    )
  )
  positions <- prop_integer(
    min = 1L,
    vector = TRUE,
    min_items = 0L,
    unique_items = TRUE,
    def_name = "positionIndex"
  )
  ranges <- prop_matrix(
    prop_integer(min = 1L),
    ncol = 2L,
    min_items = 0L,
    def_name = "rangeIndex"
  )
  record <- prop_struct(
    members = list(position = prop_integer(min = 1L)),
    required = "position",
    additional = TRUE,
    def_name = "record"
  )
  list(
    Point = Point,
    positions = positions,
    ranges = ranges,
    record = record,
    seq = new_class(
      "SP_seq",
      properties = list(
        x = prop_string(pattern = "^[A-Za-z*]+$", min_length = 2L)
      )
    ),
    pos = new_class("SP_pos", properties = list(x = positions)),
    rng = new_class("SP_rng", properties = list(x = ranges)),
    mixed = new_class(
      "SP_mixed",
      properties = list(x = prop_union(list(positions, ranges)))
    ),
    rec = new_class(
      "SP_rec",
      properties = list(
        x = prop_array(record, default = list(), min_items = 0L)
      )
    ),
    closed = new_class(
      "SP_closed",
      properties = list(
        x = prop_struct(
          members = list(a = prop_integer(), b = prop_string(nullable = TRUE)),
          required = "a"
        )
      )
    ),
    map = new_class(
      "SP_map",
      properties = list(
        x = prop_map(
          prop_float(min = 0, max = 1),
          min_items = 0L,
          default = list()
        )
      )
    ),
    obj = new_class(
      "SP_obj",
      properties = list(
        origin = prop_object(Point, default = quote(Point(x = 0)))
      )
    ),
    coll = new_class(
      "SP_coll",
      properties = list(
        points = prop_collection(Point, container = "map", default = list())
      )
    )
  )
})


# %% prop_string pattern and min_length ----
test_that("prop_string enforces pattern and min_length", {
  expect_identical(.SP$seq(x = "MAEp*")@x, "MAEp*")
  expect_error(.SP$seq(x = "M"), "at least 2 characters")
  expect_error(.SP$seq(x = "MA1"), "pattern")
  expect_error(prop_string(pattern = "("), "valid regular expression")
  expect_error(prop_integer(), NA)
  expect_error(
    new_prop_spec(type = "integer", pattern = "a"),
    "string properties only"
  )
})


# %% min_items = 0 ----
test_that("a vector container with min_items 0 holds an empty vector", {
  expect_identical(.SP$pos(x = integer())@x, integer())
  expect_identical(.SP$pos(x = c(3L, 1L))@x, c(3L, 1L))
  expect_error(.SP$pos(x = c(1L, 1L)), "distinct")
  expect_error(.SP$pos(x = 0L), ">= 1")
})


# %% prop_boolean ----
test_that("prop_boolean defaults to NULL when nullable", {
  Cls <- new_class(
    "SP_bool_null",
    properties = list(x = prop_boolean(nullable = TRUE))
  )
  expect_null(Cls()@x)
  expect_null(prop_spec(prop_boolean(nullable = TRUE))[["default"]])
  expect_false(prop_spec(prop_boolean())[["default"]])
})


# %% prop_matrix ----
test_that("prop_matrix enforces column count, rows and cell bounds", {
  m <- rbind(c(3L, 10L), c(15L, 22L))
  expect_identical(.SP$rng(x = m)@x, m)
  expect_identical(nrow(.SP$rng(x = matrix(integer(), ncol = 2L))@x), 0L)
  expect_error(.SP$rng(x = matrix(1:3, ncol = 3L)), "2 columns")
  expect_error(.SP$rng(x = rbind(c(0L, 4L))), "cells")
  expect_error(.SP$rng(x = 1:4), "matrix")
  expect_error(
    prop_matrix(prop_string(), ncol = 2L),
    "prop_integer\\(\\) or prop_float\\(\\)"
  )
})


# %% prop_union ----
test_that("prop_union accepts a value matching any alternative", {
  expect_identical(.SP$mixed(x = c(1L, 5L))@x, c(1L, 5L))
  m <- rbind(c(2L, 4L))
  expect_identical(.SP$mixed(x = m)@x, m)
  expect_error(.SP$mixed(x = "a"), "one of 2 alternatives")
  expect_error(prop_union(list(prop_integer())), "at least two")
})


# %% prop_struct / prop_array ----
test_that("an open struct keeps undeclared members and requires its required ones", {
  recs <- list(
    list(position = 301L, from = "P", to = "L", conditions = list("FTD")),
    list(position = 5L)
  )
  expect_identical(.SP$rec(x = recs)@x, recs)
  expect_identical(.SP$rec()@x, list())
  expect_error(.SP$rec(x = list(list(from = "P"))), "member 'position'")
  expect_error(
    .SP$rec(x = list(list(position = 0L))),
    "element 1 member 'position'"
  )
  expect_error(.SP$rec(x = list(list(position = 3))), "type integer")
})

test_that("a closed struct rejects undeclared members", {
  expect_identical(.SP$closed(x = list(a = 1L))@x, list(a = 1L))
  expect_identical(
    .SP$closed(x = list(a = 1L, b = NULL))@x,
    list(a = 1L, b = NULL)
  )
  expect_error(.SP$closed(x = list(a = 1L, c = 2L)), "must not have member 'c'")
  expect_error(.SP$closed(x = list(b = "x")), "member 'a'")
  expect_error(
    prop_struct(list(a = prop_integer()), required = "z"),
    "undeclared"
  )
})


# %% prop_map ----
test_that("prop_map validates keys and every value", {
  expect_identical(.SP$map()@x, list())
  expect_identical(
    .SP$map(x = list(a = 0.1, `b c` = 1))@x,
    list(a = 0.1, `b c` = 1)
  )
  expect_error(.SP$map(x = list(a = 2)), "entry 'a' must be <= 1")
  expect_error(.SP$map(x = list(0.1)), "named")
})


# %% prop_object / prop_collection ----
test_that("prop_object and prop_collection require instances of the class", {
  expect_identical(.SP$obj()@origin@x, 0)
  expect_error(.SP$obj(origin = list(x = 1)))
  p <- .SP$Point(x = 1)
  expect_identical(.SP$coll(points = list(a = p))@points[["a"]]@x, 1)
  expect_error(
    .SP$coll(points = list(a = list(x = 1))),
    "entry 'a' must be a SP_Point"
  )
  expect_error(.SP$coll(points = list(p)), "named")
  expect_error(prop_object("Point"), "S7 class")
})


# %% Declaration errors ----
test_that("structural factories reject malformed declarations", {
  expect_error(
    prop_array(S7::new_property(S7::class_integer)),
    "prop_\\* factory"
  )
  expect_error(prop_struct(list()), "non-empty list")
  expect_error(prop_integer(min_items = -1L), "non-negative")
  expect_error(
    prop_integer(vector = TRUE, min_items = 3L, max_items = 2L),
    "at least `min_items`"
  )
  expect_error(prop_integer(def_name = "1bad"), "def_name")
})
