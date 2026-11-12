# test-structural_props.R

library(S7)

# %% Fixtures ----
.SP <- local({
  Point <- new_class(
    "SP_Point",
    properties = list(
      x = prop_number(description = "Horizontal coordinate."),
      label = prop_string("", min_length = 0L)
    )
  )
  positions <- prop_array(
    prop_integer(min = 1L),
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
    additional_members = prop_any(),
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
          default = list(a = 1L)
        )
      )
    ),
    map = new_class(
      "SP_map",
      properties = list(
        x = prop_map(
          prop_number(min = 0, max = 1),
          min_members = 0L,
          default = quote(c(a = 0.5))
        )
      )
    ),
    obj = new_class(
      "SP_obj",
      properties = list(
        origin = prop_instance(Point, default = quote(Point(x = 0)))
      )
    ),
    coll = new_class(
      "SP_coll",
      properties = list(
        points = prop_map(
          prop_instance(Point),
          min_members = 0L,
          default = list()
        )
      )
    )
  )
})


# %% prop_string pattern and min_length ----
test_that("prop_string enforces pattern and min_length", {
  expect_identical(.SP$seq(x = "MAEp*")@x, "MAEp*")
  expect_error(.SP$seq(x = "M"), "at least 2 characters")
  expect_error(.SP$seq(x = "MA1"), "pattern")
  expect_error(
    new_prop_spec(type = "integer", pattern = "a"),
    "string declarations only"
  )
})


# %% prop_array ----
test_that("an array of scalars holds an atomic vector", {
  expect_identical(.SP$pos(x = integer())@x, integer())
  expect_identical(.SP$pos(x = c(3L, 1L))@x, c(3L, 1L))
  expect_error(.SP$pos(x = c(1L, 1L)), "distinct")
  expect_error(.SP$pos(x = 0L), ">= 1")
  expect_error(.SP$pos(x = list(1L)))
  expect_error(.SP$pos(x = c(a = 1L)), "attribute `names`")
})

test_that("an item with a description keeps its declaration and its R value", {
  p <- prop_array(prop_string(description = "A tag."))
  expect_identical(prop_spec(p)[["type"]], "array")
  expect_identical(prop_spec(p)[["items"]][["description"]], "A tag.")
  Cls <- new_class("SP_tags", properties = list(x = p))
  expect_identical(Cls(x = c("a", "b"))@x, c("a", "b"))
})

test_that("an array of nullable scalars holds a list with NULL elements", {
  Cls <- new_class(
    "SP_nullable_items",
    properties = list(x = prop_array(prop_string(nullable = TRUE)))
  )
  expect_identical(Cls(x = list("a", NULL))@x, list("a", NULL))
  expect_error(Cls(x = c("a", "b")))
})

test_that("nullable applies at the level it is declared", {
  outer <- new_class(
    "SP_null_outer",
    properties = list(x = prop_array(prop_string(), nullable = TRUE))
  )
  expect_null(outer()@x)
  expect_error(outer(x = list("a", NULL)))
})

test_that("unique_items holds for every item kind", {
  Cls <- new_class(
    "SP_unique_nested",
    properties = list(
      x = prop_array(prop_array(prop_integer()), unique_items = TRUE)
    )
  )
  expect_error(Cls(x = list(1:2, 1:2)), "distinct")
  Pts <- new_class(
    "SP_unique_points",
    properties = list(
      x = prop_array(prop_instance(.SP$Point), unique_items = TRUE)
    )
  )
  p <- .SP$Point(x = 1)
  expect_error(Pts(x = list(p, p)), "distinct")
})


# %% prop_matrix ----
test_that("prop_matrix enforces column count, rows and cell bounds", {
  m <- rbind(c(3L, 10L), c(15L, 22L))
  expect_identical(.SP$rng(x = m)@x, m)
  expect_identical(nrow(.SP$rng(x = matrix(integer(), ncol = 2L))@x), 0L)
  expect_error(.SP$rng(x = matrix(1:3, ncol = 3L)), "2 columns")
  expect_error(.SP$rng(x = rbind(c(0L, 4L))), "cells")
  expect_error(.SP$rng(x = 1:4), "matrix")
  m_names <- m
  dimnames(m_names) <- list(NULL, c("start", "end"))
  expect_error(.SP$rng(x = m_names), "dimnames")
  expect_error(
    prop_matrix(prop_string(), ncol = 2L),
    "prop_integer\\(\\) or prop_number\\(\\)"
  )
})


# %% prop_union ----
test_that("prop_union accepts a value matching any alternative", {
  expect_identical(.SP$mixed(x = c(1L, 5L))@x, c(1L, 5L))
  m <- rbind(c(2L, 4L))
  expect_identical(.SP$mixed(x = m)@x, m)
  # Grammar 3: the array alternative rejects a matrix, so a 3-column matrix
  # matches neither alternative.
  expect_error(.SP$mixed(x = matrix(1:6, ncol = 3L)), "one of 2 alternatives")
  expect_error(.SP$mixed(x = "a"), "one of 2 alternatives")
  expect_error(prop_union(list(prop_integer())), "at least two")
})

test_that("a union of instances requires a const discriminator", {
  A <- new_class(
    "SP_A",
    properties = list(kind = prop_string(const = "a"), x = prop_number(0))
  )
  B <- new_class(
    "SP_B",
    properties = list(kind = prop_string(const = "b"), y = prop_number(0))
  )
  C <- new_class("SP_C", properties = list(x = prop_number(0)))
  expect_no_error(prop_union(list(prop_instance(A), prop_instance(B))))
  expect_error(
    prop_union(list(prop_instance(A), prop_instance(C))),
    "const"
  )
})


# %% prop_struct ----
test_that("a struct requires its non-nullable members", {
  expect_identical(.SP$closed()@x, list(a = 1L))
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

test_that("a struct member declares no default; a const member is required", {
  expect_error(
    prop_struct(list(a = prop_integer(1L))),
    "must not declare a `default`"
  )
  s <- prop_struct(list(type = prop_string(const = "pt"), x = prop_number()))
  expect_identical(prop_spec(s)[["required"]], c("type", "x"))
})

test_that("a struct with no required member admits an empty list", {
  Cls <- new_class(
    "SP_struct_empty",
    properties = list(x = prop_struct(list(a = prop_string(nullable = TRUE))))
  )
  expect_identical(Cls(x = list())@x, list())
})

test_that("an open struct keeps undeclared members", {
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

test_that("additional_members validates undeclared members by its declaration", {
  Cls <- new_class(
    "SP_struct_extra",
    properties = list(
      x = prop_struct(
        list(id = prop_string()),
        additional_members = prop_number()
      )
    )
  )
  expect_identical(
    Cls(x = list(id = "a", w = 0.5))@x,
    list(id = "a", w = 0.5)
  )
  expect_error(Cls(x = list(id = "a", w = "x")), "member 'w'")
})


# %% prop_map ----
test_that("prop_map validates keys and every value", {
  expect_identical(.SP$map()@x, c(a = 0.5))
  expect_identical(
    .SP$map(x = c(a = 0.1, `b c` = 1))@x,
    c(a = 0.1, `b c` = 1)
  )
  expect_error(.SP$map(x = c(a = 2)), "<= 1")
  expect_error(.SP$map(x = 0.1), "named")
  expect_identical(.SP$map(x = numeric(0))@x, numeric(0))
})

test_that("a map rejects empty, missing, and repeated keys", {
  Cls <- new_class(
    "SP_str_map",
    properties = list(x = prop_map(prop_string()))
  )
  expect_error(Cls(x = c(a = "1", "2")), "non-empty name")
  # A key of spaces is a non-empty JSON key, as the emitted
  # `propertyNames: {minLength: 1}` states.
  expect_identical(Cls(x = c(a = "1", `  ` = "2"))@x, c(a = "1", `  ` = "2"))
  expect_error(Cls(x = c(a = "1", a = "2")), "distinct names")
})

test_that("a map of any values carries a named list", {
  Cls <- new_class(
    "SP_any_map",
    properties = list(x = prop_map(prop_any(), nullable = TRUE))
  )
  expect_null(Cls()@x)
  nested <- list(a = 1, b = "two", c = list(d = TRUE), e = NULL)
  expect_identical(Cls(x = nested)@x, nested)
  expect_error(Cls(x = list(1, 2)), "named")
  expect_error(Cls(x = list()), "must not be empty")
  expect_error(Cls(x = list(a = 1, a = 2)), "distinct names")
  expect_error(Cls(x = "a"), "list")
})


# %% prop_instance ----
test_that("prop_instance requires an object of exactly the class", {
  expect_identical(.SP$obj()@origin@x, 0)
  expect_error(.SP$obj(origin = list(x = 1)))
  Sub <- new_class(
    "SP_PointSub",
    parent = .SP$Point,
    properties = list(z = prop_number(0))
  )
  expect_error(.SP$obj(origin = Sub(x = 1)), "must be a SP_Point object")
  p <- .SP$Point(x = 1)
  expect_identical(.SP$coll(points = list(a = p))@points[["a"]]@x, 1)
  expect_error(
    .SP$coll(points = list(a = list(x = 1))),
    "entry 'a' must be a SP_Point"
  )
  expect_error(.SP$coll(points = list(p)), "named")
  expect_error(prop_instance("Point"), "S7 class")
})


# %% prop_any ----
test_that("prop_any admits every value, NULL included", {
  Cls <- new_class("SP_any", properties = list(x = prop_any()))
  expect_null(Cls()@x)
  expect_identical(Cls(x = 1:3)@x, 1:3)
  expect_identical(Cls(x = list(a = NULL))@x, list(a = NULL))
})


# %% Declaration errors ----
test_that("structural factories reject malformed declarations", {
  expect_error(
    prop_array(S7::new_property(S7::class_integer)),
    "prop_\\* factory"
  )
  expect_error(prop_array(prop_integer(1L)), "must not declare a `default`")
  expect_error(prop_struct(list()), "non-empty list")
  expect_error(prop_array(prop_integer(), min_items = -1L), "non-negative")
  expect_error(
    prop_array(prop_integer(), min_items = 3L, max_items = 2L),
    "at least `min_items`"
  )
  expect_error(prop_array(prop_integer(), 1L), "by name")
})
