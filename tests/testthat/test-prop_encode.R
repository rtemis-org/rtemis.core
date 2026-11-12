# test-prop_encode.R

library(S7)
skip_if_not_installed("jsonlite")

# Write with to_JSON(), read with jsonlite::fromJSON(simplifyVector = FALSE),
# as the rule's "Values in JSON" section fixes the round trip.
.rt <- function(p, v) {
  json <- to_JSON(prop_encode(p, v))
  prop_decode(p, jsonlite::fromJSON(json, simplifyVector = FALSE))
}

.EN <- local({
  P <- new_class(
    "EN_P",
    properties = list(kind = prop_string(const = "p"), x = prop_number(0))
  )
  Q <- new_class(
    "EN_Q",
    properties = list(kind = prop_string(const = "q"), y = prop_number(0))
  )
  list(P = P, Q = Q)
})


# %% to_JSON ----
test_that("to_JSON writes each double in its shortest identical form", {
  expect_identical(
    to_JSON(list(a = 0.1, b = 0.1 + 0.2, c = 100, d = 2.5e-8)),
    '{"a":0.1,"b":0.30000000000000004,"c":100,"d":2.5e-08}'
  )
  set.seed(2026)
  x <- c(runif(10000), rnorm(1000) * 1e6, 10^runif(1000, -300, 300))
  expect_identical(jsonlite::fromJSON(to_JSON(I(x))), x)
})

test_that("to_JSON writes scalars, arrays, objects, and nulls", {
  expect_identical(
    to_JSON(list(
      a = 1L,
      b = I(2.5),
      c = c(1, 2),
      d = NULL,
      e = NA,
      f = structure(list(), names = character(0)),
      g = list()
    )),
    '{"a":1,"b":[2.5],"c":[1,2],"d":null,"e":null,"f":{},"g":[]}'
  )
  expect_identical(
    to_JSON(matrix(c(0.1, 0.2, 0.3, 0.4), 2)),
    "[[0.1,0.3],[0.2,0.4]]"
  )
})


# %% Round trips ----
test_that("scalars round-trip", {
  set.seed(1)
  expect_identical(.rt(prop_boolean(), TRUE), TRUE)
  expect_identical(.rt(prop_integer(), 7L), 7L)
  x <- runif(1)
  expect_identical(.rt(prop_number(), x), x)
  expect_identical(.rt(prop_string(), "x"), "x")
  expect_null(.rt(prop_string(nullable = TRUE), NULL))
})

test_that("arrays round-trip at lengths 0, 1, and 2", {
  set.seed(2)
  p0 <- prop_array(prop_number(), min_items = 0L)
  expect_identical(.rt(p0, numeric(0)), numeric(0))
  x1 <- runif(1)
  expect_identical(.rt(p0, x1), x1)
  x2 <- runif(2)
  expect_identical(.rt(p0, x2), x2)
  expect_identical(.rt(prop_array(prop_string()), "a"), "a")
  expect_identical(
    to_JSON(prop_encode(prop_array(prop_string()), "a")),
    '["a"]'
  )
  expect_identical(
    .rt(prop_array(prop_string(nullable = TRUE)), list("a", NULL)),
    list("a", NULL)
  )
  expect_identical(
    .rt(prop_array(prop_string(description = "t")), c("a", "b")),
    c("a", "b")
  )
  expect_identical(
    .rt(prop_array(prop_array(prop_integer())), list(1L, 2:3)),
    list(1L, 2:3)
  )
  expect_null(.rt(prop_array(prop_string(), nullable = TRUE), NULL))
})

test_that("maps round-trip and an empty map reads in the named form", {
  set.seed(3)
  m <- c(a = 0.25, b = runif(1))
  expect_identical(.rt(prop_map(prop_number()), m), m)
  p0 <- prop_map(prop_number(), min_members = 0L)
  expect_identical(to_JSON(prop_encode(p0, numeric(0))), "{}")
  expect_identical(
    .rt(p0, numeric(0)),
    stats::setNames(numeric(0), character(0))
  )
  bag <- list(a = 1L, b = list("x", NULL))
  expect_identical(.rt(prop_map(prop_any()), bag), bag)
})

test_that("matrices round-trip, zero rows included", {
  set.seed(4)
  p <- prop_matrix(prop_number(), ncol = 2L)
  m <- matrix(runif(4), 2)
  expect_identical(.rt(p, m), m)
  p0 <- prop_matrix(prop_integer(), ncol = 2L, min_items = 0L)
  m0 <- matrix(integer(0), ncol = 2L)
  expect_identical(.rt(p0, m0), m0)
})

test_that("struct presence round-trips: absent, null, and a value", {
  st <- prop_struct(list(a = prop_integer(), b = prop_string(nullable = TRUE)))
  expect_identical(.rt(st, list(a = 1L)), list(a = 1L))
  expect_identical(.rt(st, list(a = 1L, b = NULL)), list(a = 1L, b = NULL))
  expect_identical(.rt(st, list(a = 1L, b = "x")), list(a = 1L, b = "x"))
  expect_identical(
    to_JSON(prop_encode(st, list(a = 1L, b = NULL))),
    '{"a":1,"b":null}'
  )
  st0 <- prop_struct(list(b = prop_string(nullable = TRUE)))
  expect_identical(to_JSON(prop_encode(st0, list())), "{}")
  expect_identical(.rt(st0, list()), structure(list(), names = character(0)))
  open <- prop_struct(
    list(a = prop_integer()),
    additional_members = prop_number()
  )
  expect_identical(.rt(open, list(a = 1L, w = 0.5)), list(a = 1L, w = 0.5))
})

test_that("instances round-trip through their constructor", {
  p <- .EN$P(x = 0.3)
  expect_identical(.rt(prop_instance(.EN$P), p), p)
  qs <- list(.EN$Q(y = 1), .EN$Q())
  expect_identical(.rt(prop_array(prop_instance(.EN$Q)), qs), qs)
  expect_identical(
    .rt(prop_map(prop_instance(.EN$Q)), list(a = .EN$Q())),
    list(a = .EN$Q())
  )
  # Absent members take the class's defaults.
  read <- prop_decode(prop_instance(.EN$P), list(x = 2))
  expect_identical(read, .EN$P(x = 2))
})

test_that("a union reads as the first alternative whose JSON form matches", {
  u <- prop_union(list(prop_instance(.EN$P), prop_instance(.EN$Q)))
  expect_identical(.rt(u, .EN$Q(y = 2)), .EN$Q(y = 2))
  pos <- prop_array(prop_integer(min = 1L), min_items = 0L)
  rng <- prop_matrix(prop_integer(min = 1L), ncol = 2L, min_items = 0L)
  idx <- prop_union(list(pos, rng))
  expect_identical(.rt(idx, c(1L, 4L)), c(1L, 4L))
  expect_identical(.rt(idx, rbind(c(1L, 4L))), rbind(c(1L, 4L)))
  # An empty ranges matrix and an empty positions array are both `[]`.
  expect_identical(.rt(idx, matrix(integer(0), ncol = 2L)), integer(0))
})

test_that("the stated exceptions read back as the rule says", {
  expect_identical(.rt(prop_number(), 3L), 3)
  expect_identical(.rt(prop_any(), list(1L, "a")), list(1L, "a"))
  expect_identical(.rt(prop_any(), c(1, 2)), list(1L, 2L))
})


# %% Encoding ----
test_that("a string's class is dropped", {
  json <- jsonlite::toJSON(list(a = 1))
  expect_identical(to_JSON(prop_encode(prop_string(), json)), '"{\\"a\\":[1]}"')
})

test_that("an invalid value does not encode", {
  expect_error(prop_encode(prop_integer(), 1.5), class = "rtemis_value_error")
  expect_error(
    prop_encode(S7::new_property(S7::class_integer), 1L),
    "prop_\\* factory"
  )
})


# %% Decoding ----
test_that("integers decode within R's integer range", {
  expect_identical(prop_decode(prop_integer(), 3), 3L)
  expect_identical(prop_decode(prop_integer(), 2147483647L), 2147483647L)
  expect_error(prop_decode(prop_integer(), -2147483648), "whole number")
  expect_error(prop_decode(prop_integer(), 3e9), "whole number")
  expect_error(prop_decode(prop_integer(), 2.5), "whole number")
})

test_that("decoding checks the JSON form and the declaration", {
  expect_error(prop_decode(prop_string(), list("a")), "a string")
  expect_error(prop_decode(prop_array(prop_string()), "a"), "an array")
  expect_error(prop_decode(prop_map(prop_string()), list("a")), "an object")
  expect_error(
    prop_decode(prop_integer(min = 1L), 0L),
    class = "rtemis_value_error"
  )
  expect_error(
    prop_decode(prop_instance(.EN$P), list(z = 1)),
    "no property 'z'"
  )
})
