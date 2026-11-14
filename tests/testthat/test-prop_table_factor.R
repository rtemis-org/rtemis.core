# test-prop_table_factor.R

library(S7)
skip_if_not_installed("jsonlite")

.rt_tf <- function(p, v) {
  json <- to_JSON(prop_encode(p, v))
  prop_decode(p, jsonlite::fromJSON(json, simplifyVector = FALSE))
}

.TF <- local({
  metrics <- prop_table(
    list(
      fold = prop_integer(min = 1L),
      accuracy = prop_number(min = 0, max = 1, nullable = TRUE),
      note = prop_string(nullable = TRUE)
    ),
    min_items = 0L
  )
  outcome <- prop_factor(enum = c("case", "control"), nullable_codes = TRUE)
  Res <- new_class(
    "TF_Res",
    properties = list(metrics = metrics, outcome = outcome)
  )
  list(metrics = metrics, outcome = outcome, Res = Res)
})


# %% prop_table ----
test_that("a table is a data.frame of declared columns", {
  v <- data.frame(fold = 1:2, accuracy = c(0.9, NA))
  expect_identical(
    .TF$Res(metrics = v, outcome = factor("case"))@metrics,
    v
  )
  bad <- list(
    list(data.frame(accuracy = 0.5), "must have column 'fold'"),
    list(data.frame(fold = 1L, other = 1), "must not have column 'other'"),
    list(data.frame(fold = 0L), "column 'fold' must be >= 1"),
    list(data.frame(fold = 1L, accuracy = NaN), "finite numbers or NA"),
    list(data.frame(fold = NA_integer_), "missing values"),
    list(data.frame(fold = 1L, row.names = "a"), "automatic row names"),
    list(
      structure(data.frame(fold = 1L), class = c("tbl", "data.frame")),
      "data.frame"
    ),
    list(
      data.frame(fold = 1L, note = factor("a")),
      "column 'note' must be of type string"
    )
  )
  for (case in bad) {
    expect_error(
      .TF$Res(metrics = case[[1L]], outcome = factor("case")),
      case[[2L]],
      fixed = TRUE
    )
  }
})

test_that("a table emits an array of row objects", {
  defs <- new_JSONSchema_defs()
  f <- prop_to_JSONSchema(.TF$metrics, defs)
  expect_identical(f[["type"]], "array")
  expect_identical(f[["x-rtemis"]], list(node = "table"))
  row <- f[["items"]]
  expect_identical(as.character(row[["required"]]), "fold")
  expect_identical(
    row[["properties"]][["accuracy"]][["type"]],
    c("number", "null")
  )
  expect_false(row[["additionalProperties"]])
  open <- prop_table(
    list(a = prop_integer()),
    additional_columns = prop_number(),
    required = "a"
  )
  row <- prop_to_JSONSchema(open, new_JSONSchema_defs())[["items"]]
  expect_identical(row[["additionalProperties"]], list(type = "number"))
  expect_identical(row[["propertyNames"]], list(minLength = 1L))
  expect_identical(
    prop_to_JSONSchema(open, new_JSONSchema_defs())[["minItems"]],
    1L
  )
})

test_that("tables round-trip, with every column in every row", {
  v <- data.frame(
    fold = 1:3,
    accuracy = c(0.1, NA, 0.1 + 0.2),
    note = c("a", NA, "c")
  )
  expect_identical(.rt_tf(.TF$metrics, v), v)
  expect_identical(
    to_JSON(prop_encode(
      .TF$metrics,
      v[2L, , drop = FALSE] |> `rownames<-`(NULL)
    )),
    '[{"fold":2,"accuracy":null,"note":null}]'
  )
  empty <- prop_decode(.TF$metrics, list())
  expect_identical(names(empty), "fold")
  expect_identical(nrow(empty), 0L)
  expect_error(
    prop_decode(
      .TF$metrics,
      list(list(fold = 1L), list(fold = 2L, note = "x"))
    ),
    "different columns"
  )
  expect_error(
    prop_decode(.TF$metrics, list(fold = 1L)),
    "array of row objects"
  )
})


# %% prop_factor ----
test_that("a factor is an unordered factor of declared levels", {
  ok <- factor(c("case", NA, "control"))
  expect_identical(
    .TF$Res(metrics = data.frame(fold = 1L), outcome = ok)@outcome,
    ok
  )
  bad <- list(
    list(factor("x"), "levels from"),
    list(factor(c("case", "control"), ordered = TRUE), "unordered factor"),
    list(factor(character(0L)), "at least one level"),
    list(c("case"), "S3<factor>"),
    list(structure(factor("case"), names = "a"), "attribute `names`")
  )
  for (case in bad) {
    expect_error(
      .TF$Res(metrics = data.frame(fold = 1L), outcome = case[[1L]]),
      case[[2L]],
      fixed = TRUE
    )
  }
  strict <- prop_factor()
  F <- new_class("TF_F", properties = list(f = strict))
  expect_error(F(f = factor(c("a", NA))), "missing values")
})

test_that("a factor emits its levels and codes", {
  f <- prop_to_JSONSchema(.TF$outcome, new_JSONSchema_defs())
  expect_identical(f[["x-rtemis"]], list(node = "factor"))
  expect_identical(as.character(f[["required"]]), c("levels", "codes"))
  levels <- f[["properties"]][["levels"]]
  expect_true(levels[["uniqueItems"]])
  expect_identical(levels[["minItems"]], 1L)
  expect_identical(
    as.character(levels[["items"]][["enum"]]),
    c("case", "control")
  )
  codes <- f[["properties"]][["codes"]]
  expect_identical(
    codes[["items"]],
    list(type = c("integer", "null"), minimum = 1L)
  )
})

test_that("factors round-trip, and decoding checks levels and codes", {
  v <- factor(c("control", NA, "case"))
  expect_identical(.rt_tf(.TF$outcome, v), v)
  expect_identical(
    to_JSON(prop_encode(.TF$outcome, v)),
    '{"levels":["case","control"],"codes":[2,null,1]}'
  )
  expect_error(
    prop_decode(
      .TF$outcome,
      list(levels = list("case", "case"), codes = list(1L))
    ),
    "distinct",
    class = "rtemis_value_error"
  )
  expect_error(
    prop_decode(.TF$outcome, list(levels = list("case"), codes = list(2L))),
    "index its levels",
    class = "rtemis_value_error"
  )
})


# %% keys, contains, struct counts ----
test_that("map keys satisfy their declaration", {
  m <- prop_map(prop_number(), keys = prop_string(pattern = "^[a-z]+$"))
  M <- new_class("TF_M", properties = list(m = m))
  expect_identical(M(m = c(a = 1))@m, c(a = 1))
  expect_error(M(m = c(A = 1)), "key 'A' must match the pattern")
  f <- prop_to_JSONSchema(m, new_JSONSchema_defs())
  expect_identical(
    f[["propertyNames"]],
    list(type = "string", minLength = 1L, pattern = "^[a-z]+$")
  )
  expect_error(prop_map(prop_number(), keys = prop_integer()), "prop_string")
  expect_error(
    prop_map(prop_number(), keys = prop_string(min_length = 0L)),
    "min_length"
  )
  expect_error(
    prop_map(prop_number(), keys = prop_string(nullable = TRUE)),
    "non-nullable"
  )
})

test_that("an array holds as many elements satisfying `contains` as declared", {
  a <- prop_array(
    prop_number(min = 0),
    contains = prop_number(exclusive_min = 0),
    min_contains = 2L,
    max_contains = 3L
  )
  A <- new_class("TF_A", properties = list(a = a))
  expect_identical(A(a = c(0, 1, 2))@a, c(0, 1, 2))
  expect_error(A(a = c(0, 1)), "at least 2 elements that satisfy `contains`")
  expect_error(A(a = c(1, 2, 3, 4)), "at most 3 elements")
  f <- prop_to_JSONSchema(a, new_JSONSchema_defs())
  expect_identical(f[["contains"]], list(type = "number", exclusiveMinimum = 0))
  expect_identical(f[["minContains"]], 2L)
  expect_identical(f[["maxContains"]], 3L)
  one <- prop_to_JSONSchema(
    prop_array(prop_string(), contains = prop_string(const = "x")),
    new_JSONSchema_defs()
  )
  expect_null(one[["minContains"]])
  expect_error(
    prop_array(prop_string(), min_contains = 2L),
    "apply with `contains`"
  )
  expect_error(
    prop_array(prop_string(), max_contains = 2L),
    "apply with `contains`"
  )
  expect_error(
    prop_array(
      prop_string(),
      contains = prop_string(),
      min_contains = 2L,
      max_contains = 1L
    ),
    "at least `min_contains`"
  )
})

test_that("a struct counts its members", {
  s <- prop_struct(
    list(a = prop_integer(nullable = TRUE), b = prop_integer(nullable = TRUE)),
    min_members = 1L,
    max_members = 1L
  )
  S <- new_class("TF_S", properties = list(s = s))
  expect_identical(S(s = list(a = 1L))@s, list(a = 1L))
  expect_error(S(s = setNames(list(), character(0L))), "at least 1 member")
  expect_error(S(s = list(a = 1L, b = 2L)), "at most 1 member")
  f <- prop_to_JSONSchema(s, new_JSONSchema_defs())
  expect_identical(f[["minProperties"]], 1L)
  expect_identical(f[["maxProperties"]], 1L)
  plain <- prop_to_JSONSchema(
    prop_struct(list(a = prop_integer())),
    new_JSONSchema_defs()
  )
  expect_null(plain[["minProperties"]])
})


# %% to_JSON ----
test_that("to_JSON writes a data frame as rows without row names", {
  df <- data.frame(
    a = c(0.1, 0.1 + 0.2),
    b = c("x", NA),
    row.names = c("r1", "r2")
  )
  expect_identical(
    to_JSON(df),
    '[{"a":0.1,"b":"x"},{"a":0.30000000000000004,"b":null}]'
  )
  expect_identical(to_JSON(data.frame(a = integer(0L))), "[]")
})
