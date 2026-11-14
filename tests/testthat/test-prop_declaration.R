# test-prop_declaration.R

library(S7)

.PD <- local({
  PD_Point <- new_class("PD_Point", properties = list(x = prop_number(0)))
  environment()
})

.pd_cases <- function() {
  Point <- PD_Point <- .PD$PD_Point
  list(
    prop_boolean(TRUE),
    prop_integer(3L, min = 1L, max = 10, nullable = TRUE),
    prop_number(enum = c(0.5, 1)),
    prop_number(const = 1L),
    prop_string("a", pattern = "^[a-z]$", max_length = 1L, def_name = "Letter"),
    prop_string(const = "x"),
    prop_string(min_length = 0L, description = "Free text."),
    prop_array(prop_integer(min = 1L), min_items = 0L, unique_items = TRUE),
    prop_array(prop_string(nullable = TRUE), max_items = 3L),
    prop_array(
      prop_number(),
      contains = prop_number(exclusive_min = 0),
      min_contains = 2L
    ),
    prop_map(prop_number(), keys = prop_string(pattern = "^[a-z]+$")),
    prop_map(prop_any(), nullable = TRUE),
    prop_array(prop_instance(Point)),
    prop_map(prop_instance("PD_Point"), min_members = 0L),
    prop_matrix(prop_integer(nullable = TRUE), ncol = 2L, min_items = 0L),
    prop_struct(
      list(a = prop_integer(), b = prop_string(nullable = TRUE)),
      required = "b",
      additional_members = prop_any(),
      max_members = 4L
    ),
    prop_table(
      list(a = prop_integer(), b = prop_number(nullable = TRUE)),
      additional_columns = prop_string()
    ),
    prop_factor(enum = c("a", "b"), nullable_codes = TRUE),
    prop_union(list(prop_string(), prop_array(prop_string())), nullable = TRUE),
    prop_instance(
      Point,
      default = quote(PD_Point()),
      members = list(x = prop_number(min = 0))
    ),
    prop_any(default = list(1L)),
    prop_extend(prop_number(0.5), "mypkg", list(tunable = TRUE, group = "g"))
  )
}


# %% prop_declaration / prop_build ----
test_that("a declaration rebuilds into a property of the same declaration", {
  env <- list2env(list(PD_Point = .PD$PD_Point))
  for (p in .pd_cases()) {
    d <- prop_declaration(p)
    expect_identical(
      prop_declaration(prop_build(d, env)),
      d,
      label = d[["factory"]]
    )
  }
})

test_that("a declaration lists every argument of its factory", {
  for (p in .pd_cases()) {
    d <- prop_declaration(p)
    f <- getExportedValue("rtemis.core", d[["factory"]])
    expect_identical(
      names(d[["arguments"]]),
      setdiff(names(formals(f)), "..."),
      label = d[["factory"]]
    )
  }
})

test_that("a declaration states what its factory was given", {
  d <- prop_declaration(prop_integer(3L, min = 1, max = 10))
  expect_identical(d[["arguments"]][["min"]], 1L)
  expect_identical(d[["arguments"]][["default"]], 3L)
  expect_null(prop_declaration(prop_string(const = "x"))[["arguments"]][[
    "default"
  ]])
  PD_Point <- .PD$PD_Point
  q <- prop_declaration(prop_instance(PD_Point, default = quote(PD_Point())))
  expect_identical(q[["arguments"]][["default"]], quote(PD_Point()))
  expect_match(q[["arguments"]][["cls"]], "PD_Point$")
  s <- prop_declaration(prop_struct(list(a = prop_integer()), required = "a"))
  expect_null(s[["arguments"]][["required"]])
  expect_identical(
    prop_declaration(prop_bag())[["arguments"]][["values"]][["factory"]],
    "prop_any"
  )
  expect_error(prop_declaration(optional_logical_scalar), "prop_\\* factory")
})

test_that("prop_build evaluates and resolves in its environment", {
  PD_Point <- .PD$PD_Point
  d <- prop_declaration(prop_instance("PD_Point", default = quote(PD_Point())))
  env <- list2env(list(PD_Point = .PD$PD_Point))
  p <- prop_build(d, env)
  expect_identical(p[["default"]], quote(PD_Point()))
  expect_error(prop_build(list(factory = "prop_none")), "prop_declaration")
})


# %% prop_extend / prop_extension ----
test_that("producer fields merge by name and travel with the declaration", {
  p <- prop_extend(prop_number(0.5), "mypkg", list(a = 1, b = 2))
  p <- prop_extend(p, "mypkg", list(b = 3))
  expect_identical(prop_extension(p, "mypkg"), list(a = 1, b = 3))
  expect_null(prop_extension(p, "other"))
  arr <- prop_spec(prop_array(prop_extend(prop_string(), "mypkg", list(a = 1))))
  expect_identical(arr[["items"]][["extensions"]], list(mypkg = list(a = 1)))
  expect_identical(
    prop_to_JSONSchema(p, new_JSONSchema_defs()),
    list(type = "number")
  )
})


# %% prop_integer bounds ----
test_that("a bound of prop_integer is a whole number", {
  expect_error(prop_integer(min = 0.5), "whole number")
  expect_error(prop_integer(exclusive_max = 1.5), "whole number")
  expect_no_error(prop_integer(min = 1, max = 3e9))
})
