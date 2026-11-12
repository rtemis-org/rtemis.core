# test-utils.R

# match_arg ----
test_that("match_arg matches case-insensitively", {
  expect_equal(
    match_arg("papaya", c("AppleExtreme", "SuperBanana", "PapayaMaster")),
    "PapayaMaster"
  )
})

test_that("match_arg supports partial matching", {
  expect_equal(
    match_arg("apple", c("AppleExtreme", "SuperBanana", "PapayaMaster")),
    "AppleExtreme"
  )
})

test_that("match_arg errors on ambiguous match", {
  expect_error(match_arg("a", c("Apple", "Apricot", "Banana")))
})

# abbreviate_class ----
test_that("abbreviate_class returns abbreviated class string", {
  result <- abbreviate_class("hello")
  expect_match(result, "^<.+>$")
  expect_true(nchar(result) >= 3) # at least < + char + >
})

test_that("abbreviate_class respects n parameter", {
  result <- abbreviate_class(iris, n = 3)
  expect_match(result, "^<.+>$")
})

# collapse_head ----
test_that("collapse_head collapses vector with ellipsis", {
  expect_equal(collapse_head(1:10, maxlength = 3), "1, 2, 3, ...")
  expect_equal(collapse_head(1:3, maxlength = 3), "1, 2, 3")
  expect_equal(collapse_head(1:5, maxlength = -1), "1, 2, 3, 4, 5")
  expect_equal(
    collapse_head(c("a", "b", "c"), maxlength = 2L, format_fn = toupper),
    "A, B, ..."
  )
})


# %% nullable ----
test_that("nullable() returns a union of NULL and the type", {
  u <- nullable(S7::class_character)
  expect_true(inherits(u, "S7_union"))
  Cls <- S7::new_class("UT_nullable", properties = list(x = u))
  expect_null(Cls()@x)
  expect_identical(Cls(x = "a")@x, "a")
  expect_error(nullable("character"), "S7 base class or S7 class")
})


# %% printls ----
test_that("printls() names the class of an element it does not expand", {
  pc <- stats::prcomp(matrix(c(1, 2, 3, 4, 2, 4, 1, 3), 4))
  expect_output(printls(list(a = pc)), "object of class 'prcomp'")
  expect_output(printls(pc), "object of class 'prcomp'")
})
