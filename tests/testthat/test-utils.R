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
