# test-clean.R

# clean_int ----
test_that("clean_int returns integer input unchanged", {
  expect_identical(clean_int(6L), 6L)
  expect_identical(clean_int(c(1L, 2L, 3L)), c(1L, 2L, 3L))
})

test_that("clean_int coerces whole-number doubles to integer", {
  expect_identical(clean_int(3), 3L)
  expect_identical(clean_int(c(3, 5, 7)), c(3L, 5L, 7L))
})

test_that("clean_int errors on non-whole doubles", {
  expect_error(clean_int(12.1))
  expect_error(clean_int(c(3, 5, 7.01)))
})

test_that("clean_int returns NULL for NULL input", {
  expect_null(clean_int(NULL))
})

test_that("clean_int errors on non-numeric input", {
  expect_error(clean_int("a"))
  expect_error(clean_int(TRUE))
})

# clean_posint ----
test_that("clean_posint returns positive integers", {
  expect_identical(clean_posint(5L), 5L)
  expect_identical(clean_posint(5), 5L)
  expect_identical(clean_posint(c(1, 2, 3)), c(1L, 2L, 3L))
})

test_that("clean_posint errors on zero or negative values", {
  expect_error(clean_posint(0))
  expect_error(clean_posint(-1))
  expect_error(clean_posint(c(1, -2, 3)))
})

test_that("clean_posint returns NULL for NULL input", {
  expect_null(clean_posint(NULL))
})

test_that("clean_posint errors on NAs by default", {
  expect_error(clean_posint(c(1, NA, 3)))
})

test_that("clean_posint allows NAs when allow_na = TRUE", {
  expect_identical(clean_posint(c(1, NA, 3), allow_na = TRUE), c(1L, 3L))
})
