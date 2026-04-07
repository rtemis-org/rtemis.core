# test-test.R

# test_inherits ----
test_that("test_inherits returns TRUE for correct class", {
  expect_true(test_inherits("papaya", "character"))
  expect_true(test_inherits(1L, "integer"))
  expect_true(test_inherits(iris, "data.frame"))
})

test_that("test_inherits returns FALSE for incorrect class", {
  expect_message(result <- test_inherits(1.5, "integer"), "is not")
  expect_false(result)
  expect_message(result <- test_inherits(iris, "list"), "is not")
  expect_false(result)
})
