test_that("property factories preserve vector and map defaults without warnings", {
  expect_silent(
    properties <- list(
      values = prop_string(default = c("a", "b"), vector = TRUE),
      mapping = prop_string(default = c(a = "1"), map = TRUE),
      bag = prop_bag(default = list(a = 1))
    )
  )
  Example <- S7::new_class(name = "Example", properties = properties)

  expect_identical(Example()@values, c("a", "b"))
  expect_identical(Example()@mapping, c(a = "1"))
  expect_identical(Example()@bag, list(a = 1))
  expect_identical(prop_spec(properties$values)$default, c("a", "b"))
})
