test_that("property factories preserve vector and map defaults without warnings", {
  expect_silent(
    properties <- list(
      enabled = prop_boolean(default = quote(!FALSE)),
      counts = prop_integer(default = quote(c(1L, 2L)), vector = TRUE),
      limits = prop_float(default = quote(c(0, 1)), vector = TRUE),
      integer_limits = prop_float(default = quote(c(0L, 1L)), vector = TRUE),
      values = prop_string(default = quote(c("a", "b")), vector = TRUE),
      mapping = prop_string(default = quote(c(a = "1")), map = TRUE),
      bag = prop_bag(default = quote(list(a = 1))),
      optional = prop_string(
        default = quote({
          NULL
        }),
        nullable = TRUE
      )
    )
  )
  Example <- S7::new_class(name = "Example", properties = properties)

  expect_identical(Example()@enabled, TRUE)
  expect_identical(Example()@counts, c(1L, 2L))
  expect_identical(Example()@limits, c(0, 1))
  expect_identical(Example()@integer_limits, c(0L, 1L))
  expect_identical(Example()@values, c("a", "b"))
  expect_identical(Example()@mapping, c(a = "1"))
  expect_identical(Example()@bag, list(a = 1))
  expect_null(Example()@optional)
  expect_identical(prop_spec(properties$values)$default, c("a", "b"))
  expect_identical(prop_spec(properties$mapping)$default, c(a = "1"))
  expect_identical(prop_spec(properties$bag)$default, list(a = 1))
})

test_that("quoted defaults are evaluated for each construction", {
  evaluations <- 0L
  make_bag <- function() {
    evaluations <<- evaluations + 1L
    list(state = new.env(parent = emptyenv()))
  }
  property <- prop_bag(default = quote(make_bag()))
  Example <- S7::new_class(name = "Example", properties = list(bag = property))

  expect_identical(evaluations, 1L)
  first <- Example()@bag$state
  second <- Example()@bag$state
  expect_identical(evaluations, 3L)
  first$value <- "first"
  expect_identical(as.list(second), list())
  expect_identical(as.list(prop_spec(property)$default$state), list())
})

test_that("quoted defaults are validated at declaration", {
  expect_snapshot(
    error = TRUE,
    suppressMessages(prop_float(
      default = quote(c(0, 2)),
      vector = TRUE,
      max = 1
    ))
  )
  expect_snapshot(
    error = TRUE,
    suppressMessages(prop_integer(default = quote(as.numeric(1))))
  )
  expect_snapshot(
    error = TRUE,
    suppressMessages(prop_bag(default = quote(list(a = 1, a = 2))))
  )
  expect_snapshot(
    error = TRUE,
    suppressMessages(prop_string(
      default = quote({
        NULL
      })
    ))
  )
})
