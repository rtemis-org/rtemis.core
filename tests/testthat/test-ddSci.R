# test-ddSci.R
# ::rtemis.core::
# 2026- EDG rtemis.org

test_that("ddSci() writes fixed decimals, or scientific notation for a value that rounds to zero", {
  x <- c(
    0.34876549,
    0.00000000457823,
    0,
    0.075,
    0.0049,
    -0.0032,
    1234567.8,
    0.999,
    1,
    12.345
  )
  expect_identical(
    ddSci(x),
    c(
      "0.35",
      "4.6e-09",
      "0.00",
      "0.07",
      "4.9e-03",
      "-3.2e-03",
      "1.2e+06",
      "1.00",
      "1.00",
      "12.35"
    )
  )
  # The test for scientific notation uses `decimal_places`.
  expect_identical(
    ddSci(x, 3),
    c(
      "0.349",
      "4.6e-09",
      "0.000",
      "0.075",
      "0.005",
      "-0.003",
      "1.23e+06",
      "0.999",
      "1.000",
      "12.345"
    )
  )
  expect_identical(
    ddSci(x, 1),
    c(
      "0.3",
      "4.6e-09",
      "0.0",
      "0.1",
      "4.9e-03",
      "-3.2e-03",
      "1e+06",
      "1.0",
      "1.0",
      "12.3"
    )
  )
  expect_identical(ddSci(0.0004, 3), "4e-04")
  expect_identical(ddSci(c(1, NA)), c("1.00", "NA"))
})
