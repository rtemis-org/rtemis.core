# test-00_S7_properties.R

library(S7)

# Define one test class per property so S7 validation fires on construction.
.TC <- local({
  list(
    chr = new_class("TC_chr", properties = list(x = character_scalar)),
    opt_chr = new_class(
      "TC_opt_chr",
      properties = list(x = optional_character_scalar)
    ),
    dbl = new_class("TC_dbl", properties = list(x = double_scalar)),
    opt_dbl = new_class(
      "TC_opt_dbl",
      properties = list(x = optional_double_scalar)
    ),
    int = new_class("TC_int", properties = list(x = integer_scalar)),
    opt_int = new_class(
      "TC_opt_int",
      properties = list(x = optional_integer_scalar)
    ),
    nng_int = new_class(
      "TC_nng_int",
      properties = list(x = nonneg_integer_scalar)
    ),
    opt_nng_int = new_class(
      "TC_opt_nng_int",
      properties = list(x = optional_nonneg_integer_scalar)
    ),
    lgl = new_class("TC_lgl", properties = list(x = logical_scalar)),
    opt_lgl = new_class(
      "TC_opt_lgl",
      properties = list(x = optional_logical_scalar)
    ),
    prob = new_class("TC_prob", properties = list(x = prob_scalar)),
    opt_prb = new_class(
      "TC_opt_prb",
      properties = list(x = optional_prob_scalar)
    ),
    unit_o = new_class("TC_unit_o", properties = list(x = unit_open_scalar)),
    pos_dbl = new_class("TC_pos_dbl", properties = list(x = pos_double_scalar)),
    opt_pos = new_class(
      "TC_opt_pos",
      properties = list(x = optional_pos_double_scalar)
    ),
    nng_dbl = new_class(
      "TC_nng_dbl",
      properties = list(x = nonneg_double_scalar)
    ),
    opt_nng = new_class(
      "TC_opt_nng",
      properties = list(x = optional_nonneg_double_scalar)
    ),
    pvec = new_class("TC_pvec", properties = list(x = prob_vector)),
    opt_pvec = new_class(
      "TC_opt_pvec",
      properties = list(x = optional_prob_vector)
    ),
    uovec = new_class("TC_uovec", properties = list(x = unit_open_vector)),
    opt_uov = new_class(
      "TC_opt_uov",
      properties = list(x = optional_unit_open_vector)
    ),
    posvec = new_class("TC_posvec", properties = list(x = pos_double_vector)),
    opt_posv = new_class(
      "TC_opt_posv",
      properties = list(x = optional_pos_double_vector)
    ),
    nngvec = new_class(
      "TC_nngvec",
      properties = list(x = nonneg_double_vector)
    ),
    opt_nngv = new_class(
      "TC_opt_nngv",
      properties = list(x = optional_nonneg_double_vector)
    )
  )
})

# character_scalar ----
test_that("character_scalar accepts a non-empty string", {
  expect_no_error(.TC$chr(x = "hello"))
})

test_that("character_scalar rejects empty string", {
  expect_error(.TC$chr(x = ""))
})

test_that("character_scalar rejects whitespace-only string", {
  expect_error(.TC$chr(x = "   "))
})

test_that("character_scalar rejects NA character", {
  expect_error(.TC$chr(x = NA_character_))
})

test_that("character_scalar rejects length > 1", {
  expect_error(.TC$chr(x = c("a", "b")))
})

# optional_character_scalar ----
test_that("optional_character_scalar accepts NULL", {
  expect_no_error(.TC$opt_chr(x = NULL))
})

test_that("optional_character_scalar accepts a non-empty string", {
  expect_no_error(.TC$opt_chr(x = "hello"))
})

test_that("optional_character_scalar rejects empty string", {
  expect_error(.TC$opt_chr(x = ""))
})

test_that("optional_character_scalar rejects NA character", {
  expect_error(.TC$opt_chr(x = NA_character_))
})

# double_scalar ----
test_that("double_scalar accepts a finite double", {
  expect_no_error(.TC$dbl(x = 3.14))
})

test_that("double_scalar accepts Inf", {
  expect_no_error(.TC$dbl(x = Inf))
})

test_that("double_scalar rejects NA", {
  expect_error(.TC$dbl(x = NA_real_))
})

test_that("double_scalar rejects length > 1", {
  expect_error(.TC$dbl(x = c(1.0, 2.0)))
})

# optional_double_scalar ----
test_that("optional_double_scalar accepts NULL", {
  expect_no_error(.TC$opt_dbl(x = NULL))
})

test_that("optional_double_scalar accepts a double", {
  expect_no_error(.TC$opt_dbl(x = -1.5))
})

test_that("optional_double_scalar rejects NA", {
  expect_error(.TC$opt_dbl(x = NA_real_))
})

# integer_scalar ----
test_that("integer_scalar accepts an integer", {
  expect_no_error(.TC$int(x = 5L))
})

test_that("integer_scalar rejects NA_integer_", {
  expect_error(.TC$int(x = NA_integer_))
})

test_that("integer_scalar rejects a plain double", {
  expect_error(.TC$int(x = 5.0))
})

# optional_integer_scalar ----
test_that("optional_integer_scalar accepts NULL", {
  expect_no_error(.TC$opt_int(x = NULL))
})

test_that("optional_integer_scalar accepts an integer", {
  expect_no_error(.TC$opt_int(x = 2L))
})

test_that("optional_integer_scalar rejects NA_integer_", {
  expect_error(.TC$opt_int(x = NA_integer_))
})

# nonneg_integer_scalar ----
test_that("nonneg_integer_scalar accepts 0L", {
  expect_no_error(.TC$nng_int(x = 0L))
})

test_that("nonneg_integer_scalar accepts a positive integer", {
  expect_no_error(.TC$nng_int(x = 5L))
})

test_that("nonneg_integer_scalar rejects a negative integer", {
  expect_error(.TC$nng_int(x = -1L))
})

test_that("nonneg_integer_scalar rejects NA_integer_", {
  expect_error(.TC$nng_int(x = NA_integer_))
})

test_that("nonneg_integer_scalar rejects a plain double", {
  expect_error(.TC$nng_int(x = 0.0))
})

test_that("nonneg_integer_scalar rejects length > 1", {
  expect_error(.TC$nng_int(x = c(0L, 1L)))
})

# optional_nonneg_integer_scalar ----
test_that("optional_nonneg_integer_scalar accepts NULL", {
  expect_no_error(.TC$opt_nng_int(x = NULL))
})

test_that("optional_nonneg_integer_scalar accepts 0L", {
  expect_no_error(.TC$opt_nng_int(x = 0L))
})

test_that("optional_nonneg_integer_scalar accepts a positive integer", {
  expect_no_error(.TC$opt_nng_int(x = 3L))
})

test_that("optional_nonneg_integer_scalar rejects a negative integer", {
  expect_error(.TC$opt_nng_int(x = -1L))
})

test_that("optional_nonneg_integer_scalar rejects NA_integer_", {
  expect_error(.TC$opt_nng_int(x = NA_integer_))
})

# logical_scalar ----
test_that("logical_scalar accepts TRUE", {
  expect_no_error(.TC$lgl(x = TRUE))
})

test_that("logical_scalar accepts FALSE", {
  expect_no_error(.TC$lgl(x = FALSE))
})

test_that("logical_scalar rejects NA", {
  expect_error(.TC$lgl(x = NA))
})

test_that("logical_scalar rejects length > 1", {
  expect_error(.TC$lgl(x = c(TRUE, FALSE)))
})

test_that("logical_scalar rejects integer 1L", {
  expect_error(.TC$lgl(x = 1L))
})

# optional_logical_scalar ----
test_that("optional_logical_scalar accepts NULL", {
  expect_no_error(.TC$opt_lgl(x = NULL))
})

test_that("optional_logical_scalar accepts FALSE", {
  expect_no_error(.TC$opt_lgl(x = FALSE))
})

test_that("optional_logical_scalar rejects NA", {
  expect_error(.TC$opt_lgl(x = NA))
})

# prob_scalar ----
test_that("prob_scalar accepts 0", {
  expect_no_error(.TC$prob(x = 0))
})

test_that("prob_scalar accepts 0.5", {
  expect_no_error(.TC$prob(x = 0.5))
})

test_that("prob_scalar accepts 1", {
  expect_no_error(.TC$prob(x = 1))
})

test_that("prob_scalar rejects value > 1", {
  expect_error(.TC$prob(x = 1.01))
})

test_that("prob_scalar rejects negative value", {
  expect_error(.TC$prob(x = -0.01))
})

test_that("prob_scalar rejects NA", {
  expect_error(.TC$prob(x = NA_real_))
})

# optional_prob_scalar ----
test_that("optional_prob_scalar accepts NULL", {
  expect_no_error(.TC$opt_prb(x = NULL))
})

test_that("optional_prob_scalar accepts 0.5", {
  expect_no_error(.TC$opt_prb(x = 0.5))
})

test_that("optional_prob_scalar rejects value > 1", {
  expect_error(.TC$opt_prb(x = 2.0))
})

# unit_open_scalar ----
test_that("unit_open_scalar accepts strictly interior value", {
  expect_no_error(.TC$unit_o(x = 0.5))
})

test_that("unit_open_scalar rejects 0", {
  expect_error(.TC$unit_o(x = 0))
})

test_that("unit_open_scalar rejects 1", {
  expect_error(.TC$unit_o(x = 1))
})

test_that("unit_open_scalar rejects value outside (0, 1)", {
  expect_error(.TC$unit_o(x = 1.5))
})

# pos_double_scalar ----
test_that("pos_double_scalar accepts positive value", {
  expect_no_error(.TC$pos_dbl(x = 0.001))
})

test_that("pos_double_scalar rejects 0", {
  expect_error(.TC$pos_dbl(x = 0))
})

test_that("pos_double_scalar rejects negative value", {
  expect_error(.TC$pos_dbl(x = -1.0))
})

test_that("pos_double_scalar rejects Inf", {
  expect_error(.TC$pos_dbl(x = Inf))
})

# optional_pos_double_scalar ----
test_that("optional_pos_double_scalar accepts NULL", {
  expect_no_error(.TC$opt_pos(x = NULL))
})

test_that("optional_pos_double_scalar accepts positive value", {
  expect_no_error(.TC$opt_pos(x = 2.5))
})

test_that("optional_pos_double_scalar rejects 0", {
  expect_error(.TC$opt_pos(x = 0))
})

# nonneg_double_scalar ----
test_that("nonneg_double_scalar accepts 0", {
  expect_no_error(.TC$nng_dbl(x = 0))
})

test_that("nonneg_double_scalar accepts positive value", {
  expect_no_error(.TC$nng_dbl(x = 5.0))
})

test_that("nonneg_double_scalar rejects negative value", {
  expect_error(.TC$nng_dbl(x = -0.001))
})

test_that("nonneg_double_scalar rejects Inf", {
  expect_error(.TC$nng_dbl(x = Inf))
})

# optional_nonneg_double_scalar ----
test_that("optional_nonneg_double_scalar accepts NULL", {
  expect_no_error(.TC$opt_nng(x = NULL))
})

test_that("optional_nonneg_double_scalar accepts 0", {
  expect_no_error(.TC$opt_nng(x = 0))
})

test_that("optional_nonneg_double_scalar rejects negative value", {
  expect_error(.TC$opt_nng(x = -1.0))
})

# bounded_double_property ----
test_that("bounded_double_property closed interval accepts endpoints", {
  TC <- new_class(
    "TC_bnd1",
    properties = list(x = bounded_double_property(0, 10))
  )
  expect_no_error(TC(x = 0))
  expect_no_error(TC(x = 10))
  expect_no_error(TC(x = 5))
})

test_that("bounded_double_property closed interval rejects outside", {
  TC <- new_class(
    "TC_bnd2",
    properties = list(x = bounded_double_property(0, 10))
  )
  expect_error(TC(x = -0.1))
  expect_error(TC(x = 10.1))
})

test_that("bounded_double_property lower_open rejects lower endpoint", {
  TC <- new_class(
    "TC_bnd3",
    properties = list(x = bounded_double_property(0, 1, lower_open = TRUE))
  )
  expect_error(TC(x = 0))
  expect_no_error(TC(x = 0.001))
  expect_no_error(TC(x = 1))
})

test_that("bounded_double_property upper_open rejects upper endpoint", {
  TC <- new_class(
    "TC_bnd4",
    properties = list(x = bounded_double_property(0, 1, upper_open = TRUE))
  )
  expect_no_error(TC(x = 0))
  expect_error(TC(x = 1))
})

test_that("bounded_double_property nullable accepts NULL", {
  TC <- new_class(
    "TC_bnd5",
    properties = list(x = bounded_double_property(0, 1, nullable = TRUE))
  )
  expect_no_error(TC(x = NULL))
  expect_no_error(TC(x = 0.5))
  expect_error(TC(x = 2.0))
})

test_that("bounded_double_property rejects Inf", {
  TC <- new_class(
    "TC_bnd6",
    properties = list(x = bounded_double_property(0, 100))
  )
  expect_error(TC(x = Inf))
})

# prob_vector ----
test_that("prob_vector accepts valid vector", {
  expect_no_error(.TC$pvec(x = c(0, 0.5, 1)))
})

test_that("prob_vector rejects empty vector", {
  expect_error(.TC$pvec(x = double(0)))
})

test_that("prob_vector rejects NA", {
  expect_error(.TC$pvec(x = c(0.5, NA)))
})

test_that("prob_vector rejects out-of-range values", {
  expect_error(.TC$pvec(x = c(0.5, 1.5)))
  expect_error(.TC$pvec(x = c(-0.1, 0.5)))
})

# optional_prob_vector ----
test_that("optional_prob_vector accepts NULL", {
  expect_no_error(.TC$opt_pvec(x = NULL))
})

test_that("optional_prob_vector accepts valid vector", {
  expect_no_error(.TC$opt_pvec(x = c(0.2, 0.8)))
})

test_that("optional_prob_vector rejects out-of-range values", {
  expect_error(.TC$opt_pvec(x = c(0.5, 2)))
})

# unit_open_vector ----
test_that("unit_open_vector accepts strictly interior values", {
  expect_no_error(.TC$uovec(x = c(0.1, 0.5, 0.9)))
})

test_that("unit_open_vector rejects 0", {
  expect_error(.TC$uovec(x = c(0, 0.5)))
})

test_that("unit_open_vector rejects 1", {
  expect_error(.TC$uovec(x = c(0.5, 1)))
})

test_that("unit_open_vector rejects empty vector", {
  expect_error(.TC$uovec(x = double(0)))
})

test_that("unit_open_vector rejects NA", {
  expect_error(.TC$uovec(x = c(0.5, NA)))
})

# optional_unit_open_vector ----
test_that("optional_unit_open_vector accepts NULL", {
  expect_no_error(.TC$opt_uov(x = NULL))
})

test_that("optional_unit_open_vector accepts valid vector", {
  expect_no_error(.TC$opt_uov(x = c(0.2, 0.8)))
})

test_that("optional_unit_open_vector rejects endpoints", {
  expect_error(.TC$opt_uov(x = c(0, 0.5)))
  expect_error(.TC$opt_uov(x = c(0.5, 1)))
})

# pos_double_vector ----
test_that("pos_double_vector accepts positive values", {
  expect_no_error(.TC$posvec(x = c(0.001, 1, 100)))
})

test_that("pos_double_vector rejects 0", {
  expect_error(.TC$posvec(x = c(0, 1)))
})

test_that("pos_double_vector rejects negative values", {
  expect_error(.TC$posvec(x = c(-1, 1)))
})

test_that("pos_double_vector rejects Inf", {
  expect_error(.TC$posvec(x = c(1, Inf)))
})

test_that("pos_double_vector rejects empty vector", {
  expect_error(.TC$posvec(x = double(0)))
})

test_that("pos_double_vector rejects NA", {
  expect_error(.TC$posvec(x = c(1, NA)))
})

# optional_pos_double_vector ----
test_that("optional_pos_double_vector accepts NULL", {
  expect_no_error(.TC$opt_posv(x = NULL))
})

test_that("optional_pos_double_vector accepts positive values", {
  expect_no_error(.TC$opt_posv(x = c(0.5, 2)))
})

test_that("optional_pos_double_vector rejects 0", {
  expect_error(.TC$opt_posv(x = c(0, 1)))
})

# nonneg_double_vector ----
test_that("nonneg_double_vector accepts 0 and positive values", {
  expect_no_error(.TC$nngvec(x = c(0, 1, 2.5)))
})

test_that("nonneg_double_vector rejects negative values", {
  expect_error(.TC$nngvec(x = c(-1, 0, 1)))
})

test_that("nonneg_double_vector rejects Inf", {
  expect_error(.TC$nngvec(x = c(1, Inf)))
})

test_that("nonneg_double_vector rejects empty vector", {
  expect_error(.TC$nngvec(x = double(0)))
})

test_that("nonneg_double_vector rejects NA", {
  expect_error(.TC$nngvec(x = c(0, NA)))
})

# optional_nonneg_double_vector ----
test_that("optional_nonneg_double_vector accepts NULL", {
  expect_no_error(.TC$opt_nngv(x = NULL))
})

test_that("optional_nonneg_double_vector accepts 0 and positive values", {
  expect_no_error(.TC$opt_nngv(x = c(0, 1, 5)))
})

test_that("optional_nonneg_double_vector rejects negative values", {
  expect_error(.TC$opt_nngv(x = c(-1, 0)))
})
