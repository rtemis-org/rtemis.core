# test-check.R

# check_inherits ----
test_that("check_inherits passes for correct class", {
  expect_invisible(check_inherits("papaya", "character"))
  expect_invisible(check_inherits(1L, "integer"))
})

test_that("check_inherits errors for wrong class", {
  expect_error(check_inherits(1.5, "integer", allow_null = FALSE))
  expect_error(check_inherits(iris, "list", allow_null = FALSE))
})

test_that("check_inherits allows NULL by default", {
  expect_invisible(check_inherits(NULL, "character"))
})

test_that("check_inherits errors on NULL when allow_null = FALSE", {
  expect_error(check_inherits(NULL, "character", allow_null = FALSE))
})

# check_logical ----
test_that("check_logical passes for logical input", {
  expect_invisible(check_logical(TRUE))
  expect_invisible(check_logical(c(TRUE, FALSE)))
})

test_that("check_logical errors for non-logical input", {
  expect_error(check_logical(1, allow_null = FALSE))
  expect_error(check_logical("true", allow_null = FALSE))
})

test_that("check_logical errors on NAs", {
  expect_error(check_logical(c(TRUE, NA)))
})

test_that("check_logical allows NULL by default", {
  expect_invisible(check_logical(NULL))
})

test_that("check_logical errors on NULL when allow_null = FALSE", {
  expect_error(check_logical(NULL, allow_null = FALSE))
})

# check_character ----
test_that("check_character passes for character input", {
  expect_invisible(check_character("hello"))
  expect_invisible(check_character(c("a", "b")))
})

test_that("check_character errors for non-character input", {
  expect_error(check_character(1, allow_null = FALSE))
  expect_error(check_character(TRUE, allow_null = FALSE))
})

test_that("check_character errors on NAs", {
  expect_error(check_character(c("a", NA)))
})

test_that("check_character allows NULL by default", {
  expect_invisible(check_character(NULL))
})

# check_floatpos ----
test_that("check_floatpos passes for positive numeric", {
  expect_invisible(check_floatpos(1.5))
  expect_invisible(check_floatpos(c(0.1, 2, 3.5)))
})

test_that("check_floatpos errors on zero or negative values", {
  expect_error(check_floatpos(0, allow_null = FALSE))
  expect_error(check_floatpos(-1, allow_null = FALSE))
})

test_that("check_floatpos errors on non-numeric", {
  expect_error(check_floatpos("a", allow_null = FALSE))
})

test_that("check_floatpos errors on NAs", {
  expect_error(check_floatpos(c(1, NA)))
})

test_that("check_floatpos allows NULL by default", {
  expect_invisible(check_floatpos(NULL))
})

# check_float01exc ----
test_that("check_float01exc passes for values in (0, 1)", {
  expect_invisible(check_float01exc(0.5))
  expect_invisible(check_float01exc(c(0.1, 0.5, 0.9)))
})

test_that("check_float01exc errors on boundaries", {
  expect_error(check_float01exc(0, allow_null = FALSE))
  expect_error(check_float01exc(1, allow_null = FALSE))
})

test_that("check_float01exc errors outside range", {
  expect_error(check_float01exc(-0.1, allow_null = FALSE))
  expect_error(check_float01exc(1.1, allow_null = FALSE))
})

# check_float01inc ----
test_that("check_float01inc passes for values in [0, 1]", {
  expect_invisible(check_float01inc(0))
  expect_invisible(check_float01inc(1))
  expect_invisible(check_float01inc(0.5))
  expect_invisible(check_float01inc(c(0, 0.5, 1)))
})

test_that("check_float01inc errors outside range", {
  expect_error(check_float01inc(-0.1, allow_null = FALSE))
  expect_error(check_float01inc(1.1, allow_null = FALSE))
})

test_that("check_float01inc errors on non-numeric", {
  expect_error(check_float01inc("a", allow_null = FALSE))
})

# check_floatpos1 ----
test_that("check_floatpos1 passes for values in (0, 1]", {
  expect_invisible(check_floatpos1(1))
  expect_invisible(check_floatpos1(0.5))
  expect_invisible(check_floatpos1(c(0.1, 0.5, 1)))
})

test_that("check_floatpos1 errors on zero and values > 1", {
  expect_error(check_floatpos1(0, allow_null = FALSE))
  expect_error(check_floatpos1(1.1, allow_null = FALSE))
})

# check_float0pos ----
test_that("check_float0pos passes for non-negative values", {
  expect_invisible(check_float0pos(0))
  expect_invisible(check_float0pos(1.5))
  expect_invisible(check_float0pos(c(0, 1, 2.5)))
})

test_that("check_float0pos errors on negative values", {
  expect_error(check_float0pos(-0.1, allow_null = FALSE))
  expect_error(check_float0pos(c(1, -1), allow_null = FALSE))
})

# check_float_neg1_1 ----
test_that("check_float_neg1_1 passes for values in [-1, 1]", {
  expect_invisible(check_float_neg1_1(-1))
  expect_invisible(check_float_neg1_1(0))
  expect_invisible(check_float_neg1_1(1))
  expect_invisible(check_float_neg1_1(c(-1, 0, 0.5, 1)))
})

test_that("check_float_neg1_1 errors outside range", {
  expect_error(check_float_neg1_1(-1.1, allow_null = FALSE))
  expect_error(check_float_neg1_1(1.1, allow_null = FALSE))
})

# check_dependencies ----
test_that("check_dependencies passes for installed packages", {
  expect_invisible(check_dependencies("base"))
})

# check_data.table ----
test_that("check_data.table passes for data.table", {
  dt <- data.table::data.table(a = 1:3)
  expect_invisible(check_data.table(dt))
})

test_that("check_data.table errors for non-data.table", {
  expect_error(check_data.table(iris))
  expect_error(check_data.table(list(a = 1)))
})

# check_tabular ----
test_that("check_tabular passes for tabular objects", {
  expect_invisible(check_tabular(iris))
  expect_invisible(check_tabular(data.table::data.table(a = 1)))
})

test_that("check_tabular errors for non-tabular objects", {
  expect_error(check_tabular(list(a = 1)))
  expect_error(check_tabular(matrix(1:4, 2, 2)))
})

# check_prob_vector ----
test_that("check_prob_vector passes for valid vector", {
  expect_invisible(check_prob_vector(c(0, 0.5, 1)))
})

test_that("check_prob_vector rejects empty vector", {
  expect_error(check_prob_vector(double(0)))
})

test_that("check_prob_vector rejects NA", {
  expect_error(check_prob_vector(c(0.5, NA)))
})

test_that("check_prob_vector rejects out-of-range values", {
  expect_error(check_prob_vector(c(0.5, 1.5)))
  expect_error(check_prob_vector(c(-0.1, 0.5)))
})

test_that("check_prob_vector rejects non-numeric", {
  expect_error(check_prob_vector("0.5"))
})

test_that("check_optional_prob_vector accepts NULL", {
  expect_invisible(check_optional_prob_vector(NULL))
})

test_that("check_optional_prob_vector passes for valid vector", {
  expect_invisible(check_optional_prob_vector(c(0.2, 0.8)))
})

test_that("check_optional_prob_vector rejects out-of-range values", {
  expect_error(check_optional_prob_vector(c(0.5, 2)))
})

# check_unit_open_vector ----
test_that("check_unit_open_vector passes for strictly interior values", {
  expect_invisible(check_unit_open_vector(c(0.1, 0.5, 0.9)))
})

test_that("check_unit_open_vector rejects 0", {
  expect_error(check_unit_open_vector(c(0, 0.5)))
})

test_that("check_unit_open_vector rejects 1", {
  expect_error(check_unit_open_vector(c(0.5, 1)))
})

test_that("check_unit_open_vector rejects empty vector", {
  expect_error(check_unit_open_vector(double(0)))
})

test_that("check_unit_open_vector rejects NA", {
  expect_error(check_unit_open_vector(c(0.5, NA)))
})

test_that("check_optional_unit_open_vector accepts NULL", {
  expect_invisible(check_optional_unit_open_vector(NULL))
})

test_that("check_optional_unit_open_vector passes for valid vector", {
  expect_invisible(check_optional_unit_open_vector(c(0.2, 0.8)))
})

test_that("check_optional_unit_open_vector rejects endpoints", {
  expect_error(check_optional_unit_open_vector(c(0, 0.5)))
  expect_error(check_optional_unit_open_vector(c(0.5, 1)))
})

# check_pos_double_vector ----
test_that("check_pos_double_vector passes for positive values", {
  expect_invisible(check_pos_double_vector(c(0.001, 1, 100)))
})

test_that("check_pos_double_vector rejects 0", {
  expect_error(check_pos_double_vector(c(0, 1)))
})

test_that("check_pos_double_vector rejects negative values", {
  expect_error(check_pos_double_vector(c(-1, 1)))
})

test_that("check_pos_double_vector rejects Inf", {
  expect_error(check_pos_double_vector(c(1, Inf)))
})

test_that("check_pos_double_vector rejects empty vector", {
  expect_error(check_pos_double_vector(double(0)))
})

test_that("check_pos_double_vector rejects NA", {
  expect_error(check_pos_double_vector(c(1, NA)))
})

test_that("check_optional_pos_double_vector accepts NULL", {
  expect_invisible(check_optional_pos_double_vector(NULL))
})

test_that("check_optional_pos_double_vector passes for positive values", {
  expect_invisible(check_optional_pos_double_vector(c(0.5, 2)))
})

test_that("check_optional_pos_double_vector rejects 0", {
  expect_error(check_optional_pos_double_vector(c(0, 1)))
})

# check_nonneg_double_vector ----
test_that("check_nonneg_double_vector passes for 0 and positive values", {
  expect_invisible(check_nonneg_double_vector(c(0, 1, 2.5)))
})

test_that("check_nonneg_double_vector rejects negative values", {
  expect_error(check_nonneg_double_vector(c(-1, 0, 1)))
})

test_that("check_nonneg_double_vector rejects Inf", {
  expect_error(check_nonneg_double_vector(c(1, Inf)))
})

test_that("check_nonneg_double_vector rejects empty vector", {
  expect_error(check_nonneg_double_vector(double(0)))
})

test_that("check_nonneg_double_vector rejects NA", {
  expect_error(check_nonneg_double_vector(c(0, NA)))
})

test_that("check_optional_nonneg_double_vector accepts NULL", {
  expect_invisible(check_optional_nonneg_double_vector(NULL))
})

test_that("check_optional_nonneg_double_vector passes for valid values", {
  expect_invisible(check_optional_nonneg_double_vector(c(0, 1, 5)))
})

test_that("check_optional_nonneg_double_vector rejects negative values", {
  expect_error(check_optional_nonneg_double_vector(c(-1, 0)))
})

# check_pos_integer_scalar ----
test_that("check_pos_integer_scalar passes for positive integers", {
  expect_invisible(check_pos_integer_scalar(1L))
  expect_invisible(check_pos_integer_scalar(10))
})

test_that("check_pos_integer_scalar rejects 0", {
  expect_error(check_pos_integer_scalar(0L))
})

test_that("check_pos_integer_scalar rejects negative values", {
  expect_error(check_pos_integer_scalar(-1L))
})

test_that("check_pos_integer_scalar rejects non-whole numbers", {
  expect_error(check_pos_integer_scalar(1.5))
})

test_that("check_pos_integer_scalar rejects NA", {
  expect_error(check_pos_integer_scalar(NA_integer_))
})

test_that("check_optional_pos_integer_scalar accepts NULL", {
  expect_invisible(check_optional_pos_integer_scalar(NULL))
})

test_that("check_optional_pos_integer_scalar passes for valid values", {
  expect_invisible(check_optional_pos_integer_scalar(5L))
})

test_that("check_optional_pos_integer_scalar rejects 0", {
  expect_error(check_optional_pos_integer_scalar(0L))
})

# check_character_scalar ----
test_that("check_character_scalar passes for non-empty string", {
  expect_invisible(check_character_scalar("hello"))
})

test_that("check_character_scalar rejects empty string", {
  expect_error(check_character_scalar(""))
})

test_that("check_character_scalar rejects NA", {
  expect_error(check_character_scalar(NA_character_))
})

test_that("check_character_scalar rejects length > 1", {
  expect_error(check_character_scalar(c("a", "b")))
})

test_that("check_character_scalar rejects non-character", {
  expect_error(check_character_scalar(1))
})

test_that("check_optional_character_scalar accepts NULL", {
  expect_invisible(check_optional_character_scalar(NULL))
})

test_that("check_optional_character_scalar passes for non-empty string", {
  expect_invisible(check_optional_character_scalar("hello"))
})

test_that("check_optional_character_scalar rejects empty string", {
  expect_error(check_optional_character_scalar(""))
})

# check_double_scalar ----
test_that("check_double_scalar passes for single numeric", {
  expect_invisible(check_double_scalar(3.14))
  expect_invisible(check_double_scalar(1L))
})

test_that("check_double_scalar rejects NA", {
  expect_error(check_double_scalar(NA_real_))
})

test_that("check_double_scalar rejects length > 1", {
  expect_error(check_double_scalar(c(1.0, 2.0)))
})

test_that("check_double_scalar rejects non-numeric", {
  expect_error(check_double_scalar("a"))
})

test_that("check_optional_double_scalar accepts NULL", {
  expect_invisible(check_optional_double_scalar(NULL))
})

test_that("check_optional_double_scalar passes for single numeric", {
  expect_invisible(check_optional_double_scalar(2.5))
})

test_that("check_optional_double_scalar rejects NA", {
  expect_error(check_optional_double_scalar(NA_real_))
})

# check_integer_scalar ----
test_that("check_integer_scalar passes for whole numbers", {
  expect_invisible(check_integer_scalar(5L))
  expect_invisible(check_integer_scalar(100))
})

test_that("check_integer_scalar rejects non-whole number", {
  expect_error(check_integer_scalar(1.5))
})

test_that("check_integer_scalar rejects NA", {
  expect_error(check_integer_scalar(NA_integer_))
})

test_that("check_integer_scalar rejects Inf", {
  expect_error(check_integer_scalar(Inf))
  expect_error(check_integer_scalar(-Inf))
})

test_that("check_optional_integer_scalar accepts NULL", {
  expect_invisible(check_optional_integer_scalar(NULL))
})

test_that("check_optional_integer_scalar passes for whole number", {
  expect_invisible(check_optional_integer_scalar(10L))
})

test_that("check_optional_integer_scalar rejects non-whole number", {
  expect_error(check_optional_integer_scalar(1.5))
})

# check_logical_scalar ----
test_that("check_logical_scalar passes for TRUE and FALSE", {
  expect_invisible(check_logical_scalar(TRUE))
  expect_invisible(check_logical_scalar(FALSE))
})

test_that("check_logical_scalar rejects NA", {
  expect_error(check_logical_scalar(NA))
})

test_that("check_logical_scalar rejects non-logical", {
  expect_error(check_logical_scalar(1L))
})

test_that("check_logical_scalar rejects length > 1", {
  expect_error(check_logical_scalar(c(TRUE, FALSE)))
})

test_that("check_optional_logical_scalar accepts NULL", {
  expect_invisible(check_optional_logical_scalar(NULL))
})

test_that("check_optional_logical_scalar passes for FALSE", {
  expect_invisible(check_optional_logical_scalar(FALSE))
})

test_that("check_optional_logical_scalar rejects NA", {
  expect_error(check_optional_logical_scalar(NA))
})

# check_prob_scalar ----
test_that("check_prob_scalar passes for values in [0, 1]", {
  expect_invisible(check_prob_scalar(0))
  expect_invisible(check_prob_scalar(0.5))
  expect_invisible(check_prob_scalar(1))
})

test_that("check_prob_scalar rejects values outside [0, 1]", {
  expect_error(check_prob_scalar(1.5))
  expect_error(check_prob_scalar(-0.1))
})

test_that("check_prob_scalar rejects NA", {
  expect_error(check_prob_scalar(NA_real_))
})

test_that("check_optional_prob_scalar accepts NULL", {
  expect_invisible(check_optional_prob_scalar(NULL))
})

test_that("check_optional_prob_scalar passes for 0.5", {
  expect_invisible(check_optional_prob_scalar(0.5))
})

test_that("check_optional_prob_scalar rejects values > 1", {
  expect_error(check_optional_prob_scalar(2.0))
})

# check_unit_open_scalar ----
test_that("check_unit_open_scalar passes for strictly interior values", {
  expect_invisible(check_unit_open_scalar(0.5))
  expect_invisible(check_unit_open_scalar(0.001))
})

test_that("check_unit_open_scalar rejects 0 and 1", {
  expect_error(check_unit_open_scalar(0))
  expect_error(check_unit_open_scalar(1))
})

test_that("check_unit_open_scalar rejects NA", {
  expect_error(check_unit_open_scalar(NA_real_))
})

# check_pos_double_scalar ----
test_that("check_pos_double_scalar passes for positive finite values", {
  expect_invisible(check_pos_double_scalar(0.001))
  expect_invisible(check_pos_double_scalar(100))
})

test_that("check_pos_double_scalar rejects 0 and negative values", {
  expect_error(check_pos_double_scalar(0))
  expect_error(check_pos_double_scalar(-1))
})

test_that("check_pos_double_scalar rejects Inf", {
  expect_error(check_pos_double_scalar(Inf))
})

test_that("check_pos_double_scalar rejects NA", {
  expect_error(check_pos_double_scalar(NA_real_))
})

test_that("check_optional_pos_double_scalar accepts NULL", {
  expect_invisible(check_optional_pos_double_scalar(NULL))
})

test_that("check_optional_pos_double_scalar passes for positive value", {
  expect_invisible(check_optional_pos_double_scalar(2.5))
})

test_that("check_optional_pos_double_scalar rejects 0", {
  expect_error(check_optional_pos_double_scalar(0))
})

# check_nonneg_double_scalar ----
test_that("check_nonneg_double_scalar passes for 0 and positive values", {
  expect_invisible(check_nonneg_double_scalar(0))
  expect_invisible(check_nonneg_double_scalar(5))
})

test_that("check_nonneg_double_scalar rejects negative values", {
  expect_error(check_nonneg_double_scalar(-0.001))
})

test_that("check_nonneg_double_scalar rejects Inf", {
  expect_error(check_nonneg_double_scalar(Inf))
})

test_that("check_nonneg_double_scalar rejects NA", {
  expect_error(check_nonneg_double_scalar(NA_real_))
})

test_that("check_optional_nonneg_double_scalar accepts NULL", {
  expect_invisible(check_optional_nonneg_double_scalar(NULL))
})

test_that("check_optional_nonneg_double_scalar passes for 0", {
  expect_invisible(check_optional_nonneg_double_scalar(0))
})

test_that("check_optional_nonneg_double_scalar rejects negative values", {
  expect_error(check_optional_nonneg_double_scalar(-1))
})
