# test-prop_datetime_r.R

parse_json <- function(json) jsonlite::fromJSON(json, simplifyVector = FALSE)


# %% prop_datetime ----
test_that("prop_datetime() admits one finite POSIXct with class and tzone only", {
  p <- prop_datetime()
  ok <- as.POSIXct("2026-11-15 10:20:30", tz = "America/Los_Angeles")
  expect_null(validate_with_spec(ok, prop_spec(p)))
  expect_null(validate_with_spec(Sys.time(), prop_spec(p)))
  expect_match(
    validate_with_spec(as.Date("2026-11-15"), prop_spec(p)),
    "POSIXct"
  )
  expect_match(validate_with_spec(1.5, prop_spec(p)), "POSIXct")
  expect_match(
    validate_with_spec(c(ok, ok), prop_spec(p)),
    "single value"
  )
  expect_match(
    validate_with_spec(ok[NA_integer_], prop_spec(p)),
    "missing"
  )
  expect_match(
    validate_with_spec(
      structure(0, class = c("POSIXct", "POSIXt", "extra")),
      prop_spec(p)
    ),
    "further classes"
  )
  expect_match(
    validate_with_spec(structure(ok, label = "x"), prop_spec(p)),
    "attribute"
  )
  expect_match(
    validate_with_spec(.POSIXct(Inf, tz = "UTC"), prop_spec(p)),
    "year 1 to year 9999"
  )
  expect_match(
    validate_with_spec(.POSIXct(253402300800, tz = "UTC"), prop_spec(p)),
    "year 1 to year 9999"
  )
})

test_that("prop_datetime() takes a quoted default and no released arguments", {
  M <- S7::new_class(
    "M",
    properties = list(created = prop_datetime(quote(Sys.time())))
  )
  expect_s3_class(M()@created, "POSIXct")
  expect_error(
    prop_datetime(as.Date("2026-01-01")),
    class = "rtemis_value_error"
  )
  expect_error(prop_datetime(vector = TRUE), class = "rtemis_value_error")
  expect_null(prop_spec(prop_datetime(nullable = TRUE))[["default"]])
})

test_that("an array of date-times is a POSIXct vector with NA for a missing element", {
  p <- prop_array(prop_datetime(nullable = TRUE), min_items = 0L)
  x <- as.POSIXct(c("2026-01-01 00:00:00", NA), tz = "UTC")
  expect_null(validate_with_spec(x, prop_spec(p)))
  expect_match(
    validate_with_spec(list(x[[1L]]), prop_spec(p)),
    "POSIXct"
  )
  expect_match(
    validate_with_spec(x, prop_spec(prop_array(prop_datetime()))),
    "missing"
  )
})

test_that("a date-time is written in UTC as RFC 3339, rounded to the microsecond", {
  p <- prop_datetime()
  x <- as.POSIXct("2026-11-15 10:20:30.123456", tz = "America/Los_Angeles")
  expect_identical(prop_encode(p, x), "2026-11-15T18:20:30.123456Z")
  expect_identical(
    prop_encode(p, as.POSIXct("2026-11-15 10:20:30", tz = "UTC")),
    "2026-11-15T10:20:30Z"
  )
  expect_identical(
    prop_encode(p, as.POSIXct("2026-11-15 10:20:30.5", tz = "UTC")),
    "2026-11-15T10:20:30.5Z"
  )
  # A fraction that rounds to a whole second carries into the seconds.
  expect_identical(
    prop_encode(p, .POSIXct(0.9999996, tz = "UTC")),
    "1970-01-01T00:00:01Z"
  )
  expect_identical(
    prop_encode(p, .POSIXct(-62135596800, tz = "UTC")),
    "0001-01-01T00:00:00Z"
  )
  expect_identical(
    prop_encode(p, .POSIXct(-58979923200, tz = "UTC")),
    "0101-01-01T00:00:00Z"
  )
})

test_that("a date-time reads back in UTC within a microsecond, and its text is stable", {
  p <- prop_datetime()
  x <- as.POSIXct("2026-11-15 10:20:30.123456", tz = "America/Los_Angeles")
  back <- prop_decode(p, parse_json(to_JSON(prop_encode(p, x))))
  expect_identical(attr(back, "tzone"), "UTC")
  expect_lt(abs(as.double(back) - as.double(x)), 1e-6)
  now <- prop_encode(p, Sys.time())
  expect_identical(prop_encode(p, prop_decode(p, now)), now)
})

test_that("prop_decode() reads RFC 3339 offsets and lowercase separators", {
  p <- prop_datetime()
  utc <- as.POSIXct("2026-11-15 08:20:30", tz = "UTC")
  expect_equal(prop_decode(p, "2026-11-15T10:20:30+02:00"), utc)
  expect_equal(prop_decode(p, "2026-11-15T03:20:30-05:00"), utc)
  expect_equal(prop_decode(p, "2026-11-15t08:20:30z"), utc)
  expect_equal(
    prop_decode(p, "0001-01-01T00:00:00Z"),
    .POSIXct(-62135596800, tz = "UTC")
  )
  expect_equal(
    as.double(prop_decode(p, "2026-11-15T08:20:30.25Z")),
    as.double(utc) + 0.25
  )
})

test_that("prop_decode() rejects strings that are not RFC 3339 date-times", {
  p <- prop_datetime()
  for (bad in c(
    "2026-02-30T00:00:00Z",
    "2026-11-15 10:20:30Z",
    "2026-11-15T10:20:30",
    "2026-11-15T24:00:00Z",
    "2026-11-15T10:20:30+25:00",
    "15/11/2026"
  )) {
    expect_error(prop_decode(p, bad), class = "rtemis_value_error", label = bad)
  }
  expect_error(prop_decode(p, 1), class = "rtemis_type_error")
})

test_that("arrays, maps, and tables of date-times round-trip", {
  x <- as.POSIXct(
    c("2026-01-01 00:00:00", NA, "1970-01-01 00:00:01"),
    tz = "UTC"
  )
  a <- prop_array(prop_datetime(nullable = TRUE))
  json <- to_JSON(prop_encode(a, x))
  expect_identical(
    json,
    '["2026-01-01T00:00:00Z",null,"1970-01-01T00:00:01Z"]'
  )
  expect_equal(prop_decode(a, parse_json(json)), x)
  m <- prop_map(prop_datetime())
  y <- c(start = x[[1L]])
  expect_identical(
    to_JSON(prop_encode(m, y)),
    '{"start":"2026-01-01T00:00:00Z"}'
  )
  expect_equal(prop_decode(m, list(start = "2026-01-01T00:00:00Z")), y)
  t <- prop_table(
    list(when = prop_datetime(nullable = TRUE), n = prop_integer()),
    min_items = 0L
  )
  df <- data.frame(when = x, n = 1:3)
  json <- to_JSON(prop_encode(t, df))
  expect_identical(
    json,
    paste0(
      '[{"when":"2026-01-01T00:00:00Z","n":1},{"when":null,"n":2},',
      '{"when":"1970-01-01T00:00:01Z","n":3}]'
    )
  )
  expect_equal(prop_decode(t, parse_json(json)), df)
  required <- prop_table(list(when = prop_datetime()), min_items = 0L)
  empty <- prop_decode(required, list())
  expect_s3_class(empty[["when"]], "POSIXct")
  expect_identical(nrow(empty), 0L)
})

test_that("a date-time emits a string of format date-time", {
  M <- S7::new_class(
    "M",
    properties = list(
      created = prop_datetime(
        quote(Sys.time()),
        description = "Creation time."
      ),
      seen = prop_array(prop_datetime(nullable = TRUE), nullable = TRUE)
    )
  )
  schema <- class_to_JSONSchema(M, id = "https://example.org/m")
  expect_identical(
    schema[["properties"]][["created"]],
    list(
      type = "string",
      format = "date-time",
      description = "Creation time."
    )
  )
  expect_identical(
    schema[["properties"]][["seen"]][["items"]],
    list(type = c("string", "null"), format = "date-time")
  )
  expect_identical(
    prop_to_JSONSchema(prop_datetime(nullable = TRUE), new_JSONSchema_defs()),
    list(type = c("string", "null"), format = "date-time")
  )
})

test_that("the declaration of prop_datetime() rebuilds itself", {
  for (p in list(
    prop_datetime(),
    prop_datetime(quote(Sys.time()), description = "Creation time."),
    prop_datetime(nullable = TRUE, def_name = "Instant"),
    prop_array(prop_datetime(nullable = TRUE), min_items = 0L)
  )) {
    d <- prop_declaration(p)
    expect_identical(prop_declaration(prop_build(d)), d)
  }
  expect_identical(
    prop_declaration(prop_datetime())[["factory"]],
    "prop_datetime"
  )
})


# %% prop_r ----
test_that("prop_r() checks the class of an R value", {
  Tool <- S7::new_class(
    "Tool",
    properties = list(
      impl = prop_r(S7::class_function, nullable = TRUE),
      state = prop_r(S7::class_environment, default = quote(new.env()))
    )
  )
  expect_null(Tool()@impl)
  expect_identical(Tool(impl = sum)@impl, sum)
  expect_error(Tool(impl = "sum"), "function")
  expect_false(identical(Tool()@state, Tool()@state))
  Frame <- S7::new_class(
    "Frame",
    properties = list(x = prop_r(S7::class_data.frame | S7::class_list))
  )
  expect_identical(Frame(x = list(1))@x, list(1))
  expect_error(Frame(x = 1), "data.frame")
})

test_that("prop_r() takes an S7, S3, or base class", {
  expect_error(prop_r(NULL), class = "rtemis_type_error")
  expect_error(prop_r("function"), class = "rtemis_type_error")
  expect_error(prop_r(S7::class_function, TRUE), class = "rtemis_type_error")
  expect_s3_class(prop_r(S7::new_S3_class("lm")), "S7_property")
  Fit <- S7::new_class("Fit")
  expect_s3_class(prop_r(Fit), "S7_property")
})

test_that("only a property of a class takes a prop_r() declaration", {
  r <- prop_r(S7::class_function)
  expect_error(prop_array(r), class = "rtemis_type_error")
  expect_error(prop_map(r), class = "rtemis_type_error")
  expect_error(prop_struct(list(f = r)), class = "rtemis_type_error")
  expect_error(prop_union(list(r, prop_string())), class = "rtemis_type_error")
  Holder <- S7::new_class("Holder", properties = list(f = r))
  expect_error(
    prop_instance(Holder, members = list(f = r)),
    class = "rtemis_type_error"
  )
})

test_that("a prop_r() value has no JSON form", {
  r <- prop_r(S7::class_function, nullable = TRUE)
  Tool <- S7::new_class(
    "Tool",
    properties = list(name = prop_string("t"), impl = r)
  )
  expect_error(prop_encode(r, sum), class = "rtemis_value_error")
  expect_error(prop_decode(r, "sum"), class = "rtemis_value_error")
  expect_error(
    prop_to_JSONSchema(r, new_JSONSchema_defs()),
    class = "rtemis_value_error"
  )
  expect_error(
    class_to_JSONSchema(Tool, id = "https://example.org/tool"),
    "Property 'impl' of Tool holds an R value with no JSON form",
    class = "rtemis_value_error"
  )
  expect_error(
    prop_encode(prop_instance(Tool), Tool()),
    "Property 'impl' of Tool",
    class = "rtemis_value_error"
  )
  expect_error(
    prop_decode(prop_instance(Tool), list(impl = "sum")),
    "Property 'impl' of Tool",
    class = "rtemis_value_error"
  )
})

test_that("the declaration of prop_r() holds its class and rebuilds itself", {
  r <- prop_r(
    S7::class_function,
    nullable = TRUE,
    description = "Implementation."
  )
  d <- prop_declaration(r)
  expect_identical(d[["factory"]], "prop_r")
  expect_identical(d[["arguments"]][["cls"]], S7::class_function)
  expect_identical(prop_declaration(prop_build(d)), d)
  e <- prop_extend(r, "mypkg", list(role = "runtime"))
  expect_identical(prop_extension(e, "mypkg"), list(role = "runtime"))
})
