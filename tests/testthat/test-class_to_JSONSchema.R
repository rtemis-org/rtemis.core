# test-class_to_JSONSchema.R

library(S7)

# %% Fixtures ----
.CS <- local({
  positions <- prop_integer(
    min = 1L,
    vector = TRUE,
    min_items = 0L,
    unique_items = TRUE,
    def_name = "positionIndex",
    description = "1-based positions."
  )
  ranges <- prop_matrix(
    prop_integer(min = 1L),
    ncol = 2L,
    min_items = 0L,
    def_name = "rangeIndex",
    description = "[start, end] ranges."
  )
  Entry <- new_class(
    "CS_Entry",
    properties = list(
      index = prop_union(list(positions, ranges), def_name = "mixedIndex"),
      type = prop_string(default = "", allow_empty = TRUE)
    )
  )
  Meta <- new_class(
    "CS_Meta",
    properties = list(organism = prop_string(default = "", allow_empty = TRUE))
  )
  Doc <- new_class(
    "CS_Doc",
    properties = list(
      sequence = prop_string(pattern = "^[A-Za-z*]+$", min_length = 2L),
      entries = prop_collection(Entry, container = "map", default = list()),
      variants = prop_array(
        prop_struct(
          members = list(position = prop_integer(min = 1L)),
          required = "position",
          additional = TRUE
        ),
        default = list(),
        min_items = 0L
      ),
      metadata = prop_object(Meta, default = quote(Meta())),
      note = prop_string(nullable = TRUE)
    )
  )
  list(Doc = Doc)
})

.cs_schema <- function() {
  class_to_JSONSchema(
    .CS$Doc,
    id = "https://example.org/doc/v1/schema.json",
    title = "Doc",
    description = "A test document.",
    envelope = list(version = prop_const("1.0.0")),
    required = "version"
  )
}


# %% Document ----
test_that("class_to_JSONSchema emits a closed document with envelope first", {
  s <- .cs_schema()
  expect_identical(
    s[["$schema"]],
    "https://json-schema.org/draft/2020-12/schema"
  )
  expect_identical(s[["$id"]], "https://example.org/doc/v1/schema.json")
  expect_identical(
    names(s[["properties"]]),
    c("version", "sequence", "entries", "variants", "metadata", "note")
  )
  expect_identical(as.vector(s[["required"]]), c("version", "sequence"))
  expect_false(s[["additionalProperties"]])
  expect_identical(s[["properties"]][["version"]][["const"]], "1.0.0")
})

test_that("no default keyword is emitted anywhere", {
  json <- as.character(jsonlite::toJSON(.cs_schema(), auto_unbox = TRUE))
  expect_false(grepl('"default"', json, fixed = TRUE))
})


# %% Fragments ----
test_that("string pattern and minLength are emitted", {
  seq <- .cs_schema()[["properties"]][["sequence"]]
  expect_identical(seq[["pattern"]], "^[A-Za-z*]+$")
  expect_identical(seq[["minLength"]], 2L)
})

test_that("a non-empty string without a pattern emits a non-blank pattern", {
  note <- .cs_schema()[["properties"]][["note"]]
  expect_identical(note[["type"]], c("string", "null"))
  expect_identical(note[["minLength"]], 1L)
  expect_identical(note[["pattern"]], "\\S")
})

test_that("collections and objects refer to class definitions", {
  s <- .cs_schema()
  entries <- s[["properties"]][["entries"]]
  expect_identical(entries[["type"]], "object")
  expect_identical(entries[["propertyNames"]], list(minLength = 1L))
  expect_identical(
    entries[["additionalProperties"]],
    list(`$ref` = "#/$defs/CS_Entry")
  )
  expect_null(entries[["minProperties"]])
  expect_identical(
    s[["properties"]][["metadata"]],
    list(`$ref` = "#/$defs/CS_Meta")
  )
  expect_setequal(
    names(s[["$defs"]]),
    c("CS_Entry", "CS_Meta", "mixedIndex", "positionIndex", "rangeIndex")
  )
})

test_that("def_name declarations are defined once and referred to", {
  defs <- .cs_schema()[["$defs"]]
  expect_identical(
    defs[["CS_Entry"]][["properties"]][["index"]],
    list(`$ref` = "#/$defs/mixedIndex")
  )
  expect_identical(
    defs[["mixedIndex"]][["anyOf"]],
    list(
      list(`$ref` = "#/$defs/positionIndex"),
      list(`$ref` = "#/$defs/rangeIndex")
    )
  )
  pos <- defs[["positionIndex"]]
  expect_identical(pos[["type"]], "array")
  expect_identical(pos[["items"]], list(type = "integer", minimum = 1L))
  expect_true(pos[["uniqueItems"]])
  expect_null(pos[["minItems"]])
  rng <- defs[["rangeIndex"]]
  expect_identical(
    rng[["items"]],
    list(
      type = "array",
      items = list(type = "integer", minimum = 1L),
      minItems = 2L,
      maxItems = 2L
    )
  )
  expect_identical(as.vector(defs[["CS_Entry"]][["required"]]), "index")
})

test_that("an open struct omits additionalProperties and lists its required members", {
  rec <- .cs_schema()[["properties"]][["variants"]][["items"]]
  expect_identical(rec[["type"]], "object")
  expect_identical(as.vector(rec[["required"]]), "position")
  expect_null(rec[["additionalProperties"]])
})

test_that("a property without a prop_* declaration is a declaration error", {
  Bare <- new_class("CS_Bare", properties = list(x = class_character))
  expect_error(
    class_to_JSONSchema(Bare, id = "https://example.org/bare.json"),
    "not built by a prop_\\* factory"
  )
})

test_that("two different schemas under one def_name are a declaration error", {
  Clash <- new_class(
    "CS_Clash",
    properties = list(
      a = prop_integer(def_name = "n"),
      b = prop_float(def_name = "n")
    )
  )
  expect_error(
    class_to_JSONSchema(Clash, id = "https://example.org/clash.json"),
    "different schemas"
  )
})

test_that("the document serializes through write_JSONSchema", {
  skip_if_not_installed("jsonlite")
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  write_JSONSchema(.cs_schema(), tmp, verbosity = 0L)
  back <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  expect_identical(names(back)[1:3], c("$id", "$schema", "title"))
  expect_identical(utils::tail(names(back), 1L), "$defs")
  expect_identical(back[["required"]], list("version", "sequence"))
  expect_identical(
    back[["$defs"]][["CS_Entry"]][["required"]],
    list("index")
  )
})
