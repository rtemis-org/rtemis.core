# test-schema_class.R

library(S7)

# %% Fixtures ----
.SC <- local({
  Part <- schema_class(
    "SC_Part",
    properties = list(label = prop_string("", min_length = 0L)),
    description = "One labeled part of a record."
  )
  Plain <- new_class(
    "SC_Plain",
    properties = list(n = prop_integer(1L))
  )
  Record <- schema_class(
    "SC_Record",
    package = "example",
    properties = list(
      part = prop_instance(Part, default = quote(Part())),
      plain = prop_instance(Plain, default = quote(Plain()))
    ),
    validator = function(self) {
      if (identical(self@part@label, "invalid")) "@part is invalid."
    },
    description = "A record about one entity.",
    publication = SchemaPublication(
      scope = "shared",
      domain = "domain",
      kind = "data"
    ),
    extensions = list(example = list(units = "mm"))
  )
  Child <- schema_class(
    "SC_Child",
    package = "example",
    parent = Record,
    description = "A record with a source.",
    publication = SchemaPublication(
      domain = "domain",
      kind = "data",
      order = 2L
    )
  )
  list(Part = Part, Plain = Plain, Record = Record, Child = Child)
})

.sc_schema <- function(cls = .SC$Record, ...) {
  class_to_JSONSchema(
    cls,
    id = "https://example.org/record/v1/schema.json",
    ...
  )
}


# %% SchemaPublication ----
test_that("SchemaPublication requires domain and kind and fixes the vocabularies", {
  p <- SchemaPublication(domain = "ml", kind = "config")
  expect_identical(p@scope, "implementation")
  expect_identical(p@role, "document")
  expect_identical(p@order, 1L)
  expect_error(SchemaPublication(kind = "data"), "domain")
  expect_error(SchemaPublication(domain = "domain"), "kind")
  expect_error(SchemaPublication(domain = "bio", kind = "data"), "domain")
  expect_error(SchemaPublication(domain = "domain", kind = "dataset"), "kind")
  expect_error(
    SchemaPublication(domain = "domain", kind = "data", role = "inline"),
    "role"
  )
  expect_error(
    SchemaPublication(domain = "domain", kind = "data", order = 0L),
    "order"
  )
})


# %% schema_class ----
test_that("schema_class stores description, publication, and extensions", {
  expect_identical(class_description(.SC$Record), "A record about one entity.")
  expect_identical(class_publication(.SC$Record)@kind, "data")
  expect_identical(class_extension(.SC$Record, "example"), list(units = "mm"))
  expect_null(class_extension(.SC$Record, "other"))
  expect_identical(class_description(.SC$Part), "One labeled part of a record.")
  expect_null(class_publication(.SC$Part))
  expect_null(class_description(.SC$Plain))
  expect_null(class_publication(.SC$Plain))
})

test_that("metadata does not inherit and an instance reads its class's metadata", {
  expect_identical(class_description(.SC$Child), "A record with a source.")
  expect_identical(class_publication(.SC$Child)@order, 2L)
  expect_null(class_extension(.SC$Child, "example"))
  expect_identical(
    class_description(S7_class(.SC$Record())),
    "A record about one entity."
  )
})

test_that("schema_class keeps the validator it is given", {
  expect_error(
    .SC$Record(part = .SC$Part(label = "invalid")),
    "@part is invalid"
  )
  expect_true(S7_inherits(.SC$Part(), .SC$Part))
})

test_that("schema_class checks its arguments", {
  expect_error(
    schema_class("SC_X", properties = list(), description = ""),
    "description"
  )
  expect_error(schema_class("SC_X", properties = list()), "description")
  expect_error(
    schema_class(
      "SC_X",
      description = "An object.",
      publication = list(kind = "data")
    ),
    "SchemaPublication"
  )
  expect_error(
    schema_class(
      "SC_X",
      description = "An object.",
      publication = SchemaPublication(domain = "domain", kind = "data")
    ),
    "package"
  )
  expect_error(
    schema_class("SC_X", description = "An object.", extensions = list(1)),
    "extensions"
  )
  expect_error(
    schema_class(
      "SC_X",
      description = "An object.",
      extensions = list(a = list(), a = list())
    ),
    "extensions"
  )
  expect_error(
    schema_class("SC_X", description = "NULL = unweighted."),
    "Published-description contract"
  )
})


# %% class_to_JSONSchema ----
test_that("the root description defaults to the class description", {
  schema <- .sc_schema()
  expect_identical(schema[["description"]], "A record about one entity.")
  expect_identical(
    .sc_schema(description = "Override.")[["description"]],
    "Override."
  )
  expect_null(.sc_schema(description = "")[["description"]])
})

test_that("each $defs class carries its own description", {
  defs <- .sc_schema()[["$defs"]]
  expect_identical(
    defs[["SC_Part"]][["description"]],
    "One labeled part of a record."
  )
  expect_identical(names(defs[["SC_Part"]])[[1L]], "description")
  expect_null(defs[["SC_Plain"]][["description"]])
})

test_that("a published root writes x-rtemis.publication", {
  expect_identical(
    .sc_schema()[["x-rtemis"]],
    list(
      publication = list(
        producer = "example",
        language = "r",
        scope = "shared",
        domain = "domain",
        class = "example::SC_Record",
        role = "document",
        kind = "data",
        order = 1L
      )
    )
  )
  expect_null(.sc_schema(.SC$Part)[["x-rtemis"]])
})

test_that("a document whose parent is a published document names the parent", {
  publication <- .sc_schema(.SC$Child)[["x-rtemis"]][["publication"]]
  expect_identical(publication[["parent"]], "example::SC_Record")
  expect_identical(publication[["scope"]], "implementation")
})

test_that("only the root class writes a publication", {
  Holder <- new_class(
    "SC_Holder",
    properties = list(
      record = prop_instance(.SC$Record, default = quote(.SC$Record()))
    )
  )
  schema <- .sc_schema(Holder)
  expect_null(schema[["x-rtemis"]])
  expect_identical(
    schema[["$defs"]][["SC_Record"]][["x-rtemis"]],
    list(class = "example::SC_Record")
  )
  expect_identical(
    schema[["$defs"]][["SC_Record"]][["description"]],
    "A record about one entity."
  )
})
