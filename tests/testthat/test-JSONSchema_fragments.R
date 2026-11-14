# test-JSONSchema_fragments.R

library(S7)

.JF <- local({
  Leaf <- new_class(
    "JF_Leaf",
    properties = list(
      algorithm = prop_string(const = "leaf"),
      x = prop_number(0)
    )
  )
  Tree <- new_class(
    "JF_Tree",
    properties = list(
      algorithm = prop_string(const = "tree"),
      depth = prop_integer(1L)
    )
  )
  Ref <- new_class("JF_Ref", properties = list(layout = prop_string("array")))
  family <- prop_union(
    list(prop_instance(Leaf), prop_instance(Tree)),
    def_name = "Model"
  )
  Doc <- new_class(
    "JF_Doc",
    properties = list(
      model = family,
      data = prop_instance(
        Ref,
        members = list(layout = prop_string(const = "factor")),
        nullable = TRUE
      ),
      leaf = prop_instance(Leaf, nullable = TRUE)
    )
  )
  list(Leaf = Leaf, Tree = Tree, Ref = Ref, family = family, Doc = Doc)
})


# %% refs ----
test_that("refs point a class or a def_name to another document", {
  leaf_class <- target_class_name(.JF$Leaf)
  ref_class <- target_class_name(.JF$Ref)
  schema <- class_to_JSONSchema(
    .JF$Doc,
    id = "https://example.org/doc.json",
    refs = c(
      Model = "https://example.org/model.json",
      stats::setNames("https://example.org/dataref.json", ref_class)
    )
  )
  props <- schema[["properties"]]
  expect_identical(
    props[["model"]],
    list(`$ref` = "https://example.org/model.json")
  )
  data <- props[["data"]][["anyOf"]][[1L]]
  expect_identical(data[["$ref"]], "https://example.org/dataref.json")
  expect_identical(data[["x-rtemis"]], list(class = ref_class))
  expect_identical(
    data[["properties"]][["layout"]],
    list(type = "string", const = "factor", minLength = 1L)
  )
  expect_identical(as.character(data[["required"]]), "layout")
  expect_identical(props[["data"]][["anyOf"]][[2L]], list(type = "null"))
  # The family's leaves enter $defs only through `leaf`; the family does not.
  expect_setequal(names(schema[["$defs"]]), "JF_Leaf")
  expect_identical(
    schema[["$defs"]][["JF_Leaf"]][["x-rtemis"]],
    list(class = leaf_class)
  )
})

test_that("without refs, a family is a $defs entry of its leaves", {
  schema <- class_to_JSONSchema(.JF$Doc, id = "https://example.org/doc.json")
  expect_identical(
    schema[["properties"]][["model"]],
    list(`$ref` = "#/$defs/Model")
  )
  expect_identical(
    schema[["$defs"]][["Model"]][["anyOf"]],
    list(list(`$ref` = "#/$defs/JF_Leaf"), list(`$ref` = "#/$defs/JF_Tree"))
  )
  expect_identical(
    names(schema[["$defs"]]),
    sort(names(schema[["$defs"]]), method = "radix")
  )
})

test_that("refs hold absolute URIs under distinct names", {
  expect_error(new_JSONSchema_defs(refs = c(Model = "model.json")), "no scheme")
  expect_error(
    new_JSONSchema_defs(refs = "https://example.org/a.json"),
    "names"
  )
  expect_error(
    new_JSONSchema_defs(refs = c(a = "https://x.org/1", a = "https://x.org/2")),
    "distinct"
  )
})


# %% Nullable unions ----
test_that("a nullable union admits null as one more alternative", {
  u <- prop_union(list(prop_string(), prop_integer()), nullable = TRUE)
  expect_identical(
    prop_to_JSONSchema(u, new_JSONSchema_defs())[["anyOf"]][[3L]],
    list(type = "null")
  )
  named <- prop_union(
    list(prop_string(), prop_integer()),
    nullable = TRUE,
    def_name = "U"
  )
  expect_identical(
    prop_to_JSONSchema(named, new_JSONSchema_defs()),
    list(anyOf = list(list(`$ref` = "#/$defs/U"), list(type = "null")))
  )
})


# %% Markers ----
test_that("matrix, table, and factor fragments carry their node", {
  m <- prop_to_JSONSchema(
    prop_matrix(prop_integer(), ncol = 2L),
    new_JSONSchema_defs()
  )
  expect_identical(m[["x-rtemis"]], list(node = "matrix"))
})


# %% annotate ----
test_that("annotate runs at the definition and member sites", {
  sites <- list()
  annotate <- function(fragment, declaration, site) {
    sites[[length(sites) + 1L]] <<- list(
      factory = declaration[["factory"]],
      site = site
    )
    if (identical(site[["kind"]], "member")) {
      fragment[["readOnly"]] <- TRUE
      fragment[["x-rtemis"]] <- c(fragment[["x-rtemis"]], list(group = "g"))
    }
    fragment
  }
  defs <- new_JSONSchema_defs(annotate = annotate)
  f <- prop_to_JSONSchema(
    prop_array(prop_integer(), nullable = TRUE),
    defs,
    name = "n",
    owner = "pkg::Cls"
  )
  expect_true(f[["readOnly"]])
  expect_identical(f[["x-rtemis"]], list(group = "g"))
  factories <- vapply(sites, `[[`, "", "factory")
  kinds <- vapply(sites, function(s) s[["site"]][["kind"]], "")
  expect_identical(factories, c("prop_integer", "prop_array", "prop_array"))
  expect_identical(kinds, c("definition", "definition", "member"))
  expect_identical(sites[[3L]][["site"]][["owner"]], "pkg::Cls")
  schema <- class_to_JSONSchema(
    .JF$Doc,
    id = "https://example.org/doc.json",
    annotate = annotate
  )
  expect_true(schema[["properties"]][["leaf"]][["readOnly"]])
  expect_null(schema[["$defs"]][["JF_Leaf"]][["readOnly"]])
})

test_that("annotate may only add annotations", {
  run <- function(f) {
    prop_to_JSONSchema(
      prop_matrix(prop_integer()),
      new_JSONSchema_defs(annotate = f),
      name = "m"
    )
  }
  expect_error(
    run(function(x, d, s) {
      x[["type"]] <- "object"
      x
    }),
    "changed 'type'"
  )
  expect_error(
    run(function(x, d, s) {
      x[["minimum"]] <- 1
      x
    }),
    "not an annotation"
  )
  expect_error(
    run(function(x, d, s) {
      x[["x-rtemis"]][["node"]] <- "table"
      x
    }),
    "'x-rtemis' member 'node'"
  )
  expect_no_error(run(function(x, d, s) {
    x[["description"]] <- "Ranges."
    x[["x-other"]] <- 1L
    x
  }))
})


# %% class_schema ----
test_that("class_schema supplies the object schema of a class", {
  hook <- function(cls, defs) {
    if (identical(cls@name, "JF_Tree")) {
      list(type = "object", properties = list(depth = list(type = "integer")))
    }
  }
  defs <- new_JSONSchema_defs(class_schema = hook)
  prop_to_JSONSchema(.JF$family, defs)
  entries <- JSONSchema_defs(defs)
  expect_identical(names(entries), c("JF_Leaf", "JF_Tree", "Model"))
  expect_identical(names(entries[["JF_Tree"]][["properties"]]), "depth")
  expect_identical(
    entries[["JF_Tree"]][["x-rtemis"]],
    list(class = target_class_name(.JF$Tree))
  )
  expect_false(entries[["JF_Leaf"]][["additionalProperties"]])
})

test_that("prop_to_JSONSchema takes a collector and a factory declaration", {
  expect_error(prop_to_JSONSchema(prop_integer(), new.env()), "collector")
  expect_error(
    prop_to_JSONSchema(new_property(class_integer), new_JSONSchema_defs()),
    "prop_\\* factory"
  )
})
