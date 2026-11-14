# test-class_references.R

library(S7)

.CR <- local({
  CR_Node <- new_class(
    "CR_Node",
    properties = list(
      kind = prop_string(const = "node"),
      children = prop_array(
        prop_instance("CR_Node"),
        min_items = 0L,
        default = list()
      ),
      leaf = prop_instance("CR_Leaf", nullable = TRUE)
    )
  )
  CR_Leaf <- new_class(
    "CR_Leaf",
    properties = list(kind = prop_string(const = "leaf"), x = prop_number(0))
  )
  CR_Cand <- new_class("CR_Cand", properties = list(candidates = prop_any()))
  environment()
})


# %% Resolution ----
test_that("a class named by a string resolves where the call is written", {
  n <- .CR$CR_Node(children = list(.CR$CR_Node()), leaf = .CR$CR_Leaf(x = 1))
  expect_length(n@children, 1L)
  expect_identical(n@leaf@x, 1)
  expect_error(
    .CR$CR_Node(children = list(.CR$CR_Leaf())),
    "must be a CR_Node object"
  )
  expect_true(check_class_references(.CR))
})

test_that("a value is checked by name, and emission resolves the name", {
  Holder <- new_class(
    "CR_Holder",
    properties = list(x = prop_instance("CR_Missing", nullable = TRUE))
  )
  expect_null(Holder()@x)
  expect_error(Holder(x = .CR$CR_Leaf()), "must be a CR_Missing object")
  expect_error(
    class_to_JSONSchema(Holder, id = "https://example.org/h.json"),
    "names no S7 class",
    class = "rtemis_class_reference_error"
  )
  env <- new.env()
  env$Holder <- Holder
  expect_error(
    check_class_references(env),
    "CR_Holder@x: 'CR_Missing' names no S7 class",
    class = "rtemis_class_reference_error"
  )
})

test_that("a self-referencing class emits one definition that refers to itself", {
  schema <- class_to_JSONSchema(.CR$CR_Node, id = "https://example.org/n.json")
  expect_identical(
    schema[["properties"]][["children"]][["items"]],
    list(`$ref` = "#/$defs/CR_Node")
  )
  node <- schema[["$defs"]][["CR_Node"]]
  expect_identical(
    node[["properties"]][["children"]][["items"]][["$ref"]],
    "#/$defs/CR_Node"
  )
  expect_match(node[["x-rtemis"]][["class"]], "CR_Node$")
})

test_that("an instance has no implicit default", {
  Shape <- new_class(
    "CR_Shape",
    properties = list(origin = prop_instance(.CR$CR_Leaf))
  )
  expect_error(Shape(), "must not be NULL")
  expect_identical(Shape(origin = .CR$CR_Leaf())@origin, .CR$CR_Leaf())
  schema <- class_to_JSONSchema(Shape, id = "https://example.org/s.json")
  expect_identical(as.character(schema[["required"]]), "origin")
})

test_that("a forward reference takes a default only once its class exists", {
  expect_error(
    prop_instance("CR_Later", default = quote(CR_Later())),
    "could not find function"
  )
})


# %% Unions ----
test_that("a union of named classes checks its discriminator when they resolve", {
  u <- prop_union(list(prop_instance("CR_Node"), prop_instance("CR_Leaf")))
  expect_null(prop_spec(u)[["discriminator"]])
  local({
    CR_Node <- .CR$CR_Node
    CR_Leaf <- .CR$CR_Leaf
    u2 <- prop_union(list(prop_instance("CR_Node"), prop_instance("CR_Leaf")))
    expect_identical(prop_spec(u2)[["discriminator"]], "kind")
  })
  declare <- function() {
    late <- prop_union(list(prop_instance("CR_A"), prop_instance("CR_B")))
    CR_A <- new_class("CR_A", properties = list(x = prop_number(0)))
    CR_B <- new_class("CR_B", properties = list(x = prop_number(0)))
    CR_UHolder <- new_class("CR_UHolder", properties = list(u = late))
    environment()
  }
  expect_error(check_class_references(declare()), "share a `const` property")
})

test_that("a union of instances decodes by its discriminator", {
  u <- prop_union(list(prop_instance(.CR$CR_Node), prop_instance(.CR$CR_Leaf)))
  leaf <- prop_decode(u, list(kind = "leaf", x = 2))
  expect_identical(leaf, .CR$CR_Leaf(x = 2))
  expect_error(
    prop_decode(u, list(kind = "leaf", x = "a")),
    "must be a number"
  )
  expect_error(
    prop_decode(u, list(kind = "tree")),
    "names none of the union's classes"
  )
})

test_that("a union that nests a union of instances needs no discriminator", {
  Set <- new_class(
    "CR_Set",
    properties = list(variants = prop_map(prop_instance(.CR$CR_Leaf)))
  )
  family <- prop_union(list(
    prop_instance(.CR$CR_Node),
    prop_instance(.CR$CR_Leaf)
  ))
  pair <- prop_union(list(family, prop_instance(Set)), nullable = TRUE)
  set <- Set(variants = list(a = .CR$CR_Leaf()))
  json <- to_JSON(prop_encode(pair, set))
  expect_identical(
    prop_decode(pair, jsonlite::fromJSON(json, simplifyVector = FALSE)),
    set
  )
})


# %% Use-site members ----
test_that("use-site members constrain, encode, and decode named properties", {
  p <- prop_instance(
    .CR$CR_Cand,
    members = list(candidates = prop_array(prop_number(), min_items = 2L))
  )
  H <- new_class("CR_H", properties = list(x = p))
  expect_error(
    H(x = .CR$CR_Cand(candidates = 1)),
    "property 'candidates' must have at least 2 elements"
  )
  v <- .CR$CR_Cand(candidates = c(0.1, 0.2))
  json <- to_JSON(prop_encode(p, v))
  expect_identical(json, '{"candidates":[0.1,0.2]}')
  expect_identical(
    prop_decode(p, jsonlite::fromJSON(json, simplifyVector = FALSE)),
    v
  )
  defs <- new_JSONSchema_defs()
  fragment <- prop_to_JSONSchema(p, defs)
  expect_identical(fragment[["$ref"]], "#/$defs/CR_Cand")
  expect_identical(
    fragment[["properties"]][["candidates"]][["minItems"]],
    2L
  )
  expect_identical(as.character(fragment[["required"]]), "candidates")
  expect_named(JSONSchema_defs(defs), "CR_Cand")
})

test_that("use-site members name properties of the class", {
  expect_error(
    prop_instance(.CR$CR_Cand, members = list(other = prop_any())),
    "does not have"
  )
  expect_error(prop_instance(.CR$CR_Cand, members = list(prop_any())), "names")
  expect_error(
    prop_instance(.CR$CR_Cand, members = list(candidates = prop_number(1))),
    "must not declare a `default`"
  )
})

test_that("an array item with use-site members keeps its declaration", {
  item <- prop_instance(.CR$CR_Leaf, members = list(x = prop_number(min = 1)))
  arr <- prop_array(item)
  spec <- prop_spec(arr)
  expect_identical(spec[["items"]][["members"]][["x"]][["minimum"]], 1)
  H <- new_class("CR_HA", properties = list(a = arr))
  expect_error(H(a = list(.CR$CR_Leaf(x = 0))), "property 'x' must be >= 1")
})
