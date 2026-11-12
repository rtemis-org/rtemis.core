# test-prop_factory_grammar.R

# The signatures of the canonical factories, as the rule in rtemis-skills
# `R/prop-factories.md` fixes them. A change to a signature is made in the rule
# first, then here and in the factory.
.GRAMMAR <- list(
  prop_boolean = alist(
    default = NULL,
    ... = ,
    const = NULL,
    nullable = FALSE,
    def_name = NULL,
    description = ""
  ),
  prop_integer = alist(
    default = NULL,
    ... = ,
    enum = NULL,
    const = NULL,
    min = NULL,
    max = NULL,
    exclusive_min = NULL,
    exclusive_max = NULL,
    nullable = FALSE,
    def_name = NULL,
    description = ""
  ),
  prop_number = alist(
    default = NULL,
    ... = ,
    enum = NULL,
    const = NULL,
    min = NULL,
    max = NULL,
    exclusive_min = NULL,
    exclusive_max = NULL,
    nullable = FALSE,
    def_name = NULL,
    description = ""
  ),
  prop_string = alist(
    default = NULL,
    ... = ,
    enum = NULL,
    const = NULL,
    pattern = NULL,
    min_length = 1L,
    max_length = NULL,
    nullable = FALSE,
    def_name = NULL,
    description = ""
  ),
  prop_array = alist(
    items = ,
    ... = ,
    default = NULL,
    min_items = 1L,
    max_items = NULL,
    unique_items = FALSE,
    nullable = FALSE,
    def_name = NULL,
    description = ""
  ),
  prop_map = alist(
    values = ,
    ... = ,
    default = NULL,
    min_members = 1L,
    max_members = NULL,
    nullable = FALSE,
    def_name = NULL,
    description = ""
  ),
  prop_matrix = alist(
    items = ,
    ncol = ,
    ... = ,
    default = NULL,
    min_items = 1L,
    max_items = NULL,
    nullable = FALSE,
    def_name = NULL,
    description = ""
  ),
  prop_struct = alist(
    members = ,
    ... = ,
    default = NULL,
    required = NULL,
    additional_members = FALSE,
    nullable = FALSE,
    def_name = NULL,
    description = ""
  ),
  prop_union = alist(
    alternatives = ,
    ... = ,
    default = NULL,
    nullable = FALSE,
    def_name = NULL,
    description = ""
  ),
  prop_instance = alist(
    cls = ,
    ... = ,
    default = NULL,
    nullable = FALSE,
    description = ""
  ),
  prop_any = alist(... = , default = NULL, description = "")
)

.COMPAT_PROPS <- c("prop_float", "prop_bag", "prop_const")
.NON_FACTORIES <- c("prop_spec", "prop_encode", "prop_decode")


# %% Formals ----
test_that("every canonical factory has the signature the rule fixes", {
  for (nm in names(.GRAMMAR)) {
    f <- getExportedValue("rtemis.core", nm)
    expect_identical(as.list(formals(f)), .GRAMMAR[[nm]], label = nm)
  }
})

test_that("the exported prop_* names are the factories, compatibility forms, and accessors", {
  exported <- grep("^prop_", getNamespaceExports("rtemis.core"), value = TRUE)
  expect_setequal(
    exported,
    c(names(.GRAMMAR), .COMPAT_PROPS, .NON_FACTORIES)
  )
})


# %% Rule ----
# The rule lives outside the package. `just check-rule` sets RTEMIS_PROP_RULE
# to its path; elsewhere the test is skipped.
test_that("the rule's signatures equal the table above", {
  path <- Sys.getenv("RTEMIS_PROP_RULE")
  skip_if(!nzchar(path), "RTEMIS_PROP_RULE is unset")
  lines <- readLines(path)
  start <- which(lines == "```r")[[1L]]
  end <- start + which(lines[(start + 1L):length(lines)] == "```")[[1L]]
  calls <- lines[(start + 1L):(end - 1L)]
  parsed <- lapply(calls, function(line) str2lang(line))
  rule <- lapply(parsed, function(call) {
    args <- as.list(call)[-1L]
    keys <- names(args) %||% rep("", length(args))
    out <- vector("list", length(args))
    for (i in seq_along(args)) {
      if (nzchar(keys[[i]])) {
        out[i] <- list(args[[i]])
      } else {
        keys[[i]] <- as.character(args[[i]])
        out[i] <- list(quote(expr = ))
      }
    }
    names(out) <- keys
    out
  })
  names(rule) <- vapply(parsed, function(call) as.character(call[[1L]]), "")
  expect_identical(rule, .GRAMMAR)
})
