# test-schema_contract.R
# ::rtemis.core::
# 2026- EDG rtemis.org

# %% helpers ----
one_prop <- function(description) {
  list(
    "$id" = "https://schema.rtemis.org/example/v1/schema.json",
    type = "object",
    properties = list(x = list(type = "integer", description = description))
  )
}


# %% rule 4: R constructs ----
test_that("a description naming an R construct is rejected", {
  expect_error(
    assert_config_contract(one_prop("Elastic net. See setup_GLMNET.")),
    "names an R construct"
  )
  expect_error(
    assert_config_contract(one_prop("Determined by stats::lm.")),
    "names an R construct"
  )
})

test_that("naming a package without a call is allowed", {
  expect_silent(assert_config_contract(one_prop("Elastic net (glmnet).")))
})


# %% rule 5: R spellings of a value ----
test_that("a description spelling a value the way R does is rejected", {
  for (text in c(
    "Case weights. NULL = unweighted.",
    "Handle missing values. FALSE ignores them.",
    "Applies only when linear_tree is TRUE.",
    "Undefined for a single component: NA.",
    "Repeats of the cross-validation. Requires @nfold.",
    "Repeats of the cross-validation. Requires config@nfold.",
    "Follow @rtemis."
  )) {
    expect_error(
      assert_config_contract(one_prop(text)),
      "spells a value the way R does",
      info = text
    )
  }
})

test_that("the language rules match a value, not a sentence containing one", {
  # Word boundaries: the remedy wording must not itself trip the rule, and a
  # longer word that merely contains the literal is not one.
  for (text in c(
    "Unset leaves the cases unweighted.",
    "A nullable value, annullable in the same pass.",
    "Reported as N/A where the estimator cannot supply it.",
    "Set to true to keep every split.",
    "Contact the maintainer at the address in DESCRIPTION."
  )) {
    expect_silent(assert_config_contract(one_prop(text)))
  }
})

test_that("email addresses are allowed without hiding other violations", {
  for (assert in list(assert_config_contract, assert_description_language)) {
    for (text in c(
      "Contact support@example.org.",
      "Contact <first.last+support@example.co.uk>.",
      "Contact a@example.org or b@example.org."
    )) {
      expect_silent(assert(one_prop(text)))
    }
    for (text in c(
      "Contact support@example.org. Requires @nfold.",
      "Contact support@example.org. Requires config@nfold.",
      "Contact support@example.org. NULL uses the default."
    )) {
      expect_error(
        assert(one_prop(text)),
        "x \\((@nfold|NULL)\\)",
        info = text
      )
    }
    expect_error(
      assert(one_prop("Contact support@example.org. See setup_GLMNET.")),
      "names an R construct"
    )
  }
})

test_that("every offending description is reported, not only the first", {
  schema <- list(
    "$id" = "https://schema.rtemis.org/example/v1/schema.json",
    type = "object",
    properties = list(
      a = list(type = "integer", description = "NULL uses the default."),
      b = list(type = "boolean", description = "FALSE skips the step.")
    )
  )
  err <- tryCatch(assert_config_contract(schema), error = identity)
  expect_s3_class(err, "simpleError")
  # One message naming both, each with the literal that tripped it.
  expect_match(conditionMessage(err), "a (NULL)", fixed = TRUE)
  expect_match(conditionMessage(err), "b (FALSE)", fixed = TRUE)
})

test_that("the rules hold at every depth", {
  # A nullable `$ref` is emitted as a `oneOf`, so a check that stopped at
  # `properties` would not see inside one.
  schema <- list(
    "$id" = "https://schema.rtemis.org/example/v1/schema.json",
    type = "object",
    properties = list(
      a = list(
        oneOf = list(
          list(type = "null"),
          list(type = "integer", description = "NULL uses the default.")
        )
      )
    )
  )
  expect_error(
    assert_config_contract(schema),
    "spells a value the way R does"
  )
})


# %% assert_description_language ----
test_that("assert_description_language applies the prose rules alone", {
  # A result class states what rtemis always writes, so it legitimately
  # requires its keys -- which `assert_config_contract()` would reject. Its
  # descriptions are published all the same.
  result_like <- list(
    "$id" = "https://schema.rtemis.org/examplemetrics/v1/schema.json",
    type = "object",
    required = c("n_cases"),
    properties = list(
      n_cases = list(type = "integer", description = "Cases scored.")
    )
  )
  expect_error(assert_config_contract(result_like), "declares required")
  expect_silent(assert_description_language(result_like))

  result_like[["properties"]][["n_cases"]][["description"]] <-
    "Cases scored. NA before the fit."
  expect_error(
    assert_description_language(result_like),
    "spells a value the way R does"
  )

  for (text in c("See setup_GLMNET.", "Determined by stats::lm.")) {
    result_like[["properties"]][["n_cases"]][["description"]] <- text
    expect_error(
      assert_description_language(result_like),
      "names an R construct",
      info = text
    )
  }
})

test_that("assert_description_language returns the schema invisibly", {
  schema <- one_prop("Number of clusters.")
  expect_identical(assert_description_language(schema), schema)
  expect_invisible(assert_description_language(schema))
})
