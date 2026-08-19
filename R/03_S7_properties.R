# 2026- EDG rtemis.org

# TOC ----
# Character -------------------------------------------------------------------
#   character_scalar / optional_character_scalar
# Double ----------------------------------------------------------------------
#   double_scalar / optional_double_scalar
# Integer ---------------------------------------------------------------------
#   integer_scalar / optional_integer_scalar
#   nonneg_integer_scalar / optional_nonneg_integer_scalar [0, Inf)
#   pos_integer_scalar / optional_pos_integer_scalar       (0, Inf)
# Logical ---------------------------------------------------------------------
#   logical_scalar / optional_logical_scalar
# Bounded double scalars ------------------------------------------------------
#   prob_scalar / optional_prob_scalar                   [0, 1]
#   unit_open_scalar / optional_unit_open_scalar         (0, 1)
#   pos_double_scalar / optional_pos_double_scalar       (0, Inf)
#   nonneg_double_scalar / optional_nonneg_double_scalar [0, Inf)
# Bounded double vectors ------------------------------------------------------
#   prob_vector / optional_prob_vector                   [0, 1]
#   unit_open_vector / optional_unit_open_vector         (0, 1)
#   pos_double_vector / optional_pos_double_vector       (0, Inf)
#   nonneg_double_vector / optional_nonneg_double_vector [0, Inf)
# Factory ---------------------------------------------------------------------
#   bounded_double_property

# %% Character ----
#' Non-empty character scalar S7 property
#'
#' S7 property accepting a single non-NA, non-empty (after trimming whitespace) string.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' Model <- S7::new_class("Model", properties = list(algorithm = character_scalar))
#' Model(algorithm = "LightGBM")@algorithm
#' try(Model(algorithm = ""))
character_scalar <- new_property(
  class_character,
  validator = function(value) {
    if (length(value) != 1L || is.na(value) || !nzchar(trimws(value))) {
      return("must be a non-empty character scalar")
    }
    NULL
  }
)


#' Optional non-empty character scalar S7 property
#'
#' S7 property accepting `NULL` or a single non-NA, non-empty (after trimming whitespace) string.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' Model <- S7::new_class("Model", properties = list(label = optional_character_scalar))
#' Model()@label
#' Model(label = "Experiment 1")@label
#' try(Model(label = ""))
optional_character_scalar <- new_property(
  class = new_union(NULL, class_character),
  validator = function(value) {
    if (
      !is.null(value) &&
        (length(value) != 1L || is.na(value) || !nzchar(trimws(value)))
    ) {
      return("must be NULL or a non-empty character scalar")
    }
    NULL
  }
)


# %% Double ----
#' Double scalar S7 property
#'
#' S7 property accepting a single non-NA double value.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' Metric <- S7::new_class("Metric", properties = list(value = double_scalar))
#' Metric(value = -1.5)@value
#' try(Metric(value = c(1, 2)))
double_scalar <- new_property(
  class_double,
  validator = function(value) {
    if (length(value) != 1L || is.na(value)) {
      return("must be a double scalar")
    }
    NULL
  }
)


#' Optional double scalar S7 property
#'
#' S7 property accepting `NULL` or a single non-NA double value.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' Metric <- S7::new_class("Metric", properties = list(offset = optional_double_scalar))
#' Metric()@offset
#' Metric(offset = 0.5)@offset
#' try(Metric(offset = NA_real_))
optional_double_scalar <- new_property(
  class = new_union(NULL, class_double),
  validator = function(value) {
    if (!is.null(value) && (length(value) != 1L || is.na(value))) {
      return("must be NULL or a double scalar")
    }
    NULL
  }
)


# %% Integer ----
#' Integer scalar S7 property
#'
#' S7 property accepting a single non-NA integer value (must be `integer` type, e.g. `1L`).
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' Params <- S7::new_class("Params", properties = list(seed = integer_scalar))
#' Params(seed = 42L)@seed
#' # Doubles are not accepted: the type must be integer
#' try(Params(seed = 42))
integer_scalar <- new_property(
  class_integer,
  validator = function(value) {
    if (length(value) != 1L || is.na(value)) {
      return("must be an integer scalar (e.g. 1L)")
    }
    NULL
  }
)


#' Optional integer scalar S7 property
#'
#' S7 property accepting `NULL` or a single non-NA integer value (must be `integer` type, e.g. `1L`).
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' Params <- S7::new_class("Params", properties = list(seed = optional_integer_scalar))
#' Params()@seed
#' Params(seed = 42L)@seed
#' try(Params(seed = 42))
optional_integer_scalar <- new_property(
  class = new_union(NULL, class_integer),
  validator = function(value) {
    if (!is.null(value) && (length(value) != 1L || is.na(value))) {
      return("must be NULL or an integer scalar (e.g. 1L)")
    }
    NULL
  }
)


#' Non-negative integer scalar S7 property
#'
#' S7 property accepting a single non-NA integer value greater than or equal to zero,
#' i.e. in \eqn{[0, \infty)} (e.g. `0L`, `1L`).
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' Counter <- S7::new_class(
#'   "Counter",
#'   properties = list(n_failed = nonneg_integer_scalar)
#' )
#' Counter(n_failed = 0L)@n_failed
#' try(Counter(n_failed = -1L))
nonneg_integer_scalar <- new_property(
  class_integer,
  validator = function(value) {
    if (length(value) != 1L || is.na(value) || value < 0L) {
      return("must be a non-negative integer scalar (>= 0, e.g. 0L)")
    }
    NULL
  }
)


#' Optional non-negative integer scalar S7 property
#'
#' S7 property accepting `NULL` or a single non-NA integer value greater than or equal to zero.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' Counter <- S7::new_class(
#'   "Counter",
#'   properties = list(n_failed = optional_nonneg_integer_scalar)
#' )
#' Counter()@n_failed
#' Counter(n_failed = 3L)@n_failed
#' try(Counter(n_failed = -1L))
optional_nonneg_integer_scalar <- new_property(
  class = new_union(NULL, class_integer),
  validator = function(value) {
    if (
      !is.null(value) && (length(value) != 1L || is.na(value) || value < 0L)
    ) {
      return("must be NULL or a non-negative integer scalar (>= 0, e.g. 0L)")
    }
    NULL
  }
)


#' Positive integer scalar S7 property
#'
#' S7 property accepting a single non-NA integer value strictly greater than zero (e.g. `1L`).
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' Resample <- S7::new_class(
#'   "Resample",
#'   properties = list(n_resamples = pos_integer_scalar)
#' )
#' Resample(n_resamples = 10L)@n_resamples
#' try(Resample(n_resamples = 0L))
pos_integer_scalar <- new_property(
  class_integer,
  validator = function(value) {
    if (length(value) != 1L || is.na(value) || value <= 0L) {
      return("must be a positive integer scalar (> 0, e.g. 1L)")
    }
    NULL
  }
)


#' Optional positive integer scalar S7 property
#'
#' S7 property accepting `NULL` or a single non-NA integer value strictly greater than zero.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' Resample <- S7::new_class(
#'   "Resample",
#'   properties = list(n_workers = optional_pos_integer_scalar)
#' )
#' Resample()@n_workers
#' Resample(n_workers = 4L)@n_workers
#' try(Resample(n_workers = 0L))
optional_pos_integer_scalar <- new_property(
  class = new_union(NULL, class_integer),
  validator = function(value) {
    if (
      !is.null(value) && (length(value) != 1L || is.na(value) || value <= 0L)
    ) {
      return("must be NULL or a positive integer scalar (> 0, e.g. 1L)")
    }
    NULL
  }
)


# %% Logical ----
#' Logical scalar S7 property
#'
#' S7 property accepting a single non-NA logical value.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' Params <- S7::new_class("Params", properties = list(scale = logical_scalar))
#' Params(scale = TRUE)@scale
#' try(Params(scale = NA))
logical_scalar <- new_property(
  class_logical,
  validator = function(value) {
    if (length(value) != 1L || is.na(value)) {
      return("must be a logical scalar (TRUE or FALSE)")
    }
    NULL
  }
)


#' Optional logical scalar S7 property
#'
#' S7 property accepting `NULL` or a single non-NA logical value.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' Params <- S7::new_class("Params", properties = list(scale = optional_logical_scalar))
#' Params()@scale
#' Params(scale = FALSE)@scale
#' try(Params(scale = c(TRUE, FALSE)))
optional_logical_scalar <- new_property(
  class = new_union(NULL, class_logical),
  validator = function(value) {
    if (!is.null(value) && (length(value) != 1L || is.na(value))) {
      return("must be NULL or a logical scalar (TRUE or FALSE)")
    }
    NULL
  }
)


# %% Bounded double scalars ----
#' Probability scalar S7 property
#'
#' S7 property accepting a single finite double in \eqn{[0, 1]}.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' Split <- S7::new_class("Split", properties = list(train_p = prob_scalar))
#' Split(train_p = 0.75)@train_p
#' try(Split(train_p = 1.5))
prob_scalar <- new_property(
  class_double,
  validator = function(value) {
    if (length(value) != 1L || is.na(value) || value < 0 || value > 1) {
      return("must be a finite double in [0, 1]")
    }
    NULL
  }
)


#' Optional probability scalar S7 property
#'
#' S7 property accepting `NULL` or a single finite double in \eqn{[0, 1]}.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' Split <- S7::new_class("Split", properties = list(threshold = optional_prob_scalar))
#' Split()@threshold
#' Split(threshold = 0.5)@threshold
#' try(Split(threshold = -0.1))
optional_prob_scalar <- new_property(
  class = new_union(NULL, class_double),
  validator = function(value) {
    if (
      !is.null(value) &&
        (length(value) != 1L || is.na(value) || value < 0 || value > 1)
    ) {
      return("must be NULL or a finite double in [0, 1]")
    }
    NULL
  }
)


#' Open-unit-interval scalar S7 property
#'
#' S7 property accepting a single finite double strictly in \eqn{(0, 1)}.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' Boost <- S7::new_class("Boost", properties = list(learning_rate = unit_open_scalar))
#' Boost(learning_rate = 0.1)@learning_rate
#' # Bounds are exclusive: 0 and 1 are rejected
#' try(Boost(learning_rate = 1))
unit_open_scalar <- new_property(
  class_double,
  validator = function(value) {
    if (length(value) != 1L || is.na(value) || value <= 0 || value >= 1) {
      return("must be a finite double in (0, 1)")
    }
    NULL
  }
)


#' Optional open-unit-interval scalar S7 property
#'
#' S7 property accepting `NULL` or a single finite double strictly in \eqn{(0, 1)}.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' Boost <- S7::new_class(
#'   "Boost",
#'   properties = list(subsample = optional_unit_open_scalar)
#' )
#' Boost()@subsample
#' Boost(subsample = 0.8)@subsample
#' try(Boost(subsample = 0))
optional_unit_open_scalar <- new_property(
  class = new_union(NULL, class_double),
  validator = function(value) {
    if (
      !is.null(value) &&
        (length(value) != 1L || is.na(value) || value <= 0 || value >= 1)
    ) {
      return("must be NULL or a finite double in (0, 1)")
    }
    NULL
  }
)


#' Positive double scalar S7 property
#'
#' S7 property accepting a single finite double strictly greater than zero, i.e. in \eqn{(0, \infty)}.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' Penalty <- S7::new_class("Penalty", properties = list(lambda = pos_double_scalar))
#' Penalty(lambda = 0.01)@lambda
#' try(Penalty(lambda = 0))
pos_double_scalar <- new_property(
  class_double,
  validator = function(value) {
    if (
      length(value) != 1L || is.na(value) || !is.finite(value) || value <= 0
    ) {
      return("must be a finite positive double (> 0)")
    }
    NULL
  }
)


#' Optional positive double scalar S7 property
#'
#' S7 property accepting `NULL` or a single finite double strictly greater than zero.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' Penalty <- S7::new_class(
#'   "Penalty",
#'   properties = list(gamma = optional_pos_double_scalar)
#' )
#' Penalty()@gamma
#' Penalty(gamma = 2)@gamma
#' try(Penalty(gamma = Inf))
optional_pos_double_scalar <- new_property(
  class = new_union(NULL, class_double),
  validator = function(value) {
    if (
      !is.null(value) &&
        (length(value) != 1L || is.na(value) || !is.finite(value) || value <= 0)
    ) {
      return("must be NULL or a finite positive double (> 0)")
    }
    NULL
  }
)


#' Non-negative double scalar S7 property
#'
#' S7 property accepting a single finite double greater than or equal to zero, i.e. in \eqn{[0, \infty)}.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' Penalty <- S7::new_class("Penalty", properties = list(alpha = nonneg_double_scalar))
#' Penalty(alpha = 0)@alpha
#' try(Penalty(alpha = -1))
nonneg_double_scalar <- new_property(
  class_double,
  validator = function(value) {
    if (length(value) != 1L || is.na(value) || !is.finite(value) || value < 0) {
      return("must be a finite non-negative double (>= 0)")
    }
    NULL
  }
)


#' Optional non-negative double scalar S7 property
#'
#' S7 property accepting `NULL` or a single finite double greater than or equal to zero.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' Penalty <- S7::new_class(
#'   "Penalty",
#'   properties = list(alpha = optional_nonneg_double_scalar)
#' )
#' Penalty()@alpha
#' Penalty(alpha = 0)@alpha
#' try(Penalty(alpha = -1))
optional_nonneg_double_scalar <- new_property(
  class = new_union(NULL, class_double),
  validator = function(value) {
    if (
      !is.null(value) &&
        (length(value) != 1L || is.na(value) || !is.finite(value) || value < 0)
    ) {
      return("must be NULL or a finite non-negative double (>= 0)")
    }
    NULL
  }
)


# %% Bounded double vectors ----
#' Probability vector S7 property
#'
#' S7 property accepting a non-empty double vector with all elements in \eqn{[0, 1]} and no NAs.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' Preds <- S7::new_class("Preds", properties = list(probabilities = prob_vector))
#' Preds(probabilities = c(0, 0.5, 1))@probabilities
#' try(Preds(probabilities = c(0.5, NA)))
prob_vector <- new_property(
  class_double,
  validator = function(value) {
    if (length(value) == 0L) {
      return("must be a non-empty vector")
    }
    if (anyNA(value)) {
      return("must not contain NAs")
    }
    if (any(value < 0) || any(value > 1)) {
      return("all elements must be in [0, 1]")
    }
    NULL
  }
)


#' Optional probability vector S7 property
#'
#' S7 property accepting `NULL` or a non-empty double vector with all elements in \eqn{[0, 1]}
#' and no NAs.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' Preds <- S7::new_class(
#'   "Preds",
#'   properties = list(probabilities = optional_prob_vector)
#' )
#' Preds()@probabilities
#' Preds(probabilities = c(0.1, 0.9))@probabilities
#' try(Preds(probabilities = c(0.1, 1.1)))
optional_prob_vector <- new_property(
  class = new_union(NULL, class_double),
  validator = function(value) {
    if (is.null(value)) {
      return(NULL)
    }
    if (length(value) == 0L) {
      return("must be a non-empty vector")
    }
    if (anyNA(value)) {
      return("must not contain NAs")
    }
    if (any(value < 0) || any(value > 1)) {
      return("all elements must be in [0, 1]")
    }
    NULL
  }
)


#' Open-unit-interval vector S7 property
#'
#' S7 property accepting a non-empty double vector with all elements strictly in \eqn{(0, 1)}
#' and no NAs.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' Grid <- S7::new_class("Grid", properties = list(learning_rates = unit_open_vector))
#' Grid(learning_rates = c(0.01, 0.1))@learning_rates
#' try(Grid(learning_rates = c(0.1, 1)))
unit_open_vector <- new_property(
  class_double,
  validator = function(value) {
    if (length(value) == 0L) {
      return("must be a non-empty vector")
    }
    if (anyNA(value)) {
      return("must not contain NAs")
    }
    if (any(value <= 0) || any(value >= 1)) {
      return("all elements must be in (0, 1)")
    }
    NULL
  }
)


#' Optional open-unit-interval vector S7 property
#'
#' S7 property accepting `NULL` or a non-empty double vector with all elements strictly in
#' \eqn{(0, 1)} and no NAs.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' Grid <- S7::new_class(
#'   "Grid",
#'   properties = list(subsamples = optional_unit_open_vector)
#' )
#' Grid()@subsamples
#' Grid(subsamples = c(0.5, 0.8))@subsamples
#' try(Grid(subsamples = c(0.5, 1)))
optional_unit_open_vector <- new_property(
  class = new_union(NULL, class_double),
  validator = function(value) {
    if (is.null(value)) {
      return(NULL)
    }
    if (length(value) == 0L) {
      return("must be a non-empty vector")
    }
    if (anyNA(value)) {
      return("must not contain NAs")
    }
    if (any(value <= 0) || any(value >= 1)) {
      return("all elements must be in (0, 1)")
    }
    NULL
  }
)


#' Positive double vector S7 property
#'
#' S7 property accepting a non-empty double vector with all elements finite, strictly greater
#' than zero, and no NAs.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' Weights <- S7::new_class(
#'   "Weights",
#'   properties = list(case_weights = pos_double_vector)
#' )
#' Weights(case_weights = c(0.5, 1, 2))@case_weights
#' try(Weights(case_weights = c(1, 0)))
pos_double_vector <- new_property(
  class_double,
  validator = function(value) {
    if (length(value) == 0L) {
      return("must be a non-empty vector")
    }
    if (anyNA(value)) {
      return("must not contain NAs")
    }
    if (!all(is.finite(value)) || any(value <= 0)) {
      return("all elements must be finite and > 0")
    }
    NULL
  }
)


#' Optional positive double vector S7 property
#'
#' S7 property accepting `NULL` or a non-empty double vector with all elements finite,
#' strictly greater than zero, and no NAs.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' Grid <- S7::new_class("Grid", properties = list(lambdas = optional_pos_double_vector))
#' Grid()@lambdas
#' Grid(lambdas = c(0.01, 0.1, 1))@lambdas
#' try(Grid(lambdas = c(0.1, Inf)))
optional_pos_double_vector <- new_property(
  class = new_union(NULL, class_double),
  validator = function(value) {
    if (is.null(value)) {
      return(NULL)
    }
    if (length(value) == 0L) {
      return("must be a non-empty vector")
    }
    if (anyNA(value)) {
      return("must not contain NAs")
    }
    if (!all(is.finite(value)) || any(value <= 0)) {
      return("all elements must be finite and > 0")
    }
    NULL
  }
)


#' Non-negative double vector S7 property
#'
#' S7 property accepting a non-empty double vector with all elements finite, greater than or
#' equal to zero, and no NAs.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' Importance <- S7::new_class(
#'   "Importance",
#'   properties = list(scores = nonneg_double_vector)
#' )
#' Importance(scores = c(0, 1.5, 3))@scores
#' try(Importance(scores = c(1, -1)))
nonneg_double_vector <- new_property(
  class_double,
  validator = function(value) {
    if (length(value) == 0L) {
      return("must be a non-empty vector")
    }
    if (anyNA(value)) {
      return("must not contain NAs")
    }
    if (!all(is.finite(value)) || any(value < 0)) {
      return("all elements must be finite and >= 0")
    }
    NULL
  }
)


#' Optional non-negative double vector S7 property
#'
#' S7 property accepting `NULL` or a non-empty double vector with all elements finite,
#' greater than or equal to zero, and no NAs.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' Importance <- S7::new_class(
#'   "Importance",
#'   properties = list(scores = optional_nonneg_double_vector)
#' )
#' Importance()@scores
#' Importance(scores = c(0, 2))@scores
#' try(Importance(scores = c(0, -2)))
optional_nonneg_double_vector <- new_property(
  class = new_union(NULL, class_double),
  validator = function(value) {
    if (is.null(value)) {
      return(NULL)
    }
    if (length(value) == 0L) {
      return("must be a non-empty vector")
    }
    if (anyNA(value)) {
      return("must not contain NAs")
    }
    if (!all(is.finite(value)) || any(value < 0)) {
      return("all elements must be finite and >= 0")
    }
    NULL
  }
)


# %% Factory ----
#' Create a bounded double S7 property
#'
#' Returns a `new_property()` for a double scalar constrained to a given interval.
#' Useful for bounds not covered by the pre-built properties.
#'
#' @param lower Numeric scalar. Lower bound. Default `-Inf`.
#' @param upper Numeric scalar. Upper bound. Default `Inf`.
#' @param lower_open Logical scalar. If `TRUE`, lower bound is exclusive `(lower, ...]`.
#'   Default `FALSE`.
#' @param upper_open Logical scalar. If `TRUE`, upper bound is exclusive `[..., upper)`.
#'   Default `FALSE`.
#' @param nullable Logical scalar. If `TRUE`, `NULL` is also accepted. Default `FALSE`.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' # Learning rate in (0, 1]
#' lr_prop <- bounded_double_property(0, 1, lower_open = TRUE)
bounded_double_property <- function(
  lower = -Inf,
  upper = Inf,
  lower_open = FALSE,
  upper_open = FALSE,
  nullable = FALSE
) {
  lower_sym <- if (lower_open) "(" else "["
  upper_sym <- if (upper_open) ")" else "]"
  bound_desc <- paste0(
    "must be a finite double in ",
    lower_sym,
    lower,
    ", ",
    upper,
    upper_sym
  )

  check_lower <- if (lower_open) {
    function(v) v > lower
  } else {
    function(v) v >= lower
  }
  check_upper <- if (upper_open) {
    function(v) v < upper
  } else {
    function(v) v <= upper
  }

  cls <- if (nullable) new_union(NULL, class_double) else class_double

  new_property(
    class = cls,
    validator = function(value) {
      if (nullable && is.null(value)) {
        return(NULL)
      }
      if (length(value) != 1L || is.na(value) || !is.finite(value)) {
        return(paste0(bound_desc, " (must be a finite scalar)"))
      }
      if (!check_lower(value) || !check_upper(value)) {
        return(bound_desc)
      }
      NULL
    }
  )
}


# %% enum() ----
#' Create an enum S7 property
#'
#' Returns a `new_property()` for a character scalar constrained to a fixed set of allowed values.
#'
#' @param values Character: Allowed values.
#' @param default Optional Character: Default value.
#' @param nullable Logical scalar. If `TRUE`, `NULL` is also accepted. Default `FALSE`.
#'
#' @return An S7 property object.
#' @author EDG
#' @export
#'
#' @examples
#' type_prop <- enum(c("string", "number", "boolean"), default = "string")
enum <- function(values, default = NULL, nullable = FALSE) {
  cls <- if (nullable) new_union(NULL, class_character) else class_character
  new_property(
    class = cls,
    validator = function(value) {
      if (nullable && is.null(value)) {
        return(NULL)
      }
      if (length(value) != 1L || is.na(value)) {
        return("must be a single non-NA character scalar")
      }
      if (!value %in% values) {
        return(paste0(
          "must be one of ",
          paste(paste0('"', values, '"'), collapse = ", ")
        ))
      }
      NULL
    },
    default = default
  )
}


# %% optional ----
#' Create an optional S7 type
#'
#' Creates an S7 union type that allows for the specified type or `NULL`.
#'
#' This should be used when the S7 class already includes all the necessary validation for the
#' non-NULL case. Otherwise, create a new S7 property with appropriate validator using
#' `S7::new_property()`.
#'
#' @param type S7 base class or S7 class.
#' @return An S7 union type that allows for the specified type or `NULL`.
#' @author EDG
#' @export
#' @examples
#' # Create an optional character type
#' optional(S7::class_character)
optional <- function(type) {
  if (!inherits(type, "S7_base_class") && !inherits(type, "S7_class")) {
    abort(
      "`type` must be an S7 base class or S7 class.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  S7::new_union(NULL, type)
}
