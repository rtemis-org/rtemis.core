# prop_declaration.R
# ::rtemis.core::
# 2026- EDG rtemis.org

# TOC ----
#   prop_extend / prop_extension
#   prop_declaration / spec_declaration
#   prop_build / build_declaration
#
# A declaration in data: the canonical factory and every argument of its
# signature, nested declarations in the same form, and the fields producers
# store. `prop_build()` is its inverse.

# %% prop_extend ----
#' Store a producer's fields on a declaration
#'
#' Merges `fields` by name into the fields stored under `producer`, as
#' `utils::modifyList()` does. The fields are part of the declaration: a
#' container keeps an item that carries them as its own declaration, and
#' [prop_declaration()] returns them. They change neither validation nor
#' emission; a producer reads them with [prop_extension()] and emits them
#' through the `annotate` function of [new_JSONSchema_defs()].
#'
#' @param property S7 property: A declaration from a `prop_*` factory.
#' @param producer Character: The producer's package name.
#' @param fields Named list: The producer's fields.
#'
#' @return `property` with the fields stored.
#'
#' @author EDG
#' @export
#'
#' @examples
#' p <- prop_extend(prop_number(0.5), "mypkg", list(tunable = TRUE))
#' prop_extension(p, "mypkg")
prop_extend <- function(property, producer, fields) {
  spec <- encode_spec_of(property)
  check_character_scalar(producer)
  if (
    !is.list(fields) ||
      (length(fields) > 0L && !is.null(validate_keys(fields)))
  ) {
    abort(
      "`fields` must be a list with distinct, non-empty names.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  extensions <- spec[["extensions"]] %||% list()
  extensions[[producer]] <- utils::modifyList(
    extensions[[producer]] %||% list(),
    fields
  )
  spec[["extensions"]] <- extensions
  property[["spec"]] <- spec
  property
} # /rtemis.core::prop_extend


# %% prop_extension ----
#' Read a producer's fields from a declaration
#'
#' @param property S7 property: A declaration from a `prop_*` factory.
#' @param producer Character: The producer's package name.
#'
#' @return Named list: The fields stored under `producer` by [prop_extend()],
#'   or NULL.
#'
#' @author EDG
#' @export
#'
#' @examples
#' p <- prop_extend(prop_number(0.5), "mypkg", list(tunable = TRUE))
#' prop_extension(p, "mypkg")[["tunable"]]
prop_extension <- function(property, producer) {
  spec <- encode_spec_of(property)
  check_character_scalar(producer)
  spec[["extensions"]][[producer]]
} # /rtemis.core::prop_extension


# %% prop_declaration ----
#' A declaration as data
#'
#' Returns the canonical factory that builds `property` and every argument of
#' its signature by name, including those at their default, with nested
#' declarations in the same form and the fields producers stored with
#' [prop_extend()]. A compatibility form returns its canonical form. `default`
#' is the argument as given: a quoted call stays quoted, and a default derived
#' from `const` is NULL. `enum` and `const` are returned as stored; `min`,
#' `max`, `exclusive_min`, and `exclusive_max` are doubles for [prop_number()]
#' and integers for [prop_integer()]; `required` holds the names beyond those
#' the declarations imply, in declared order, or NULL; `cls` is the class name
#' as given, or the qualified name of a class object. Nothing is resolved.
#' Fields set on a property after its factory returned are not part of its
#' declaration.
#'
#' For every `d` that `prop_declaration()` returns,
#' `identical(prop_declaration(prop_build(d)), d)`.
#'
#' @param property S7 property: A declaration from a `prop_*` factory.
#'
#' @return Named list: `factory`, the factory name; `arguments`, a named list;
#'   `extensions`, a named list of producer fields.
#'
#' @author EDG
#' @export
#'
#' @examples
#' d <- prop_declaration(prop_array(prop_integer(min = 1L), min_items = 0L))
#' d[["factory"]]
#' d[["arguments"]][["items"]][["arguments"]][["min"]]
prop_declaration <- function(property) {
  spec <- encode_spec_of(property)
  default <- property[["default"]]
  if (!is.null(default) && identical(default, spec[["const"]])) {
    default <- NULL
  }
  spec_declaration(spec, default = default)
} # /rtemis.core::prop_declaration


# %% new_declaration ----
#' Assemble a declaration
#'
#' @param factory Character: Factory name.
#' @param arguments Named list: Arguments.
#' @param spec Named list: The spec, for its extensions.
#'
#' @return Named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
new_declaration <- function(factory, arguments, spec) {
  list(
    factory = factory,
    arguments = arguments,
    extensions = spec[["extensions"]] %||% list()
  )
} # /rtemis.core::new_declaration


# %% arg_list ----
#' A named list that keeps NULL values
#'
#' @param ... Named arguments.
#'
#' @return Named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
arg_list <- function(...) {
  list(...)
} # /rtemis.core::arg_list


# %% bound_value ----
#' A numeric bound in the type of its declaration
#'
#' @param x Numeric or NULL.
#' @param type Character: `"integer"` or `"number"`.
#'
#' @return Integer for an integer declaration whose bound is a whole number in
#'   R's integer range, double otherwise, or NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
bound_value <- function(x, type) {
  if (is.null(x)) {
    return(NULL)
  }
  if (identical(type, "integer") && abs(x) <= .Machine[["integer.max"]]) {
    as.integer(x)
  } else {
    as.double(x)
  }
} # /rtemis.core::bound_value


# %% beyond_implied ----
#' The `required` names a declaration gave beyond those it implies
#'
#' @param required Character: Stored `required`.
#' @param specs Named list of member or column specs.
#' @param implied_by Function: TRUE for a spec that implies its name.
#'
#' @return Character or NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
beyond_implied <- function(required, specs, implied_by) {
  implied <- names(specs)[vapply(specs, implied_by, logical(1L))]
  extra <- setdiff(required, implied)
  if (length(extra) == 0L) NULL else extra
} # /rtemis.core::beyond_implied


# %% spec_declaration ----
#' The declaration of a spec
#'
#' @param spec Named list of spec fields.
#' @param default Default value or quoted call, for a property of a class.
#'
#' @return Named list: `factory`, `arguments`, `extensions`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
spec_declaration <- function(spec, default = NULL) {
  shape <- spec_shape(spec)
  container <- spec[["container"]]
  nullable <- isTRUE(spec[["nullable"]])
  def_name <- spec[["def_name"]]
  description <- spec[["description"]] %||% ""
  nested <- function(x) if (is.null(x)) NULL else spec_declaration(x)
  array_tail <- function(items) {
    arg_list(
      items = items,
      default = default,
      contains = nested(spec[["contains"]]),
      min_contains = spec[["min_contains"]] %||% 1L,
      max_contains = spec[["max_contains"]],
      min_items = spec[["min_items"]] %||% 1L,
      max_items = spec[["max_items"]],
      unique_items = isTRUE(spec[["unique_items"]]),
      nullable = nullable,
      def_name = def_name,
      description = description
    )
  }
  map_tail <- function(values) {
    arg_list(
      values = values,
      default = default,
      keys = nested(spec[["keys"]]),
      min_members = spec[["min_items"]] %||% 1L,
      max_members = spec[["max_items"]],
      nullable = nullable,
      def_name = def_name,
      description = description
    )
  }
  container_of <- function(item) {
    if (identical(container, "map")) {
      new_declaration("prop_map", map_tail(item), spec)
    } else {
      new_declaration("prop_array", array_tail(item), spec)
    }
  }
  switch(
    shape,
    leaf = {
      if (identical(container, "none")) {
        scalar_declaration(spec, default)
      } else {
        container_of(spec_declaration(container_item_spec(spec)))
      }
    },
    bag = new_declaration(
      "prop_map",
      map_tail(spec_declaration(prop_spec(prop_any()))),
      spec
    ),
    collection = container_of(spec_declaration(container_item_spec(spec))),
    nested_array = new_declaration(
      "prop_array",
      array_tail(spec_declaration(spec[["items"]])),
      spec
    ),
    nested_map = new_declaration(
      "prop_map",
      map_tail(spec_declaration(spec[["values"]])),
      spec
    ),
    matrix = new_declaration(
      "prop_matrix",
      arg_list(
        items = spec_declaration(spec[["items"]]),
        ncol = spec[["ncol"]],
        default = default,
        min_items = spec[["min_items"]] %||% 1L,
        max_items = spec[["max_items"]],
        nullable = nullable,
        def_name = def_name,
        description = description
      ),
      spec
    ),
    struct = new_declaration(
      "prop_struct",
      arg_list(
        members = lapply(spec[["members"]], spec_declaration),
        default = default,
        required = beyond_implied(
          spec[["required"]],
          spec[["members"]],
          function(s) {
            !isTRUE(s[["nullable"]]) && !identical(s[["type"]], "any")
          }
        ),
        additional_members = if (is.null(spec[["additional_members"]])) {
          FALSE
        } else {
          spec_declaration(spec[["additional_members"]])
        },
        min_members = spec[["min_members"]] %||% 0L,
        max_members = spec[["max_members"]],
        nullable = nullable,
        def_name = def_name,
        description = description
      ),
      spec
    ),
    table = new_declaration(
      "prop_table",
      arg_list(
        columns = lapply(spec[["columns"]], spec_declaration),
        default = default,
        required = beyond_implied(
          spec[["required"]],
          spec[["columns"]],
          function(s) !isTRUE(s[["nullable"]])
        ),
        additional_columns = if (is.null(spec[["additional_columns"]])) {
          FALSE
        } else {
          spec_declaration(spec[["additional_columns"]])
        },
        min_items = spec[["min_items"]] %||% 1L,
        max_items = spec[["max_items"]],
        nullable = nullable,
        def_name = def_name,
        description = description
      ),
      spec
    ),
    factor = new_declaration(
      "prop_factor",
      arg_list(
        default = default,
        enum = spec[["enum"]],
        nullable_codes = isTRUE(spec[["nullable_codes"]]),
        min_items = spec[["min_items"]] %||% 1L,
        max_items = spec[["max_items"]],
        nullable = nullable,
        def_name = def_name,
        description = description
      ),
      spec
    ),
    union = new_declaration(
      "prop_union",
      arg_list(
        alternatives = lapply(spec[["alternatives"]], spec_declaration),
        default = default,
        nullable = nullable,
        def_name = def_name,
        description = description
      ),
      spec
    ),
    object = new_declaration(
      "prop_instance",
      arg_list(
        cls = given_class_name(spec),
        default = default,
        members = if (is.null(spec[["members"]])) {
          NULL
        } else {
          lapply(spec[["members"]], spec_declaration)
        },
        nullable = nullable,
        description = description
      ),
      spec
    ),
    any = new_declaration(
      "prop_any",
      arg_list(default = default, description = description),
      spec
    )
  )
} # /rtemis.core::spec_declaration


# %% scalar_declaration ----
#' The declaration of a scalar spec
#'
#' @param spec Named list of spec fields.
#' @param default Default value or quoted call.
#'
#' @return Named list: `factory`, `arguments`, `extensions`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
scalar_declaration <- function(spec, default) {
  type <- spec[["type"]]
  tail <- arg_list(
    nullable = isTRUE(spec[["nullable"]]),
    def_name = spec[["def_name"]],
    description = spec[["description"]] %||% ""
  )
  args <- switch(
    type,
    boolean = c(arg_list(default = default, const = spec[["const"]]), tail),
    integer = ,
    number = c(
      arg_list(
        default = default,
        enum = spec[["enum"]],
        const = spec[["const"]],
        min = bound_value(spec[["minimum"]], type),
        max = bound_value(spec[["maximum"]], type),
        exclusive_min = bound_value(spec[["exclusive_minimum"]], type),
        exclusive_max = bound_value(spec[["exclusive_maximum"]], type)
      ),
      tail
    ),
    string = c(
      arg_list(
        default = default,
        enum = spec[["enum"]],
        const = spec[["const"]],
        pattern = spec[["pattern"]],
        min_length = spec[["min_length"]] %||% 0L,
        max_length = spec[["max_length"]]
      ),
      tail
    )
  )
  new_declaration(paste0("prop_", type), args, spec)
} # /rtemis.core::scalar_declaration


# %% prop_build ----
#' Build a property from its declaration
#'
#' The inverse of [prop_declaration()]: calls the declared factory with the
#' declared arguments, building nested declarations first, and stores the
#' declared producer fields. A quoted default is evaluated, and a class named
#' `"Class"` is resolved, in `env`.
#'
#' @param declaration Named list: A declaration, as [prop_declaration()]
#'   returns.
#' @param env Environment: Where a quoted default is evaluated and a class
#'   name is written.
#'
#' @return S7 property.
#'
#' @author EDG
#' @export
#'
#' @examples
#' d <- prop_declaration(prop_number(0.5, min = 0, max = 1))
#' d[["arguments"]][["max"]] <- 2
#' prop_spec(prop_build(d))[["maximum"]]
prop_build <- function(declaration, env = parent.frame()) {
  if (!is_declaration(declaration)) {
    abort(
      "`declaration` must be a list of `factory`, `arguments`, and ",
      "`extensions`, as prop_declaration() returns.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (!is.environment(env)) {
    abort(
      "`env` must be an environment.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  build_declaration(declaration, env)
} # /rtemis.core::prop_build


# %% DECLARATION_FACTORIES ----
# The canonical factories a declaration names.
DECLARATION_FACTORIES <- c(
  "prop_boolean",
  "prop_integer",
  "prop_number",
  "prop_string",
  "prop_array",
  "prop_map",
  "prop_matrix",
  "prop_struct",
  "prop_table",
  "prop_factor",
  "prop_union",
  "prop_instance",
  "prop_any"
)


# %% is_declaration ----
#' Whether a value is a declaration
#'
#' @param x Value.
#'
#' @return Logical.
#'
#' @author EDG
#' @keywords internal
#' @noRd
is_declaration <- function(x) {
  is.list(x) &&
    identical(names(x), c("factory", "arguments", "extensions")) &&
    is.character(x[["factory"]]) &&
    length(x[["factory"]]) == 1L &&
    x[["factory"]] %in% DECLARATION_FACTORIES &&
    is.list(x[["arguments"]]) &&
    is.list(x[["extensions"]])
} # /rtemis.core::is_declaration


# %% build_declaration ----
#' Build a property from a checked declaration
#'
#' @param d Named list: Declaration.
#' @param env Environment: Where the factory is called.
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
build_declaration <- function(d, env) {
  args <- d[["arguments"]]
  build <- function(x) {
    if (is_declaration(x)) build_declaration(x, env) else x
  }
  for (nm in c(
    "items",
    "values",
    "contains",
    "keys",
    "additional_members",
    "additional_columns"
  )) {
    if (!is.null(args[[nm]])) {
      args[[nm]] <- build(args[[nm]])
    }
  }
  for (nm in c("members", "alternatives", "columns")) {
    if (!is.null(args[[nm]])) {
      args[[nm]] <- lapply(args[[nm]], build)
    }
  }
  f <- get(d[["factory"]], envir = asNamespace("rtemis.core"), inherits = FALSE)
  p <- do.call(f, args, quote = TRUE, envir = env)
  for (producer in names(d[["extensions"]])) {
    p <- prop_extend(p, producer, d[["extensions"]][[producer]])
  }
  p
} # /rtemis.core::build_declaration
