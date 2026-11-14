# encode.R
# ::rtemis.core::
# 2026- EDG rtemis.org

# TOC ----
#   to_JSON / json_doubles / shortest_doubles
#   prop_encode / encode_spec
#   prop_decode / decode_spec / decode_scalars
#
# The JSON form of every factory-built value, as the rule's "Values in JSON"
# table fixes it. `prop_encode()` returns the R value that `to_JSON()` writes;
# `prop_decode()` reads the value that `jsonlite::fromJSON(simplifyVector =
# FALSE)` returns.

# %% to_JSON ----
#' Write an R value as JSON
#'
#' Writes every JSON document in the rtemis ecosystem: configs, records,
#' results, chart configs, and schemas. Each double is written as the shortest
#' of its 15-, 16-, and 17-significant-digit forms that reads back identical,
#' so `0.1` is written `0.1` and `0.1 + 0.2` is written `0.30000000000000004`.
#' A length-1 vector is written as a scalar, `NULL` and `NA` as `null`, and a
#' value wrapped in `I()` as an array.
#'
#' @param x R value: A list, vector, or scalar.
#' @param pretty Logical: If TRUE, indent the output.
#'
#' @return Character: The JSON text.
#'
#' @author EDG
#' @export
#'
#' @examplesIf requireNamespace("jsonlite", quietly = TRUE)
#' to_JSON(list(alpha = 0.1, lambda = 0.1 + 0.2, n = 10L, tags = I("a")))
to_JSON <- function(x, pretty = FALSE) {
  check_dependencies("jsonlite")
  check_logical_scalar(pretty)
  as.character(jsonlite::toJSON(
    json_doubles(x),
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    json_verbatim = TRUE,
    digits = I(17L),
    pretty = pretty
  ))
} # /rtemis.core::to_JSON


# %% json_doubles ----
#' Replace doubles with their shortest exact JSON text
#'
#' Walks lists, and replaces each plain double vector and double matrix with
#' `json`-classed strings that `jsonlite::toJSON(json_verbatim = TRUE)` writes
#' as they are. A double scalar becomes one string; a vector of any other
#' length, or one wrapped in `I()`, becomes a list, which is written as an
#' array, and a matrix becomes a list of rows. Missing and non-finite values
#' are left for jsonlite.
#'
#' @param x R value.
#'
#' @return R value of the same JSON form.
#'
#' @author EDG
#' @keywords internal
#' @noRd
json_doubles <- function(x) {
  if (is.list(x) && !is.data.frame(x)) {
    if (length(x) > 0L) {
      x[] <- lapply(x, json_doubles)
    }
    return(x)
  }
  cls <- oldClass(x)
  if (!is.double(x) || !(is.null(cls) || identical(cls, "AsIs"))) {
    return(x)
  }
  dims <- dim(x)
  if (length(dims) > 2L) {
    return(x)
  }
  elements <- function(v) {
    out <- as.list(v)
    ok <- is.finite(v)
    if (any(ok)) {
      out[ok] <- lapply(
        shortest_doubles(v[ok]),
        structure,
        class = "json"
      )
    }
    names(out) <- NULL
    out
  }
  if (length(dims) == 2L) {
    v <- unclass(x)
    return(lapply(seq_len(dims[[1L]]), function(i) elements(v[i, ])))
  }
  v <- as.vector(unclass(x))
  if (length(v) == 1L && is.null(cls)) {
    return(elements(v)[[1L]])
  }
  elements(v)
} # /rtemis.core::json_doubles


# %% shortest_doubles ----
#' Shortest decimal text of each double that reads back identical
#'
#' Tries 15, 16, and 17 significant digits in turn, and checks each form with
#' jsonlite's parser, the parser that reads the document back.
#'
#' @param v Double: Finite values.
#'
#' @return Character, one form per value.
#'
#' @author EDG
#' @keywords internal
#' @noRd
shortest_doubles <- function(v) {
  out <- character(length(v))
  todo <- seq_along(v)
  for (digits in 15:17) {
    text <- sprintf("%.*g", digits, v[todo])
    back <- as.double(unlist(jsonlite::parse_json(
      paste0("[", paste(text, collapse = ","), "]")
    )))
    ok <- if (digits == 17L) rep(TRUE, length(todo)) else back == v[todo]
    out[todo[ok]] <- text[ok]
    todo <- todo[!ok]
    if (length(todo) == 0L) {
      break
    }
  }
  out
} # /rtemis.core::shortest_doubles


# %% prop_encode ----
#' Encode a property value for JSON
#'
#' Returns the R value that [to_JSON()] writes as the JSON form of `value`,
#' following the property's declaration: an array is written as an array at
#' every length, a map or struct as an object (`{}` when empty), a matrix as an
#' array of rows, a `NULL` struct member as `null`, and an S7 instance as an
#' object of its properties. A string's `class` is dropped.
#'
#' @param property S7 property: A declaration from a `prop_*` factory.
#' @param value Value of the property. Aborts when it does not satisfy the
#'   declaration.
#'
#' @return R value to pass to [to_JSON()].
#'
#' @author EDG
#' @export
#'
#' @examplesIf requireNamespace("jsonlite", quietly = TRUE)
#' tags <- prop_array(prop_string())
#' to_JSON(prop_encode(tags, "a"))
prop_encode <- function(property, value) {
  spec <- encode_spec_of(property)
  check_encodable(value, spec)
  encode_spec(value, spec)
} # /rtemis.core::prop_encode


# %% encode_spec_of ----
#' The declaration of a property, for encoding and decoding
#'
#' @param property S7 property.
#'
#' @return Named list of spec fields. Aborts for a property without one.
#'
#' @author EDG
#' @keywords internal
#' @noRd
encode_spec_of <- function(property) {
  spec <- prop_spec(property)
  if (is.null(spec)) {
    abort(
      "`property` must be built by a prop_* factory.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  spec
} # /rtemis.core::encode_spec_of


# %% check_encodable ----
#' Abort when a value does not satisfy its declaration
#'
#' @param value Value.
#' @param spec Named list of spec fields.
#'
#' @return NULL, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_encodable <- function(value, spec) {
  msg <- validate_with_spec(value, spec)
  if (!is.null(msg)) {
    abort(
      "`value` ",
      msg,
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  invisible(NULL)
} # /rtemis.core::check_encodable


# %% named_list ----
#' A list with names, written as a JSON object even when empty
#'
#' Assigns by position, so `NULL` elements stay in the list.
#'
#' @param values List: Elements.
#' @param keys Character: Names.
#'
#' @return Named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
named_list <- function(values, keys) {
  out <- vector("list", length(values))
  for (i in seq_along(values)) {
    if (!is.null(values[[i]])) {
      out[[i]] <- values[[i]]
    }
  }
  names(out) <- keys %||% character(0L)
  out
} # /rtemis.core::named_list


# %% scalar_vector ----
#' A scalar-layout value without the attributes jsonlite would write
#'
#' @param value Atomic vector.
#'
#' @return Atomic vector without attributes.
#'
#' @author EDG
#' @keywords internal
#' @noRd
scalar_vector <- function(value) {
  out <- as.vector(unclass(value))
  attributes(out) <- NULL
  out
} # /rtemis.core::scalar_vector


# %% encode_spec ----
#' Encode a valid value by its spec
#'
#' @param value Value that satisfies `spec`.
#' @param spec Named list of spec fields.
#'
#' @return R value to pass to [to_JSON()].
#'
#' @author EDG
#' @keywords internal
#' @noRd
encode_spec <- function(value, spec) {
  shape <- spec_shape(spec)
  if (shape == "any") {
    return(value)
  }
  if (is.null(value)) {
    return(NULL)
  }
  container <- spec[["container"]]
  switch(
    shape,
    leaf = {
      v <- scalar_vector(value)
      if (identical(container, "none")) {
        v
      } else if (identical(container, "map")) {
        named_list(as.list(v), names(value))
      } else {
        I(v)
      }
    },
    bag = named_list(as.list(value), names(value)),
    matrix = {
      v <- unclass(value)
      attributes(v) <- NULL
      dim(v) <- dim(value)
      I(lapply(seq_len(nrow(v)), function(i) I(v[i, ])))
    },
    nested_array = ,
    nested_map = {
      item <- spec[["items"]] %||% spec[["values"]]
      map <- shape == "nested_map"
      if (!is.null(scalar_items(spec, item))) {
        v <- scalar_vector(value)
        if (map) named_list(as.list(v), names(value)) else I(v)
      } else {
        out <- lapply(value, function(e) encode_spec(e, item))
        if (map) named_list(out, names(value)) else I(unname_list(out))
      }
    },
    collection = {
      element <- spec
      element[["container"]] <- "none"
      element[["nullable"]] <- FALSE
      out <- lapply(value, function(e) encode_spec(e, element))
      if (identical(container, "map")) {
        named_list(out, names(value))
      } else {
        I(unname_list(out))
      }
    },
    struct = {
      members <- spec[["members"]]
      keys <- names(value) %||% character(0L)
      out <- lapply(seq_along(value), function(i) {
        member <- members[[keys[[i]]]] %||% spec[["additional_members"]]
        encode_spec(value[[i]], member)
      })
      named_list(out, keys)
    },
    object = encode_instance(value),
    union = {
      for (alt in spec[["alternatives"]]) {
        if (is.null(validate_with_spec(value, alt))) {
          return(encode_spec(value, alt))
        }
      }
    }
  )
} # /rtemis.core::encode_spec


# %% unname_list ----
#' A list without names, keeping NULL elements
#'
#' @param x List.
#'
#' @return Unnamed list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
unname_list <- function(x) {
  names(x) <- NULL
  x
} # /rtemis.core::unname_list


# %% encode_instance ----
#' Encode an S7 instance as an object of its properties
#'
#' @param value S7 object whose every property is built by a factory.
#'
#' @return Named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
encode_instance <- function(value) {
  props <- S7::S7_class(value)@properties
  keys <- names(props)
  out <- lapply(keys, function(nm) {
    spec <- prop_spec(props[[nm]])
    if (is.null(spec)) {
      abort(
        "Property '",
        nm,
        "' of ",
        S7::S7_class(value)@name,
        " is not built by a prop_* factory, so it has no JSON form.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    encode_spec(S7::prop(value, nm), spec)
  })
  named_list(out, keys)
} # /rtemis.core::encode_instance


# %% prop_decode ----
#' Decode a property value from JSON
#'
#' Reads the value that `jsonlite::fromJSON(json, simplifyVector = FALSE)`
#' returns for one property, and returns the R value its declaration admits:
#' an array of non-nullable scalars as an atomic vector, a map as a named
#' vector or list, a matrix by row, a number as a double, an integer within
#' -2147483647 to 2147483647 as an integer, and an S7 instance by passing its
#' present members to the class constructor. An absent struct member stays
#' absent, and `null` reads as a present `NULL` member. A union reads as its
#' first alternative whose JSON form matches.
#'
#' @param property S7 property: A declaration from a `prop_*` factory.
#' @param x Value parsed by `jsonlite::fromJSON(simplifyVector = FALSE)`.
#'
#' @return R value of the property. Aborts with `rtemis_type_error` or
#'   `rtemis_value_error` when `x` has no value under the declaration.
#'
#' @author EDG
#' @export
#'
#' @examplesIf requireNamespace("jsonlite", quietly = TRUE)
#' tags <- prop_array(prop_string())
#' prop_decode(tags, jsonlite::fromJSON('["a", "b"]', simplifyVector = FALSE))
prop_decode <- function(property, x) {
  spec <- encode_spec_of(property)
  value <- decode_spec(x, spec)
  check_decoded(value, spec)
  value
} # /rtemis.core::prop_decode


# %% check_decoded ----
#' Abort when a decoded value does not satisfy its declaration
#'
#' @param value Decoded value.
#' @param spec Named list of spec fields.
#'
#' @return NULL, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_decoded <- function(value, spec) {
  msg <- validate_with_spec(value, spec)
  if (!is.null(msg)) {
    abort(
      "The decoded value ",
      msg,
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  invisible(NULL)
} # /rtemis.core::check_decoded


# %% decode_error ----
#' Abort with a decoding type error
#'
#' @param what Character: What the JSON value must be.
#'
#' @return Does not return.
#'
#' @author EDG
#' @keywords internal
#' @noRd
decode_error <- function(what) {
  abort(
    "The JSON value must be ",
    what,
    ".",
    class = c("rtemis_type_error", "rtemis_input_error")
  )
} # /rtemis.core::decode_error


# %% is_json_object ----
#' Whether a parsed value is a JSON object
#'
#' `jsonlite::fromJSON(simplifyVector = FALSE)` returns an object as a list
#' with names, empty names included, and an array as a list without them.
#'
#' @param x Parsed value.
#'
#' @return Logical.
#'
#' @author EDG
#' @keywords internal
#' @noRd
is_json_object <- function(x) {
  is.list(x) && !is.null(names(x))
} # /rtemis.core::is_json_object


# %% decode_scalars ----
#' Convert parsed JSON scalars to an atomic vector of a scalar type
#'
#' @param x List of parsed scalars, or one parsed scalar.
#' @param type Character: Scalar type.
#' @param na Logical: If TRUE, a JSON `null` element is a missing element and
#'   becomes `NA` (grammar 4).
#'
#' @return Atomic vector of `type`, attribute-free.
#'
#' @author EDG
#' @keywords internal
#' @noRd
decode_scalars <- function(x, type, na = FALSE) {
  parts <- if (is.list(x)) x else list(x)
  missing <- vapply(parts, is.null, logical(1L))
  if (na && any(missing)) {
    parts[missing] <- list(NA)
  }
  ok <- vapply(
    parts,
    function(e) is.atomic(e) && length(e) == 1L,
    logical(1L)
  )
  if (!all(ok)) {
    decode_error(paste0(type, " values"))
  }
  v <- unlist(parts, use.names = FALSE)
  present <- v[!is.na(v)]
  check <- function(ok, what) {
    if (length(present) > 0L && !ok) {
      decode_error(what)
    }
  }
  switch(
    type,
    boolean = {
      check(is.logical(present), "true or false")
      as.logical(v)
    },
    integer = {
      check(is.numeric(present), "an integer")
      whole <- as.double(present)
      if (any(whole != round(whole) | abs(whole) > 2147483647)) {
        decode_error("a whole number from -2147483647 to 2147483647")
      }
      as.integer(v)
    },
    number = {
      check(is.numeric(present), "a number")
      as.double(v)
    },
    string = {
      check(is.character(present), "a string")
      as.character(v)
    }
  )
} # /rtemis.core::decode_scalars


# %% decode_spec ----
#' Decode a parsed JSON value by its spec
#'
#' @param x Parsed value.
#' @param spec Named list of spec fields.
#'
#' @return R value.
#'
#' @author EDG
#' @keywords internal
#' @noRd
decode_spec <- function(x, spec) {
  shape <- spec_shape(spec)
  if (shape == "any") {
    return(x)
  }
  if (is.null(x)) {
    return(NULL)
  }
  container <- spec[["container"]]
  type <- spec[["type"]]
  switch(
    shape,
    leaf = {
      if (identical(container, "none")) {
        if (is.list(x)) {
          decode_error(paste("a", type))
        }
        decode_scalars(x, type)
      } else if (identical(container, "map")) {
        if (!is_json_object(x)) {
          decode_error("an object")
        }
        stats::setNames(decode_scalars(x, type), names(x))
      } else {
        if (!is.list(x) || is_json_object(x)) {
          decode_error("an array")
        }
        decode_scalars(x, type)
      }
    },
    bag = {
      if (!is_json_object(x)) {
        decode_error("an object")
      }
      x
    },
    matrix = {
      if (!is.list(x) || is_json_object(x)) {
        decode_error("an array of rows")
      }
      n_col <- spec[["ncol"]] %||%
        if (length(x) > 0L && is.list(x[[1L]])) length(x[[1L]]) else 0L
      na <- isTRUE(spec[["items"]][["nullable"]])
      rows <- lapply(x, function(r) {
        if (!is.list(r) || length(r) != n_col) {
          decode_error(paste0("an array of ", n_col, "-element arrays"))
        }
        decode_scalars(r, type, na = na)
      })
      empty <- if (type == "integer") integer(0L) else double(0L)
      matrix(
        unlist(c(list(empty), rows), use.names = FALSE),
        ncol = n_col,
        byrow = TRUE
      )
    },
    nested_array = ,
    nested_map = {
      item <- spec[["items"]] %||% spec[["values"]]
      map <- shape == "nested_map"
      if (map && !is_json_object(x)) {
        decode_error("an object")
      }
      if (!map && (!is.list(x) || is_json_object(x))) {
        decode_error("an array")
      }
      if (!is.null(scalar_items(spec, item))) {
        v <- decode_scalars(x, item[["type"]], na = isTRUE(item[["nullable"]]))
        if (map) stats::setNames(v, names(x)) else v
      } else {
        out <- lapply(x, function(e) decode_spec(e, item))
        if (map) named_list(out, names(x)) else unname_list(out)
      }
    },
    collection = {
      map <- identical(container, "map")
      if (map && !is_json_object(x)) {
        decode_error("an object")
      }
      if (!map && (!is.list(x) || is_json_object(x))) {
        decode_error("an array")
      }
      element <- spec
      element[["container"]] <- "none"
      element[["nullable"]] <- FALSE
      out <- lapply(x, function(e) decode_spec(e, element))
      if (map) named_list(out, names(x)) else unname_list(out)
    },
    struct = {
      if (!is_json_object(x)) {
        decode_error("an object")
      }
      members <- spec[["members"]]
      keys <- names(x)
      out <- lapply(seq_along(x), function(i) {
        member <- members[[keys[[i]]]] %||% spec[["additional_members"]]
        if (is.null(member)) {
          return(x[[i]])
        }
        decode_spec(x[[i]], member)
      })
      named_list(out, keys)
    },
    object = decode_instance(x, spec_target(spec)),
    union = {
      for (alt in spec[["alternatives"]]) {
        # Any error, the validation error of an instance's constructor
        # included, means the JSON form does not match this alternative.
        value <- tryCatch(
          decode_spec(x, alt),
          error = function(e) {
            NULL
          }
        )
        if (!is.null(value) && is.null(validate_with_spec(value, alt))) {
          return(value)
        }
      }
      decode_error("a value of one of the union's alternatives")
    }
  )
} # /rtemis.core::decode_spec


# %% decode_instance ----
#' Decode an S7 instance from a JSON object
#'
#' Passes the present members to the class constructor, so the class's
#' defaults apply to absent members.
#'
#' @param x Parsed JSON object.
#' @param cls S7 class.
#'
#' @return S7 object of `cls`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
decode_instance <- function(x, cls) {
  if (!is_json_object(x)) {
    decode_error("an object")
  }
  props <- cls@properties
  unknown <- setdiff(names(x), names(props))
  if (length(unknown) > 0L) {
    abort(
      cls@name,
      " has no propert",
      if (length(unknown) > 1L) "ies " else "y ",
      paste0("'", unknown, "'", collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  args <- lapply(names(x), function(nm) {
    spec <- prop_spec(props[[nm]])
    if (is.null(spec)) {
      abort(
        "Property '",
        nm,
        "' of ",
        cls@name,
        " is not built by a prop_* factory, so it has no JSON form.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    decode_spec(x[[nm]], spec)
  })
  do.call(cls, named_list(args, names(x)))
} # /rtemis.core::decode_instance
