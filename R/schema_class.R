# schema_class.R
# ::rtemis.core::
# 2026- EDG rtemis.org

# Class-level metadata: the description of an S7 class, its registry
# publication, and fields stored by producer packages. `class_to_JSONSchema()`
# writes the description of every class it defines and the publication of the
# root class.

# %% SCHEMA_CLASS_ATTRIBUTE ----
# Attribute of a class's validator that holds the class's metadata environment.
SCHEMA_CLASS_ATTRIBUTE <- "rtemis_schema_class"


# %% SchemaPublication ----
#' Registry publication of a class's schema
#'
#' @description
#' The identity under which the schema generated from a class is published to
#' the schema registry at schema.rtemis.org. [class_to_JSONSchema()] writes it
#' as the root member `x-rtemis.publication`, together with the producer
#' package, the generating language, and the qualified class name.
#'
#' @param scope Character \{"shared", "implementation"\}: `"shared"` for a
#'   contract that every implementation reads and writes, `"implementation"`
#'   for a contract owned by one implementation.
#' @param domain Character \{"ml", "harness", "domain"\}: Registry subject area:
#'   machine learning, the execution harness, or a domain
#'   data format.
#' @param role Character \{"family", "leaf", "document"\}: `"family"` for the
#'   root of a set of variants selected by a discriminator, `"leaf"` for one
#'   variant, and `"document"` for a standalone document.
#' @param kind Character \{"config", "pipeline", "report", "component",
#'   "data"\}: What a document of the class is: settings for a run, settings
#'   for a run that chains steps and writes a record, the output of a run, a
#'   part embedded in other documents, or domain data about an entity.
#' @param order Integer \[1, Inf): Presentation order within the registry.
#'
#' @return `SchemaPublication` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' SchemaPublication(scope = "shared", domain = "domain", kind = "data")
SchemaPublication <- new_class(
  name = "SchemaPublication",
  package = "rtemis.core",
  properties = list(
    scope = prop_string(
      "implementation",
      enum = c("shared", "implementation"),
      description = "Whether every implementation or one implementation owns the contract."
    ),
    domain = prop_string(
      enum = c("ml", "harness", "domain"),
      description = "Registry subject area."
    ),
    role = prop_string(
      "document",
      enum = c("family", "leaf", "document"),
      description = "Publication role of the class."
    ),
    kind = prop_string(
      enum = c("config", "pipeline", "report", "component", "data"),
      description = "What a document of the class is."
    ),
    order = prop_integer(
      1L,
      min = 1L,
      description = "Presentation order within the registry."
    )
  )
) # /rtemis.core::SchemaPublication


# %% no_problems ----
# The validator of a schema class declared without one. Defined at the top level
# so that its environment is the namespace.
no_problems <- function(self) NULL


# %% schema_class ----
#' Define an S7 class with a description and an optional registry publication
#'
#' @description
#' Calls [S7::new_class()] and stores the class's description, its publication,
#' and the fields of producer packages with the class.
#' [class_to_JSONSchema()] writes the description into the class's schema and
#' the publication into the root of a schema generated from the class.
#'
#' @details
#' The metadata belongs to the class it is declared on: a subclass declares its
#' own. It is held in one environment attached to the class's validator, so a
#' class object, its copies, and `S7::S7_class()` of an instance read the same
#' metadata. A class declared without a validator receives one that reports no
#' problems.
#'
#' A producer package stores fields of its own under its package name in
#' `extensions` and reads them with [class_extension()]. They change neither
#' validation nor emission.
#'
#' @param ... Arguments passed to [S7::new_class()].
#' @param description Character: What an object of the class is, as one or more
#'   sentences. Published in the class's JSON Schema, so it names no R
#'   construct ([assert_description_language()]).
#' @param publication Optional `SchemaPublication`: The registry publication of
#'   the schema generated from the class. Requires the `package` argument of
#'   [S7::new_class()].
#' @param extensions Named list: Fields stored by producer packages, one named
#'   list per producer, named by the producer's package name.
#'
#' @return S7 class.
#'
#' @author EDG
#' @export
#'
#' @examples
#' Point <- schema_class(
#'   "Point",
#'   package = "example",
#'   properties = list(
#'     x = prop_number(description = "Horizontal coordinate."),
#'     y = prop_number(description = "Vertical coordinate.")
#'   ),
#'   description = "A point in the plane.",
#'   publication = SchemaPublication(scope = "shared", domain = "domain", kind = "data")
#' )
#' class_description(Point)
#' class_publication(Point)
schema_class <- function(
  ...,
  description,
  publication = NULL,
  extensions = list()
) {
  check_character_scalar(description)
  if (!is.null(publication)) {
    if (!S7_inherits(publication, SchemaPublication)) {
      abort(
        "`publication` must be a SchemaPublication object.",
        class = c("rtemis_type_error", "rtemis_input_error")
      )
    }
  }
  if (
    !is.list(extensions) ||
      (length(extensions) > 0L &&
        (is.null(names(extensions)) ||
          anyNA(names(extensions)) ||
          !all(nzchar(names(extensions))) ||
          anyDuplicated(names(extensions)) > 0L ||
          !all(vapply(extensions, is.list, logical(1L)))))
  ) {
    abort(
      "`extensions` must be a list of lists, named by distinct producer packages.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  args <- list(...)
  name <- if (is.null(names(args)) || !nzchar(names(args)[[1L]])) {
    args[[1L]]
  } else {
    args[["name"]]
  }
  assert_description_language(list(description = description), name)
  if (!is.null(publication)) {
    package <- args[["package"]]
    if (!is.character(package) || length(package) != 1L || !nzchar(package)) {
      abort(
        "A published class must declare its `package`.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
  metadata <- new.env(parent = emptyenv())
  metadata[["description"]] <- description
  metadata[["publication"]] <- publication
  metadata[["extensions"]] <- extensions
  lockEnvironment(metadata, bindings = TRUE)
  validator <- args[["validator"]] %||% no_problems
  attr(validator, SCHEMA_CLASS_ATTRIBUTE) <- metadata
  args[["validator"]] <- validator
  # The caller's frame becomes the environment of the S7 constructor, as it
  # does for a direct call to `new_class()`.
  do.call(new_class, args, envir = parent.frame())
} # /rtemis.core::schema_class


# %% class_metadata_store ----
#' The metadata environment of a class declared by `schema_class()`
#'
#' @param cls S7 class.
#'
#' @return Environment, or NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
class_metadata_store <- function(cls) {
  check_target_class(cls)
  if (!is.function(cls@validator)) {
    return(NULL)
  }
  attr(cls@validator, SCHEMA_CLASS_ATTRIBUTE, exact = TRUE)
} # /rtemis.core::class_metadata_store


# %% class_description ----
#' Read the metadata of a class declared by `schema_class()`
#'
#' @description
#' `class_description()` returns the class's description,
#' `class_publication()` its registry publication, and `class_extension()` the
#' fields a producer package stored with it. Each reads the class it is given;
#' metadata does not inherit.
#'
#' @param cls S7 class.
#' @param producer Character: Package name of the producer.
#'
#' @return `class_description()`: Character, or NULL for a class not declared by
#'   [schema_class()]. `class_publication()`: `SchemaPublication`, or NULL for an
#'   unpublished class. `class_extension()`: Named list, or NULL when `producer`
#'   stored no fields.
#'
#' @author EDG
#' @export
#'
#' @examples
#' Point <- schema_class(
#'   "Point",
#'   properties = list(x = prop_number(), y = prop_number()),
#'   description = "A point in the plane.",
#'   extensions = list(example = list(units = "mm"))
#' )
#' class_description(Point)
#' class_publication(Point)
#' class_extension(Point, "example")
class_description <- function(cls) {
  store <- class_metadata_store(cls)
  if (is.null(store)) NULL else store[["description"]]
} # /rtemis.core::class_description


# %% class_publication ----
#' @rdname class_description
#' @export
class_publication <- function(cls) {
  store <- class_metadata_store(cls)
  if (is.null(store)) NULL else store[["publication"]]
} # /rtemis.core::class_publication


# %% class_extension ----
#' @rdname class_description
#' @export
class_extension <- function(cls, producer) {
  check_character_scalar(producer)
  store <- class_metadata_store(cls)
  if (is.null(store)) NULL else store[["extensions"]][[producer]]
} # /rtemis.core::class_extension


# %% publication_annotation ----
#' The `x-rtemis.publication` member of a published class's schema
#'
#' A `"document"` class whose parent is a published `"document"` class names
#' the parent in `parent`.
#'
#' @param cls S7 class.
#'
#' @return Named list, or NULL for an unpublished class.
#'
#' @author EDG
#' @keywords internal
#' @noRd
publication_annotation <- function(cls) {
  publication <- class_publication(cls)
  if (is.null(publication)) {
    return(NULL)
  }
  out <- list(
    producer = cls@package,
    language = "r",
    scope = publication@scope,
    domain = publication@domain,
    class = paste0(cls@package, "::", cls@name),
    role = publication@role,
    kind = publication@kind,
    order = publication@order
  )
  parent <- cls@parent
  parent_publication <- if (inherits(parent, "S7_class")) {
    class_publication(parent)
  }
  if (
    publication@role == "document" &&
      !is.null(parent_publication) &&
      parent_publication@role == "document"
  ) {
    out[["parent"]] <- paste0(parent@package, "::", parent@name)
  }
  out
} # /rtemis.core::publication_annotation
