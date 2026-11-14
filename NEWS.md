# rtemis.core 0.5.0

- The `prop_*` factories follow one grammar: one factory per JSON Schema node, one argument per keyword, and containers by composition.
- New structural factories: `prop_array()`, `prop_map()`, `prop_struct()`, `prop_table()`, `prop_factor()`, `prop_union()`, `prop_matrix()`, `prop_instance()`, and `prop_any()`.
- `prop_instance()` takes an S7 class or a class name, `"pkg::Class"` or `"Class"`, resolved at first use, so a class may refer to itself or to a class declared later; `check_class_references()` resolves every name a package's classes use. An instance has no implicit default. `members` constrains named properties of the instance at one use.
- `prop_array()` gains `contains`, `min_contains`, and `max_contains`; `prop_map()` gains `keys`; `prop_struct()` gains `min_members` and `max_members`.
- A union whose alternatives are all instances decodes by its discriminator.
- New `new_JSONSchema_defs()`, `prop_to_JSONSchema()`, and `JSONSchema_defs()` emit schema fragments for a producer that assembles its own class objects; `refs` maps a class or a `def_name` to another document, `annotate` adds annotations at each declaration, and `class_schema` supplies the object schema of a class.
- New `prop_extend()` and `prop_extension()` store and read a producer's fields on a declaration; `prop_declaration()` returns a declaration as data, and `prop_build()` builds it.
- A bound of `prop_integer()` is a whole number.
- New `prop_number()`; `prop_float()` remains for released callers.
- Arguments after `default`, or after a structural factory's structure arguments, are passed by name.
- `enum` applies to integer and number declarations, and `const` to every scalar declaration; `prop_const()` remains for released callers.
- `prop_string()` gains `pattern`, `min_length`, and `max_length`; `min_length = 0L` admits `""`, and a string of spaces is valid unless a `pattern` excludes it.
- `prop_boolean()` has no default unless given, constant, or nullable.
- A value carries no attribute its declaration excludes: a factor, a `Date`, a named scalar, or a subclass instance is invalid; a string may carry a class.
- A struct member and an array, map, or union item take no `default`.
- An array or map of nullable scalars, and a matrix of nullable cells, holds `NA` for a missing element, written as JSON `null`; `NULL` remains the absence of a whole value.
- `prop_matrix()` takes `ncol = NULL` for a matrix of any width.
- `vector`, `map`, and `allow_empty` of the scalar factories, and `prop_bag()`, remain for released callers.
- New `class_to_JSONSchema()` emits a JSON Schema (draft 2020-12) from an S7 class built with the `prop_*` factories.
- New `schema_class()` declares an S7 class with a description and an optional `SchemaPublication`; `class_description()`, `class_publication()`, and `class_extension()` read them.
- `class_to_JSONSchema()` writes each class's description and the `x-rtemis.publication` of a published root class. Each class entry in `$defs` carries `x-rtemis.class`, and each matrix, table, and factor fragment `x-rtemis.node`; a nullable union adds `{"type": "null"}` to its own `anyOf`.
- New `to_JSON()` writes each double in the shortest form that reads back identical, and a data frame as an array of row objects; `write_JSONSchema()` writes through it.
- New `prop_encode()` and `prop_decode()` give the JSON form of a property value.
- New `nullable()` and `check_nullable_*()`; `optional()` and `check_optional_*()` remain for released callers.
- A map key is valid when it is non-missing, non-empty, and distinct.
- Removed the hand-written properties (`character_scalar`, `prob_scalar`, their `optional_` forms, and the rest) except `optional_logical_scalar`. `enum()` and `optional_logical_scalar` remain for released callers.
- `printls()` names the class of an object it does not expand.

# rtemis.core 0.4.7

- Animated progress runs only in interactive sessions outside knitting.
- Compatible with S7 releases after 0.2.2.
- The `prop_*` factories accept a quoted default, such as `default = quote(c("a", "b"))`.

# rtemis.core 0.4.6

- Schema descriptions must not spell values in R (`NULL`, `TRUE`, `FALSE`, `NA`, `@`).
- New `assert_description_language()`.
- Removed `bounded_double_property()`; use `prop_float()`.
- New `check_bounded_double_scalar()` and `check_bounded_integer_scalar()`, with `check_optional_*` forms.

# rtemis.core 0.4.5

- New `assert_config_contract()`.
- Config descriptions must not name R constructs (`setup_X`, `pkg::fn`, `.list_to_X`).
- A config may require only a dispatcher's discriminator.

# rtemis.core 0.4.4

- New `write_JSONSchema()` and `write_lines()`.

# rtemis.core 0.4.3

- License: BSD 3-Clause.
- New HTML constructors: `html_tag()`, `html_div()`, `html_p()`, `html_span()`, `html_strong()`, `html_ul()`, `html_li()`, `html_escape()`, and `html_raw()`.
- New S7 property factories `prop_boolean()`, `prop_integer()`, `prop_float()`, `prop_string()`, `prop_bag()`, and `prop_const()`, with `prop_spec()`.

# rtemis.core 0.4.2

- `get_output_type()` honors the `rtemis.output_type` option and the `RTEMIS_OUTPUT_TYPE` environment variable.
- `repr_ls()` prints a one-line S7 `repr()` on the element's own line.

# rtemis.core 0.4.1

- `abort()` gains `data`, a named list of fields attached to the condition.
- `fmt()`, its wrappers, `show_df()`, `show_table()`, and `repr_ls()` default to `output_type = NULL`, resolved by `get_output_type()`.
- `get_output_type()` with no arguments detects the session type.
- The progress completion glyph follows the handle's `output_type`.
- `progress_update()` validates `label`, `current`, and `add`.

# rtemis.core 0.4.0

- New nested progress API: `progress_begin()`, `progress_update()`, `progress_end()`, `progress_lapply()`, and `progress_clear()`.
- Progress renders as one status line with a spinner and breadcrumb (options `rtemis.progress_spinner` and `rtemis.progress_throttle`) and reports through `set_msg_sink()`.
- Log helpers clear a visible progress line before writing.

# rtemis.core 0.3.1

- `msg()` and `msg0()` close a pending `msgstart()` line.
- `pcat()` gains `pad`.
- Palette: `green` is teal; new `juniper` and `terracotta`.
- `printls()` and `repr_ls()` print the primary class, single-quoted.

# rtemis.core 0.3.0

- Exports the internals used by rtemis.
- `msg()` supports a custom sink.
- `headdot()` is renamed `collapse_head()`.

# rtemis.core 0.2.0

- New `abort()`: a styled console line, and a plain-text condition carrying `$trace` and the calling frame in `$call`.
- New `format_trace()`, `info()`, `warn()`, `success()`, and `dbg()`.
- Breaking: `check_*` and `clean_*` signal classed errors inheriting from `rtemis_input_error`, with plain-text messages.
- New option `rtemis.show_caller`.

# rtemis.core 0.1.0

- Custom S7 properties.
- More `check_*` functions.

# rtemis.core 0.0.3

- Initial CRAN release.
