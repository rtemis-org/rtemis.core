# rtemis.core NEWS

## Version 0.4.4

- `write_JSONSchema()` and `write_lines()`, moved here from `rtemis`. The schema
  registry has more than one producer, so a document's shape belongs to the
  registry rather than to whichever package emitted it. Keywords are ordered on
  write; `digits` is an argument, `I(17)` where a double must round-trip exactly.

## Version 0.4.3

- **License: BSD 3-Clause**, replacing GPL (>= 3). `Imports` are `data.table` 
  (MPL-2.0), `S7` (MIT) and `methods` (part of R). The change unblocks `rtemis` 
  itself, which cannot be permissively licensed while it imports a GPL package.
- HTML construction: `html_tag()` and the `html_div()`, `html_p()`,
  `html_span()`, `html_strong()`, `html_ul()`, `html_li()` constructors, with
  `html_escape()` and `html_raw()`. Elements are character strings rather than
  a node tree, which is what `fmt(output_type = "html")` already emits and what
  every consumer in the ecosystem wants to embed or send over the wire.
  Text children are escaped and markup children are not, so composing an
  element from an element neither double-escapes the inner markup nor leaves
  user-supplied text unescaped: a bare string is text, `html_raw()` marks a
  string that is already markup, and the constructors mark what they built.
  `paste()` drops that marker, so a run of markup assembled with `paste()` is
  passed on as `html_raw(paste(...))`.
  Output matches what `htmltools` produced for the same input, including the
  layout rule that a tag holding one text child renders inline while anything
  else renders as an indented block. The one deliberate difference is that a
  child's own line breaks are indented along with it, where `htmltools` splices
  pre-built markup in verbatim and leaves its continuation lines at column
  zero; indentation therefore always tracks nesting depth. Since HTML collapses
  that whitespace, the rendered result is identical. This lets packages that
  only built small fragments of HTML drop `htmltools`, and with it a GPL
  dependency.
- S7 property factories: `prop_boolean()`, `prop_integer()`, `prop_float()`,
  `prop_string()`, `prop_bag()` and `prop_const()`, with `prop_spec()` to read
  a property's declaration back. One call carries the property's type, default,
  bounds, enum, container and description, and its S7 validator is generated
  from that declaration rather than written by hand. Because the declaration
  stays attached to the property, `prop_spec()` can recover it from a class
  definition to generate documentation, a JSON Schema, or a defaults artifact.
  These succeed the hand-written properties in `R/03_S7_properties.R`, which
  remain: `prob_scalar` is `prop_float(min = 0, max = 1)` and
  `optional_character_scalar` is `prop_string(nullable = TRUE)`.

## Version 0.4.2

- `get_output_type()` can now be overridden, so ANSI output is available in
  non-interactive sessions (scripts, `Rscript`, the `rtemis` CLI's R backend),
  which previously always resolved to "plain". With `output_type = NULL` the
  resolution order is now: `filename` (forces "plain"), the new
  `rtemis.output_type` option, the new `RTEMIS_OUTPUT_TYPE` environment
  variable, `NO_COLOR` (forces "plain"), then as before "ansi" when
  interactive and "plain" otherwise. The option suits `.Rprofile`; the
  environment variable lets a parent process decide per invocation.
  Unrecognized values in either are ignored rather than raising, so a typo
  falls through to the next rule instead of aborting a running job. Since the
  resolved type also selects the progress display, forcing "ansi" where output
  is captured to a file yields carriage-return-rewritten lines, not just
  escape codes.
- `repr_ls()` shows an S7 element whose `repr()` is a single line on the
  element's own line, like a value, rather than breaking to the next line. The
  break exists so that every line of a multi-line `repr()` carries the same
  pad, which a one-line `repr()` does not need; without this a one-line
  `repr()` also had to end in a newline of its own or the following element ran
  on after it. An element whose `repr()` fails is reported inline the same way,
  rather than as a padded block.
- Use updated rtemis schemas

## Version 0.4.1

- `abort()` gains a `data` argument: a named list of structured fields
  attached to the signalled condition (e.g.
  `data = list(status_code = 429L, provider = "anthropic")`), retrievable by
  handlers via `condition$<name>`. Names may not collide with the built-in
  condition fields (`message`, `parent`, `call`, `trace`).
- `fmt()` and all its wrappers (`highlight()`, `bold()`, `italic()`, `thin()`,
  `gray()`, `checkmark()`, `crossmark()`, `col256()`, `fmt_gradient()`, etc.)
  as well as the `show_df()`/`show_table()`/`repr_ls()` printers now default to
  `output_type = NULL`, resolved via `get_output_type()`: "ansi" in interactive
  sessions, "plain" otherwise. Previously they defaulted to "ansi"
  unconditionally, emitting raw ANSI escape codes in non-interactive contexts
  (scripts, knitr, tests). Explicitly passing "ansi", "html", or "plain"
  behaves as before; `NULL` can now be forwarded safely through the whole
  formatting stack, so callers that only pass `output_type` through to
  formatting functions no longer need to resolve it themselves.
- `get_output_type()` called with no arguments is now environment-aware
  (previously it returned "ansi" unconditionally because its default skipped
  the NULL branch).
- The progress completion line's success glyph now follows the handle's
  resolved `output_type`, so handles created with `output_type = "plain"` no
  longer emit an ANSI-bold checkmark in interactive sessions.
- `progress_update()` now validates `label`, `current`, and `add` (scalar,
  type, non-missing) with the same condition classes as `progress_begin()`.

## Version 0.4.0

- New nested progress subsystem (`R/progress.R`) replacing the last remaining
  use of cli (`cli::cli_progress_along`) in the ecosystem: `progress_begin()`
  / `progress_update()` / `progress_end()` handle API plus a
  `progress_lapply()` near-drop-in wrapper (lapply-style `X`/`FUN`
  arguments, so `...` forwarding never collides with the wrapper's own
  parameters).
- Console rendering: single status line rewritten in place with a
  color-pulsing spinner (light-orange-to-red ping-pong ramp over the rtemis
  palette; designs selectable
  via `options(rtemis.progress_spinner = )`: `"dots"`, `"dot"`, `"blocks"`)
  and a breadcrumb of all nested levels (`Outer 2/5 > Tuning 7/30 ETA 0:41`).
  Non-interactive/plain output prints one begin and one completion line
  instead. Redraws throttled via `options(rtemis.progress_throttle = )`.
- Message-sink integration: progress events are forwarded through the
  `set_msg_sink()` envelope with `level = "progress"` and node fields
  (`node_id`, `parent_id`, `kind`, `status`, `current`, `total`),
  implementing the rtemis.core side of rtemis `specs/observability.md`.
  Sink events fire regardless of verbosity; verbosity gates only the console
  renderer. `"update"` events honor the throttle.
- Completion lines report uniformly completed nested loops as a
  multiplication chain (`Outer 2/2 x Tuning 24/24 done in 0:41`), recursively
  for deeper nesting. A nested level is included only when all of its runs
  completed fully with identical label and total; otherwise the chain is
  omitted rather than misleading.
- `msg()`/`msg0()`/`msgstart()`/`msgdone()`/`suggest()` clear a visible
  progress status line before writing, so log output never collides with an
  in-place progress redraw.
- `progress_lapply()` intercepts `message()`/`warning()` conditions raised
  by user code (or third-party packages it calls) and clears the status line
  before they print, so verbose foreign output lands on a clean line. Direct
  stdout writes (`cat()`, `print()`) cannot be intercepted; new exported
  `progress_clear()` provides an escape hatch for those.

## Version 0.3.1

- `msg()`/`msg0()` now close a pending `msgstart()` line before writing, so
  log lines and error stamps no longer collide with unfinished progress text
  (new internal `line_open` state).
- `pcat()`: `pad` argument support.
- Color updates: `green` is now teal; the previous green moved to new
  `juniper`; added `terracotta`; adjusted `light_mauve` and success color.
- `printls()`/`repr_ls()` print class names single-quoted and use the primary
  class only, for consistent `object of class '...'` output.

## Version 0.3.0

- export remaining internals used by **rtemis**
- migrated updated `msg()` with custom sink support from **rtemis**: allows for logging through **rtemis.server** to **rtemislive**
- `headdot()` renamed to `collapse_head()`

## Version 0.2.0

### New features

- New `abort()` dual-channel error signaller (in `log.R`): emits a styled
  one-line event to the operator console (most-specific class name +
  caller bracket) and signals a condition whose `$message` field is plain
  text - safe to serialize into JSON/HTML/any ANSI-unaware sink.
- `abort()` captures the call stack at the signal site and attaches it to
  the condition as `$trace`. Unlike base R's `traceback()` (which only
  populates `.Traceback` for uncaught errors), `$trace` survives
  `tryCatch()` and travels with the condition - useful for server-side
  handlers that ship errors to a browser-side debug pane.
- New exported `format_trace()` helper: renders the `$trace` pairlist (or
  the condition itself) as a numbered, one-line-per-frame string.
- `abort()` resolves the user's calling function via a stack walk that
  skips frames in this package and in `base` (`tryCatch`, `doTryCatch`,
  etc.). Both the console bracket and the condition's `$call` field point
  to the same user frame - so R's default error printer shows
  `Error in my_func(x) : ...` instead of bare `Error: ...`, regardless
  of how many `check_*` / `clean_*` wrappers sit between `abort()` and
  the caller.
- New `info()`, `warn()`, `success()`, `dbg()` log helpers, all routed
  through `msg()` for consistent timestamp + glyph + caller-bracket
  formatting.
- Error class hierarchy: every `check_*` / `clean_*` abort carries
  failure-mode classes (`rtemis_null_input`, `rtemis_na_input`,
  `rtemis_type_error`, `rtemis_length_error`, `rtemis_range_error`,
  `rtemis_value_error`, `rtemis_dependency_error`), all inheriting from
  `rtemis_input_error` - callers can catch input failures broadly or
  narrow to a specific failure mode via `tryCatch()`.
- New global option `rtemis.show_caller` (default `TRUE`): set to `FALSE`
  in `.Rprofile` to suppress the caller bracket on all log lines for the
  session.
- `format_caller()` now auto-suppresses non-name expressions (anonymous
  function literals, inline `do.call`-style invocations) - no more
  `[function (ws, req) {...]` brackets cluttering logs from
  callback-heavy code.

### Breaking changes

- All `check_*` and `clean_*` functions now signal errors via `abort()`
  with structured condition classes rather than via `cli::cli_abort()`.
  Error message text changes from cli's `{.var x}` styling to plain
  backtick-quoted names (e.g. `` `x` cannot be NULL. ``). Code that
  matches on exact error message strings will need to be updated.
  Generic `tryCatch(error = ...)` handlers continue to work; handlers
  keyed on the previous `rlang_error` / `cli`-specific classes will
  need to be updated to the new `rtemis_error` hierarchy (which also
  gains structured failure-mode subclasses).

## Version 0.1.0

- Added custom S7 properties set
- Expanded `check_*` function set

## Version 0.0.3

- Initial CRAN release
