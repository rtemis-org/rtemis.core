# rtemis.core NEWS

## Version 0.4.0

- New nested progress subsystem (`R/progress.R`) replacing the last remaining
  use of cli (`cli::cli_progress_along`) in the ecosystem: `progress_begin()`
  / `progress_update()` / `progress_end()` handle API plus a
  `progress_lapply()` near-drop-in wrapper.
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
