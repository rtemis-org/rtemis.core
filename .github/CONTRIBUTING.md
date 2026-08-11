# Contributing to rtemis.core

rtemis.core holds the utilities shared across the packages of the
[rtemis](https://github.com/rtemis-org/rtemis) ecosystem: the `msg()` messaging
system, the `fmt()` formatting system, the `S7` property library, and the
`test_*` / `check_*` / `clean_*` families.

It is a dependency rather than a destination. Most changes here are driven by a
need in a package that consumes it, so if you are proposing an addition, say
which consumer needs it and what it would replace there. A utility with one
caller usually belongs in that caller.

## Reporting Issues

Search [existing issues](https://github.com/rtemis-org/rtemis.core/issues)
first. A useful report includes:

1. **rtemis.core version**: `utils::packageVersion("rtemis.core")`
2. **R version**: `R.version.string`
3. **Operating system**
4. **A minimal reproducible example**, and what you expected instead
5. **Complete error messages**, with the stack trace

If the behavior surfaced through another rtemis package, report it there and
link across; the reproducer is usually clearer in terms of the consumer's API.

## Pull Requests

Discuss major changes in an issue first, so that design questions are settled
before anyone writes code.

### Licensing of contributions

rtemis.core is released under the [BSD 3-Clause License](../LICENSE.md). By
submitting a pull request, patch, or any other contribution, you agree to all
of the following.

1. **Inbound equals outbound.** Your contribution is licensed under the BSD
   3-Clause License, the same terms that cover the rest of the package. You
   retain copyright in your own work.

2. **You grant the right to relicense.** You grant E.D. Gennatas a perpetual,
   worldwide, non-exclusive, royalty-free, irrevocable license to reproduce,
   modify, distribute and sublicense your contribution, including the right to
   distribute it under any OSI-approved license the project later adopts.

3. **You have the right to grant it.** Either you wrote the contribution
   yourself, or you have permission from its copyright holder to submit it
   under these terms. Do not submit code copied or adapted from a source under
   a copyleft license (GPL, AGPL, LGPL) or under any license whose terms
   conflict with BSD 3-Clause. If any part of your contribution originates
   elsewhere, say so in the pull request and name the source and its license.

Point 2 is the one that is easy to skip and expensive to add later.
rtemis.core changed license once already, from GPL (>= 3) to BSD 3-Clause in
0.4.3, and doing so was straightforward only because every line of the package
was the copyright holder's own. A project that accepts contributions without an
explicit grant cannot make that kind of change again without tracking down
every past contributor for consent. The grant keeps the option open without
asking anyone to sign a separate agreement or assign their copyright.

This matters more here than in most packages: rtemis.core sits in the `Imports`
of every other rtemis package, so its license sets the floor for all of them. A
copyleft obligation acquired here would propagate to the whole ecosystem.

### Sign your commits

Every commit must carry a `Signed-off-by` line certifying the
[Developer Certificate of Origin](https://developercertificate.org/):

```sh
git commit -s -m "Your commit message"
```

which appends a line matching your git `user.name` and `user.email`:

```
Signed-off-by: Your Name <you@example.com>
```

Amend an unsigned commit with `git commit --amend -s`, or a range with
`git rebase --signoff <base>`.

### Dependencies

`Imports` is `data.table`, `methods` and `S7`, and the bar for adding to it is
high: every consumer in the ecosystem inherits whatever lands here. A new
dependency needs a justification in the pull request covering what it replaces,
its license, and its own dependency tree. Anything under a copyleft license
will not be accepted in `Imports`.

### Before you open a pull request

Development tasks go through the `justfile` rather than direct `Rscript` or
`R CMD` calls; `just --list` shows every recipe. Recipes chain, so `just
install` already runs `just document`, which already runs `just format`.

- `just install` -- format, document, and install
- `just test` -- run the test suite
- `just lint`, `just check-rd`, `just spell`
- `just check-cran` -- run before claiming CRAN compliance

Because every rtemis package depends on this one, a change that alters existing
behavior also needs a reverse-dependency check. Say in the pull request whether
you ran one and which consumers you checked against.

### Code conventions

**Classes and types.** The backend is S7 throughout. Build class properties
with the `prop_*` factories rather than declaring them by hand: one declaration
carries type, default, bounds, enum, tunability and description, and the S7
validator, the JSON Schema and the defaults artifact are all generated from it.
Hand-writing validation for a property usually means a factory argument was
missed.

Make a factory-built property optional with `nullable = TRUE`, not a union.
Declare a hand-written optional property as `NULL | <class>`, never
`<class> | NULL`: S7 takes a union's prototype from its **first** member, so
`class_integer | NULL` defaults to `integer(0)` rather than `NULL`, and every
`!is.null()` guard downstream silently misfires. `default = NULL` does not
help -- S7 reads it as "no default supplied" and falls back to the prototype.

`NULL` is the only unset value. Test for it with `is.null()`, not
`length(x) == 0L`.

**Validation.** Type-check and validate as early as possible, with corrective
error messages. The `test_*` functions return a logical, the `check_*`
functions throw an informative error, and the `clean_*` functions return a
validated and coerced value; a new helper belongs to whichever family matches
its return contract.

**Logging.** `msg()`, `info()`, and `abort()` with the rtemis error classes.
Any function that can print to the console takes a `verbosity` argument
controlling how much.

**Style.**

- Type-stable code; never rely on implicit coercion
- Integer literals carry an `L` suffix: `n = 10L`
- Optional arguments default to `NULL`, with the real default set in the body
- Two blank lines between definitions
- US English: `behavior`, `normalize`, `analyze`, `license`
- ASCII only, everywhere. CRAN rejects non-ASCII characters; use hexadecimal
  Unicode escapes where a literal one is unavoidable
- Comments describe only the current state of the code. No history ("used to",
  "renamed from", "as of <date>") and no argument for why the project works the
  way it does. Git records what changed. Do document non-obvious mechanism that
  a future editor has to preserve

**Documentation.** roxygen2 on everything, with examples. Internal functions
get `@keywords internal` and `@noRd`. Document a `@param` as
`Class: Description ending with period.` Do not restate default values in the
description -- they already appear in the `Usage` section.

**Tests.** Include tests for new functionality.

## Questions?

- **Bug reports and features**:
  [GitHub Issues](https://github.com/rtemis-org/rtemis.core/issues)
- **Security issues**: contact the maintainer directly (see `DESCRIPTION`)

---

Thank you for contributing to rtemis.core.
