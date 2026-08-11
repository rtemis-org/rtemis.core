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
behavior needs a reverse-dependency check as well; see `revdep/`.

Code conventions -- S7 classes and `prop_*` factories, type checking and
validation at construction, `NULL` as the only unset value, `L` suffixes on
integer defaults, US English spelling, ASCII only, roxygen2 on everything --
are documented in [`AGENTS.md`](../AGENTS.md) at the repository root. That file
is the authority; this list is a pointer to it, not a copy.

## Questions?

- **Bug reports and features**:
  [GitHub Issues](https://github.com/rtemis-org/rtemis.core/issues)
- **Security issues**: contact the maintainer directly (see `DESCRIPTION`)

---

Thank you for contributing to rtemis.core.
