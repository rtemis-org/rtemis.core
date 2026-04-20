[![r-ci](https://github.com/rtemis-org/rtemis.core/actions/workflows/r-ci.yml/badge.svg)](https://github.com/rtemis-org/rtemis.core/actions/workflows/r-ci.yml) [![rtemis.core status badge](https://rtemis-org.r-universe.dev/rtemis.core/badges/version)](https://rtemis-org.r-universe.dev/rtemis.core)

# rtemis.core: Core utilities for `rtemis` R packages

Shared rtemis utilities, including:

- `fmt`: formats text for ANSI, HTML, and plain text output
- `msg`: prints messages with datetime info and name of calling function
- `test_*` functions return logical value
- `check_*` functions throw informative errors when needed
- `clean_*` functions that return validated and coerced inputs
- `rtemis_colors`: rtemis color system

> [!IMPORTANT]
> This package is intended for internal use in rtemis packages.
> It is not intended for end-user installation or use.

## Installation

### Latest version from `r-universe`

```r
pak::repo_add(myuniverse = "https://rtemis-org.r-universe.dev")
pak::pak("rtemis.core")
```

or using `install.packages`:

```r
install.packages(
  'rtemis.core',
  repos = c('https://rtemis-org.r-universe.dev', 'https://cloud.r-project.org')
)
```
