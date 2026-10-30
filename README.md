# TEMPO

<!-- badges: start -->
[![R-CMD-check](https://github.com/RProjectRomania/TEMPO/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RProjectRomania/TEMPO/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`TEMPO` gives R access to the Romanian National Institute of Statistics'
[TEMPO Online](http://statistici.insse.ro:8077/tempo-online/) database:
roughly 1500 statistical matrices covering population, labour, agriculture,
trade and prices.

## Installation

```r
install.packages("TEMPO")

# Development version
# install.packages("remotes")
remotes::install_github("RProjectRomania/TEMPO")
```

## Usage

```r
library(TEMPO)

# Find a table.
tempo_search("unemployment", language = "en")

# Inspect it before downloading: dimensions, values, size, publication date.
tempo_meta("SOM101E", language = "en")

# Fetch only what you need, straight into a data frame.
unemployment <- tempo_get(
  "SOM101E",
  filters = list(Years = 2015:2024),
  language = "en"
)
```

`tempo_get()` returns analysis-ready data: a numeric `value` column, the unit
of measure as a column, integer years, a `Date` column for monthly and
quarterly tables, `":"` mapped to `NA`, and the service's footer lines
removed.

For whole tables on disk, `tempo_bulk()` writes CSV files and keeps them up to
date between sessions, recording the language, published revision and checksum
of each file in a manifest beside them.

```r
files <- tempo_bulk(
  codes = c("ACC101B", "ACC101C"),
  language = "en",
  directory = file.path(tempdir(), "tempo-data")
)

accidents <- tempo_tidy(files[["ACC101B"]], code = "ACC101B")
```

| Function | Purpose |
| --- | --- |
| `tempo_toc()` | List every matrix, optionally with its classification |
| `tempo_search()` | Filter the table of contents by pattern |
| `tempo_hierarchy()` | Walk the statistical classification tree |
| `tempo_meta()` | Dimensions, selectable values, size and update date of one matrix |
| `tempo_get()` | Download a table, or a filtered part of it, into a data frame |
| `tempo_bulk()` | Download whole tables to CSV, skipping what is current |
| `tempo_tidy()` | Turn the service's CSV layout into analysis-ready data |
| `tempo_cache_dir()` | Inspect or change where metadata responses are cached |

See `vignette("using-tempo")` for a full walkthrough, `?"TEMPO-options"` for
configuration, and `?"TEMPO-conditions"` for the classed errors that let a
script distinguish a bad argument from an unreachable service.

## The service

TEMPO Online is a public service of the Romanian National Institute of
Statistics. It is reachable only over plain HTTP and publishes no API contract
or rate limit. This package identifies itself with a `User-Agent`, retries only
what is worth retrying, caches metadata, and caps itself at four concurrent
connections by default. Please keep it modest.

Both functions that reach the network need an internet connection, and
`tempo_bulk()` writes only to the directory you give it.

## License

GPL-3 | EUPL-1.1
