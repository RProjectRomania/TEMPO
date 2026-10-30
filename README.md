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
tempo_search("unemployed", language = "en")
```

Then look before you download. A TEMPO matrix can be enormous, and
`tempo_meta()` tells you so in one request:

```r
meta <- tempo_meta("SOM101E", language = "en")
meta
#> <tempo_meta> SOM101E
#>   A. SOCIAL STATISTICS > LABOUR FORCE > REGISTERED UNEMPLOYED
#>   language:    en
#>   last update: 2026-10-12
#>   full table:  88,663,248 cells, over 2,000 requests
#>                too large to fetch whole; select values with tempo_get()
#>   dimensions:  5
#>     1. Sex (3 values): Total, Male, Female
#>     2. Counties (43 values): TOTAL, Alba, Arad, ...
#>     3. Localities (3182 values): TOTAL, 1017 ALBA IULIA MUNICIPALITY, ...
#>     4. Periods (216 values): Year 2010, January 2010, February 2010, ...
#>     5. MU: Persons (1 value): Number of persons
```

88 million cells is not a download. Fetch the slice you need instead, taking
the dimension names and value labels from `meta`:

```r
unemployment <- tempo_get(
  "SOM101E",
  filters = list(
    Sex        = "Total",
    Counties   = "Cluj",
    Localities = "TOTAL",
    Periods    = grep("2024", meta$values$Periods$label, value = TRUE)
  ),
  language = "en"
)
```

`tempo_get()` returns analysis-ready data: a numeric `value` column, the unit
of measure as a column, `":"` mapped to `NA`, and the service's footer lines
removed. Time labels are parsed according to the matrix's layout - a dedicated
`Years` dimension becomes an integer, while a combined `Periods` dimension
like the one above keeps its labels and gains `year` and `period` columns, so
an annual total stays distinguishable from the monthly figures beneath it.

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

Every function that reaches the network needs an internet connection, and
`tempo_bulk()` writes only to the directory you give it.

## License

GPL-3 | EUPL-1.1
