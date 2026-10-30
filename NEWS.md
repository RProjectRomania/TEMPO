# TEMPO 0.2.0

## Bug fixes

* `tempo_tidy()` collapsed a combined period dimension to its year, which lost
  the distinction between an annual total and a monthly figure. Many matrices,
  `SOM101E` among them, carry a single `Perioade`/`Periods` dimension whose
  labels mix frequencies: `"Year 2010"`, `"January 2010"`,
  `"February 2010"`. All three became the integer `2010` and the rows became
  indistinguishable. Such a column is now left exactly as received and gains
  `year` and `period` columns instead, where `period` is `NA` for an annual
  row. A dedicated `Ani`/`Years` dimension still becomes an integer in place,
  since it carries nothing else.

* `tempo_toc(full_description = TRUE)` returned raw HTML in
  `Statistical_domain`. The service embeds anchors in its top-level domain
  labels, so the column held values such as
  `AGRICULTURE <a href="https://insse.ro/...">Press releases</a>`. HTML is now
  stripped from every character column, and the URLs are kept separately in a
  new `Press_releases` column.

* `tempo_bulk()` ignored `language` when deciding whether a local file was
  current. Because a CSV file does not record which language it holds, a
  Romanian file was reported as up to date for an English request, and the
  caller silently received Romanian data. Freshness is now decided from a
  `.tempo-manifest.json` written beside the CSV files, which records the
  language, the service's own update string and an MD5 checksum of each file.
  Files written by earlier versions are still accepted on the old
  modification-time rule, with a warning that their language cannot be
  verified; pass the new `overwrite = TRUE` to be certain.

* Large downloads were chunked per dimension against a fixed 300-value
  threshold, which was the wrong budget in both directions. A matrix with five
  dimensions of 200 values each crosses no per-dimension threshold yet asks
  for 3.2e11 cells in a single request, which timed out; a 301-value dimension
  in an otherwise small matrix was split into four needless requests. Query
  size is now budgeted on the product of the selected values, capped by
  `getOption("TEMPO.max_cells")`, with the per-dimension cap retained as
  `getOption("TEMPO.max_options_per_dim")`.

* An update date the package could not parse made every freshness check fail,
  so affected matrices were re-downloaded on every call with no diagnostic.
  Several date layouts are now recognised, caching falls back to the service's
  raw update string, and an unrecognised layout raises a
  `tempo_date_warning` once per session.

* Chunked CSV responses were concatenated after removing one header line from
  each part, assuming without checking that every part had the same columns.
  Each part's header is now compared with the first part's, and a mismatch is
  a `tempo_api_error` rather than a misaligned file.

* `R/utils.R` was unreachable code whose `fetch_tempo_payload()` called
  `tempo_logger()`, a function deleted from the package, and used
  `utils::read.csv()` without `utils` being declared in `Imports`. The file has
  been removed and its retry logic folded into the request layer, where it is
  now actually used. `stats`, `tools` and `utils` are declared in `Imports`.

## New features

* `tempo_get()` downloads a table, or a selected part of one, straight into a
  data frame. Dimension selections are sent to the service, so fetching one
  county and ten years costs one small request instead of a full download
  followed by a subset in R. Previously the only way to obtain data was to
  download an entire matrix to a CSV file.

* `tempo_meta()` reports a matrix's dimensions, the values selectable in each,
  its publication date, its position in the statistical classification, and
  how many cells and requests a full download would cost. This metadata was
  fetched internally but never exposed. Matrices that are too large to fetch
  whole say so, and name the option that governs the limit.

* `tempo_tidy()` turns the service's CSV layout into analysis-ready data: the
  unit of measure is lifted out of the `UM:`/`MU:` column header into a
  column, `Valoare`/`Value` becomes numeric with `":"` mapped to `NA`,
  `"Anul 2020"` becomes the integer `2020`, trailing footnote and copyright
  rows are dropped, and monthly, quarterly and semestrial labels gain a
  `Date` column. `tempo_get()` applies it by default.

  Versions before 0.1.0 had a `tempo_clean()` doing part of this, but it
  assigned its result into the global environment as a side effect and so
  could not be used in a pipeline; 0.1.0 dropped it entirely.
  `tempo_tidy()` is a plain function of its input.

* `tempo_search()` filters the table of contents by one or more patterns,
  across any of its columns.

* `tempo_hierarchy()` reads the service's classification tree, returning every
  domain, sub-domain, survey and matrix with its parent and full path. The
  branch structure takes a single request; placing the matrices within it costs
  one further request per survey, which is a few hundred rather than the one
  per matrix that `tempo_toc(full_description = TRUE)` needs. Pass
  `matrices = FALSE` for the classification alone.

* `tempo_cache_dir()` and `tempo_cache_clear()` control response caching. The
  table of contents and per-matrix metadata are now cached; by default in the
  session temporary directory, so nothing is written outside it unless you ask
  for a persistent location.

* Errors carry S3 classes, so a caller can distinguish a bad argument from an
  unreachable service without matching on English message text:
  `tempo_input_error`, `tempo_offline_error`, `tempo_http_error` (with
  `status_code`, `url` and `body`), `tempo_api_error` and `tempo_io_error`,
  all inheriting from `tempo_error` and from `error`. Warnings are classed
  too, and are raised at most once per session each. See
  `?"TEMPO-conditions"`.

* Requests now send a `User-Agent` identifying the package and accept gzip,
  retry transport failures and HTTP 408, 425, 429 and 5xx with exponential
  backoff while honouring `Retry-After`, and can be throttled. Request
  fan-out, as in `tempo_toc(full_description = TRUE)`, runs concurrently with
  a connection cap of four. All of this is configurable; see
  `?"TEMPO-options"`.

* Long operations report progress when the session is interactive.

## Changes that may affect existing code

* `tempo_bulk()` now returns the paths of **all** requested matrices, named by
  matrix code, rather than only the newly written ones, with `downloaded`,
  `reason` and `requests` attributes describing what happened. Previously a
  caller had to rebuild the paths of skipped files by hand, which the old
  vignette explicitly advised. Code that loops over the result is unaffected;
  code that relied on skipped files being absent should consult
  `attr(files, "downloaded")`.

* `tempo_toc(full_description = TRUE)` returns `Last_update` as a `Date`
  rather than a character string.

* `tempo_toc()` gained a `cache` argument and may return a `comment` column
  when the service supplies one.

## Documentation

* Every exported function documents its arguments, return value and failure
  modes, with runnable examples where no network is required.

* The vignette has been rewritten around the new API, and its tidying section
  executes, so the output shown is real rather than asserted.

* New help topics `?"TEMPO-options"` and `?"TEMPO-conditions"`.

## Testing

* The test suite now covers the HTTP layer end to end against a fake TEMPO
  service built with `webfakes`, including retries, HTTP error classes,
  chunked downloads with header verification, the freshness rules and the
  language-collision regression. Previously no test exercised any network
  code path. Integration tests against the live service are kept separate and
  skipped on CRAN, offline, and unless `TEMPO_LIVE_TESTS=true`.

# TEMPO 0.1.0

* First CRAN release, with `tempo_toc()` and `tempo_bulk()`.
