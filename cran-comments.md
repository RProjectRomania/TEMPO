## Test environments

* local: macOS Tahoe 26.6.2 (aarch64-apple-darwin23), R 4.6.1
* GitHub Actions: ubuntu-latest (devel, release, oldrel-1), macOS-latest
  (release), windows-latest (release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Notes for the reviewer

This is a patch release with a single bug fix, described in `NEWS.md`.

`tempo_tidy()` left the TEMPO Online service's field padding on the values it
returned. The service separates its CSV fields with `", "`, so every column but
the first arrived with a leading space in its values, as in `" Cluj"`. Column
names were already trimmed; values were not, so a value did not match the label
`tempo_meta()` reports for it and `subset(x, Judete == "Cluj")` selected no
rows. Values are now trimmed as well as names.

One user-visible value therefore changes: a column that held `" Cluj"` now
holds `"Cluj"`. `tempo_get(tidy = FALSE)` still returns the service's own
layout untouched.

### Writing to the file system

The package writes only where the caller asks it to:

* `tempo_bulk()` writes CSV files and a manifest to the `directory` argument,
  which has no default.
* The response cache defaults to a subdirectory of `tempdir()`. A persistent
  location is used only if the user sets one explicitly via
  `tempo_cache_dir()` or `options(TEMPO.cache_dir = )`; the documentation
  suggests `tools::R_user_dir("TEMPO", "cache")` for that.

### Network use

All examples that need the TEMPO Online service are wrapped in `\dontrun{}`,
and the vignette chunks that need it are not evaluated. The unit tests run
against a local `webfakes` server, so no test contacts the network. Tests
against the live service are skipped on CRAN, skipped when offline, and
additionally require `TEMPO_LIVE_TESTS=true`.

The service is reachable only over plain HTTP; it does not offer TLS. The
package caps itself at four concurrent connections by default.
