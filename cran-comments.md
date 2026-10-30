## Test environments

* local: macOS Tahoe 26.6.2 (aarch64-apple-darwin23), R 4.6.1
* GitHub Actions: ubuntu-latest (devel, release, oldrel-1), macOS-latest
  (release), windows-latest (release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Notes for the reviewer

This is a minor release. It fixes five bugs in the 0.1.0 download and metadata
handling and adds five exported functions; `NEWS.md` lists them.

Two user-visible return values changed, both documented in `NEWS.md` and in
the vignette's migration section:

* `tempo_bulk()` now returns the paths of all requested matrices rather than
  only the newly downloaded ones.
* `tempo_toc(full_description = TRUE)` returns `Last_update` as a `Date`.

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
