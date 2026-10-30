## Test environments

* local: Ubuntu 22.04, R 4.3.3
* GitHub Actions: ubuntu-latest (devel, release, oldrel-1), macOS-latest
  (release), windows-latest (release)

## R CMD check results

0 errors | 0 warnings | 0 notes

<!-- Re-run `R CMD check --as-cran` on a networked machine before submitting
     and update this section. The run behind this release was made in a
     sandbox without outbound network access, which produced two NOTEs that
     are artifacts of that environment rather than of the package:

       * "Found the following (possibly) invalid URLs" for
         http://statistici.insse.ro:8077/tempo-online/ and the GitHub URL,
         neither of which the sandbox could resolve;
       * "checking for future file timestamps ... unable to verify current
         time".

     Both should disappear with network access. -->

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
