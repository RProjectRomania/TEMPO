local_cache <- function(env = parent.frame()) {
  directory <- file.path(tempfile("tempo-cache-"), "cache")
  old <- options(TEMPO.cache_dir = directory)
  withr::defer(options(old), envir = env)
  directory
}

test_that("the default cache lives under the session temporary directory", {
  old <- options(TEMPO.cache_dir = NULL)
  on.exit(options(old), add = TRUE)

  expect_equal(tempo_cache_dir(), file.path(tempdir(), "TEMPO-cache"))
})

test_that("the cache directory can be set and returns the previous value", {
  old <- options(TEMPO.cache_dir = NULL)
  on.exit(options(old), add = TRUE)

  previous <- tempo_cache_dir(file.path(tempdir(), "custom"))
  expect_equal(previous, file.path(tempdir(), "TEMPO-cache"))
  expect_equal(tempo_cache_dir(), file.path(tempdir(), "custom"))
})

test_that("values round-trip through the cache and are cleared on demand", {
  local_cache()

  key <- .tempo_cache_key("unit", "test")
  expect_null(.tempo_cache_get(key))

  .tempo_cache_set(key, list(a = 1))
  expect_equal(.tempo_cache_get(key), list(a = 1))

  expect_equal(tempo_cache_clear(), 1L)
  expect_null(.tempo_cache_get(key))
})

test_that("cached values expire", {
  local_cache()
  key <- .tempo_cache_key("expiry")
  .tempo_cache_set(key, "stale")

  old <- options(TEMPO.cache_max_age = -1)
  on.exit(options(old), add = TRUE)
  expect_null(.tempo_cache_get(key))
})

test_that(".tempo_cached computes once and reuses afterwards", {
  local_cache()
  calls <- 0L
  compute <- function() {
    calls <<- calls + 1L
    "value"
  }
  key <- .tempo_cache_key("memo")

  expect_equal(.tempo_cached(key, compute), "value")
  expect_equal(.tempo_cached(key, compute), "value")
  expect_equal(calls, 1L)
})

test_that("cache keys distinguish language and base URL", {
  expect_false(identical(
    .tempo_cache_key("matrix", "A", "ro"),
    .tempo_cache_key("matrix", "A", "en")
  ))
})

# Download manifest -----------------------------------------------------------

fake_metadata <- function(update = "05-11-2025") {
  list(
    ultimaActualizare = update,
    details = list(matMaxDim = 1L, matUMSpec = 0L),
    dimensionsMap = list(list(options = list(list(nomItemId = 1L))))
  )
}

write_csv_file <- function(directory, code, text = "a,b\n1,2\n") {
  path <- file.path(directory, paste0(code, ".csv"))
  writeLines(text, path)
  path
}

test_that("a missing file is never current", {
  directory <- .tempo_prepare_directory(tempfile("tempo-dl-"))
  current <- .tempo_file_is_current(
    directory, "AAA", "ro", fake_metadata(),
    file.path(directory, "AAA.csv")
  )

  expect_false(current)
  expect_equal(attr(current, "reason"), "missing")
})

test_that("a file recorded in the manifest is reused", {
  directory <- .tempo_prepare_directory(tempfile("tempo-dl-"))
  metadata <- fake_metadata()
  path <- write_csv_file(directory, "AAA")

  .tempo_manifest_write(directory, list(
    AAA = .tempo_manifest_entry("AAA", "ro", metadata, path, 1L)
  ))

  current <- .tempo_file_is_current(directory, "AAA", "ro", metadata, path)
  expect_true(current)
  expect_equal(attr(current, "reason"), "current")
})

test_that("a Romanian file is not accepted for an English request", {
  # This is the bug the manifest exists to fix: the CSV itself does not record
  # its language, so a modification-time test cannot tell them apart.
  directory <- .tempo_prepare_directory(tempfile("tempo-dl-"))
  metadata <- fake_metadata()
  path <- write_csv_file(directory, "AAA")

  .tempo_manifest_write(directory, list(
    AAA = .tempo_manifest_entry("AAA", "ro", metadata, path, 1L)
  ))

  current <- .tempo_file_is_current(directory, "AAA", "en", metadata, path)
  expect_false(current)
  expect_equal(attr(current, "reason"), "language")
})

test_that("a revised matrix invalidates the local file", {
  directory <- .tempo_prepare_directory(tempfile("tempo-dl-"))
  path <- write_csv_file(directory, "AAA")

  .tempo_manifest_write(directory, list(
    AAA = .tempo_manifest_entry("AAA", "ro", fake_metadata("05-11-2025"),
                                path, 1L)
  ))

  current <- .tempo_file_is_current(
    directory, "AAA", "ro", fake_metadata("06-12-2025"), path
  )
  expect_false(current)
  expect_equal(attr(current, "reason"), "revised")
})

test_that("an edited or truncated file invalidates the local copy", {
  directory <- .tempo_prepare_directory(tempfile("tempo-dl-"))
  metadata <- fake_metadata()
  path <- write_csv_file(directory, "AAA")

  .tempo_manifest_write(directory, list(
    AAA = .tempo_manifest_entry("AAA", "ro", metadata, path, 1L)
  ))

  writeLines("a,b\n9,9\n", path)
  current <- .tempo_file_is_current(directory, "AAA", "ro", metadata, path)
  expect_false(current)
  expect_equal(attr(current, "reason"), "modified")
})

test_that("files predating the manifest are reused with a warning", {
  .tempo_reset_warnings()
  directory <- .tempo_prepare_directory(tempfile("tempo-dl-"))
  path <- write_csv_file(directory, "AAA")

  expect_warning(
    current <- .tempo_file_is_current(
      directory, "AAA", "ro", fake_metadata("05-11-2020"), path
    ),
    class = "tempo_cache_warning"
  )
  expect_true(current)
  expect_equal(attr(current, "reason"), "mtime")
})

test_that("an unparseable update date on a legacy file forces a download", {
  directory <- .tempo_prepare_directory(tempfile("tempo-dl-"))
  path <- write_csv_file(directory, "AAA")

  current <- .tempo_file_is_current(
    directory, "AAA", "ro", fake_metadata("not a date"), path
  )
  expect_false(current)
  expect_equal(attr(current, "reason"), "unverifiable")
})

test_that("the manifest survives a round trip and records provenance", {
  directory <- .tempo_prepare_directory(tempfile("tempo-dl-"))
  metadata <- fake_metadata()
  path <- write_csv_file(directory, "AAA")

  .tempo_manifest_write(directory, list(
    AAA = .tempo_manifest_entry("AAA", "en", metadata, path, 3L)
  ))

  entry <- .tempo_manifest_read(directory)$AAA
  expect_equal(entry$language, "en")
  expect_equal(entry$service_update, "05-11-2025")
  expect_equal(entry$last_update, "2025-11-05")
  expect_equal(entry$requests, 3L)
  expect_equal(entry$md5, unname(tools::md5sum(path)))
  expect_match(entry$retrieved_at, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T")
})

test_that("a corrupt manifest is ignored rather than fatal", {
  directory <- .tempo_prepare_directory(tempfile("tempo-dl-"))
  writeLines("{not json", .tempo_manifest_path(directory))

  expect_equal(.tempo_manifest_read(directory), list())
})

test_that("an unwritable download directory is a classed error", {
  expect_error(.tempo_prepare_directory(NULL), class = "tempo_input_error")
  expect_error(.tempo_prepare_directory(c("a", "b")), class = "tempo_input_error")
})
