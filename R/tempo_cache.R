# Response cache and download manifest ----------------------------------------

.tempo_default_cache_dir <- function() {
  file.path(tempdir(), "TEMPO-cache")
}

#' Where TEMPO caches service responses
#'
#' The table of contents and the per-matrix metadata change rarely but are
#' expensive to fetch, so `TEMPO` caches them. Call `tempo_cache_dir()` to see
#' the directory in use and `tempo_cache_dir(path)` to change it.
#'
#' By default the cache lives in a subdirectory of [base::tempdir()], so it is
#' discarded when the R session ends and nothing is written outside the session
#' temporary area. To keep the cache between sessions, point it at a persistent
#' location; [tools::R_user_dir()] gives the conventional one:
#'
#' ```r
#' tempo_cache_dir(tools::R_user_dir("TEMPO", "cache"))
#' ```
#'
#' Equivalently, set `options(TEMPO.cache_dir = ...)`, which is useful in a
#' project `.Rprofile`. Entries older than `TEMPO.cache_max_age` seconds (one
#' day by default) are ignored and refetched.
#'
#' @param path A single directory path, or `NULL` to query the current setting
#'   without changing it. The directory is created when it does not exist.
#'
#' @return The cache directory, invisibly when setting it.
#'
#' @seealso [tempo_cache_clear()], [TEMPO-options]
#' @examples
#' # The default cache is inside the session temporary directory.
#' tempo_cache_dir()
#'
#' # Use a project-local cache instead.
#' old <- tempo_cache_dir(file.path(tempdir(), "my-tempo-cache"))
#' tempo_cache_dir()
#' tempo_cache_dir(old)
#' @export
tempo_cache_dir <- function(path = NULL) {
  if (is.null(path)) {
    current <- .tempo_opt("cache_dir")
    if (is.null(current)) {
      current <- .tempo_default_cache_dir()
    }
    return(as.character(current)[[1L]])
  }

  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    .tempo_abort(
      "`path` must be a single directory path.",
      class = "tempo_input_error"
    )
  }

  previous <- tempo_cache_dir()
  options(TEMPO.cache_dir = path.expand(path))
  invisible(previous)
}

#' Delete cached TEMPO responses
#'
#' Removes every cached response from [tempo_cache_dir()]. Use it after the
#' service has published new data and you want to be certain that nothing stale
#' is reused.
#'
#' @return The number of cache entries removed, invisibly.
#'
#' @seealso [tempo_cache_dir()]
#' @examples
#' tempo_cache_clear()
#' @export
tempo_cache_clear <- function() {
  directory <- tempo_cache_dir()
  if (!dir.exists(directory)) {
    return(invisible(0L))
  }

  files <- list.files(directory, pattern = "\\.rds$", full.names = TRUE)
  unlink(files)
  invisible(length(files))
}

.tempo_cache_max_age <- function() {
  value <- getOption("TEMPO.cache_max_age")
  if (is.null(value)) 86400 else as.numeric(value)
}

.tempo_cache_key <- function(...) {
  # Deterministic, filesystem-safe key. md5sum() is available from the base
  # "tools" package, so this needs no extra dependency.
  parts <- paste(vapply(list(...), paste, character(1), collapse = "\u001f"),
                 collapse = "\u001e")
  file <- tempfile()
  on.exit(unlink(file), add = TRUE)
  writeBin(charToRaw(parts), file)
  unname(tools::md5sum(file))
}

.tempo_cache_get <- function(key) {
  if (!is.finite(.tempo_cache_max_age()) && .tempo_cache_max_age() <= 0) {
    return(NULL)
  }

  path <- file.path(tempo_cache_dir(), paste0(key, ".rds"))
  if (!file.exists(path)) {
    return(NULL)
  }

  entry <- tryCatch(readRDS(path), error = function(error) NULL)
  if (!is.list(entry) || is.null(entry$stored_at) || is.null(entry$value)) {
    return(NULL)
  }

  age <- as.numeric(Sys.time() - entry$stored_at, units = "secs")
  if (is.na(age) || age > .tempo_cache_max_age()) {
    return(NULL)
  }

  entry$value
}

.tempo_cache_set <- function(key, value) {
  directory <- tempo_cache_dir()
  if (!dir.exists(directory)) {
    created <- dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    if (!created && !dir.exists(directory)) {
      # Caching is a convenience, never a requirement: give up quietly.
      return(invisible(FALSE))
    }
  }

  path <- file.path(directory, paste0(key, ".rds"))
  ok <- tryCatch(
    {
      saveRDS(list(stored_at = Sys.time(), value = value), path)
      TRUE
    },
    error = function(error) FALSE
  )

  invisible(ok)
}

.tempo_cached <- function(key, compute) {
  cached <- .tempo_cache_get(key)
  if (!is.null(cached)) {
    return(cached)
  }

  value <- compute()
  .tempo_cache_set(key, value)
  value
}

# Download directory ----------------------------------------------------------

.tempo_prepare_directory <- function(directory) {
  if (
    is.null(directory) ||
      !is.character(directory) ||
      length(directory) != 1L ||
      is.na(directory) ||
      !nzchar(directory)
  ) {
    .tempo_abort(
      "`directory` must be a single path where the CSV files can be written.",
      class = "tempo_input_error"
    )
  }

  directory <- path.expand(directory)
  if (!dir.exists(directory)) {
    created <- dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    if (!created && !dir.exists(directory)) {
      .tempo_abort(
        sprintf("Could not create `directory`: %s", directory),
        class = "tempo_io_error",
        path = directory
      )
    }
  }

  if (file.access(directory, mode = 2L) != 0L) {
    .tempo_abort(
      sprintf("`directory` is not writable: %s", directory),
      class = "tempo_io_error",
      path = directory
    )
  }

  normalizePath(directory, winslash = "/", mustWork = TRUE)
}

# Download manifest -----------------------------------------------------------
#
# tempo_bulk() records what it wrote next to the CSV files, in
# ".tempo-manifest.json".  The manifest is what makes the freshness check
# correct: a CSV on its own does not say which language it holds, nor which
# published revision of the matrix it came from, so the previous
# modification-time heuristic silently accepted a Romanian file for an English
# request.  Recording the language and the service's own update string removes
# both guesses, and the checksum additionally detects a truncated or
# hand-edited file.

.tempo_manifest_name <- ".tempo-manifest.json"

.tempo_manifest_path <- function(directory) {
  file.path(directory, .tempo_manifest_name)
}

.tempo_manifest_read <- function(directory) {
  path <- .tempo_manifest_path(directory)
  if (!file.exists(path)) {
    return(list())
  }

  manifest <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(error) NULL
  )

  if (!is.list(manifest) || is.null(manifest$files) || !is.list(manifest$files)) {
    return(list())
  }

  manifest$files
}

.tempo_manifest_write <- function(directory, files) {
  path <- .tempo_manifest_path(directory)
  payload <- list(
    manifest_version = 1L,
    package = "TEMPO",
    package_version = tryCatch(
      as.character(utils::packageVersion("TEMPO")),
      error = function(error) "unknown"
    ),
    written_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    files = files
  )

  ok <- tryCatch(
    {
      jsonlite::write_json(
        payload,
        path,
        auto_unbox = TRUE,
        pretty = TRUE,
        null = "null"
      )
      TRUE
    },
    error = function(error) FALSE
  )

  if (!ok) {
    .tempo_warn_once(
      "manifest-write",
      sprintf(
        "Could not write the download manifest to %s; freshness checks will fall back to file modification times.",
        path
      )
    )
  }

  invisible(ok)
}

.tempo_file_digest <- function(path) {
  if (!file.exists(path)) {
    return(NA_character_)
  }
  unname(tools::md5sum(path))
}

.tempo_manifest_entry <- function(code, language, metadata, path, requests) {
  list(
    code = code,
    language = language,
    file = basename(path),
    service_update = .tempo_metadata_update_string(metadata),
    last_update = {
      parsed <- .tempo_metadata_date(metadata, code = code, warn = TRUE)
      if (is.na(parsed)) NULL else format(parsed, "%Y-%m-%d")
    },
    retrieved_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    requests = as.integer(requests),
    bytes = as.numeric(file.info(path)$size),
    md5 = .tempo_file_digest(path)
  )
}

# Returns TRUE when the local CSV can be reused.  `reason` is attached so that
# tempo_bulk() can explain itself.
.tempo_file_is_current <- function(directory, code, language, metadata, path) {
  reason <- function(value, why) structure(value, reason = why)

  if (!file.exists(path)) {
    return(reason(FALSE, "missing"))
  }

  files <- .tempo_manifest_read(directory)
  entry <- files[[code]]

  if (is.null(entry)) {
    # A file written by TEMPO <= 0.1.0, or by something else.  Fall back to the
    # old modification-time test, but say that the language is unverifiable.
    update_date <- .tempo_metadata_date(metadata, code = code, warn = FALSE)
    if (is.na(update_date)) {
      return(reason(FALSE, "unverifiable"))
    }

    modified <- as.Date(file.info(path)$mtime)
    if (!is.na(modified) && modified >= update_date) {
      .tempo_warn_once(
        "manifest-missing",
        paste0(
          "Reusing CSV files that predate the download manifest. ",
          "Their language cannot be verified; delete them or call ",
          "tempo_bulk(overwrite = TRUE) if you need to be certain."
        ),
        class = "tempo_cache_warning"
      )
      return(reason(TRUE, "mtime"))
    }

    return(reason(FALSE, "outdated"))
  }

  if (!identical(as.character(entry$language), language)) {
    return(reason(FALSE, "language"))
  }

  service_update <- .tempo_metadata_update_string(metadata)
  if (!identical(as.character(entry$service_update %||% NA_character_),
                 as.character(service_update %||% NA_character_))) {
    return(reason(FALSE, "revised"))
  }

  recorded <- as.character(entry$md5 %||% NA_character_)
  if (!is.na(recorded) && !identical(recorded, .tempo_file_digest(path))) {
    return(reason(FALSE, "modified"))
  }

  reason(TRUE, "current")
}
