#' Download complete TEMPO Online tables
#'
#' Downloads one or more complete matrices from the Romanian National Institute
#' of Statistics TEMPO Online database and writes each as a CSV file. Use
#' [tempo_get()] instead when you want part of a table, or the data in memory.
#'
#' @param codes A character vector (or list) of TEMPO matrix codes, such as
#'   `"ACC101B"`. Use [tempo_toc()] or [tempo_search()] to discover codes.
#' @param language A single string: `"ro"` for Romanian (the default) or `"en"`
#'   for English.
#' @param directory A single path for the downloaded CSV files. It is created
#'   when needed, and nothing outside it is ever written.
#' @param overwrite A single logical value. If `TRUE`, download every requested
#'   matrix even when the local copy is already current. Default `FALSE`.
#' @param progress A single logical value, or `NULL` (the default) to follow
#'   `getOption("TEMPO.progress")`, which in turn defaults to
#'   [base::interactive()].
#'
#' @return Invisibly, a character vector of the CSV paths of **all** requested
#'   matrices, named by matrix code. Three attributes describe what happened:
#'   \describe{
#'     \item{`downloaded`}{Logical, whether the file was fetched in this call.}
#'     \item{`reason`}{Why each file was or was not fetched: `"missing"`,
#'       `"outdated"`, `"revised"`, `"language"`, `"modified"`,
#'       `"unverifiable"`, `"overwrite"` or `"current"`.}
#'     \item{`requests`}{Number of HTTP requests spent on each matrix.}
#'   }
#'   Up to and including version 0.1.0 only the newly written paths were
#'   returned, which forced callers to rebuild the paths of skipped files by
#'   hand.
#'
#' @section Freshness, and why a manifest:
#' Alongside the CSV files, `tempo_bulk()` maintains `.tempo-manifest.json`,
#' recording for every file the language, the service's own update string, the
#' retrieval time, the number of requests and an MD5 checksum. A file is reused
#' only when all of the language, the published revision and the checksum
#' agree.
#'
#' A CSV file does not record which language it holds, so the earlier
#' modification-time test accepted a Romanian file for an English request and
#' reported it as up to date. Files written before the manifest existed are
#' still accepted on the old modification-time rule, with a warning that their
#' language cannot be verified; delete them, or pass `overwrite = TRUE`, to be
#' certain.
#'
#' @section Query size:
#' A full matrix can exceed what the service will answer in one query, so the
#' request is split into chunks whose product of selected values stays within
#' `getOption("TEMPO.max_cells")`. The parts are concatenated, and each part's
#' CSV header is checked against the first part's so that misaligned columns
#' are an error rather than a corrupt file. See [TEMPO-options].
#'
#' @seealso [tempo_get()] for filtered, in-memory downloads, [tempo_tidy()] to
#'   turn a downloaded CSV into analysis-ready data, [tempo_meta()] to size a
#'   table first.
#'
#' @examples
#' \dontrun{
#' data_directory <- file.path(tempdir(), "tempo-data")
#'
#' files <- tempo_bulk("ACC101B", language = "en", directory = data_directory)
#' files
#' attr(files, "reason")
#'
#' # A second call reuses the file and says so.
#' tempo_bulk("ACC101B", language = "en", directory = data_directory)
#'
#' # Paths are returned for skipped files too, so this always works.
#' accidents <- tempo_tidy(files[["ACC101B"]])
#' head(accidents)
#' }
#' @export
tempo_bulk <- function(codes,
                       language = "ro",
                       directory,
                       overwrite = FALSE,
                       progress = NULL) {
  codes <- .tempo_validate_codes(codes)
  language <- .tempo_validate_language(language)
  overwrite <- .tempo_validate_flag(overwrite, "overwrite")
  if (!is.null(progress)) {
    progress <- .tempo_validate_flag(progress, "progress")
  }
  directory <- .tempo_prepare_directory(directory)

  show_progress <- if (is.null(progress)) .tempo_progress_enabled() else progress

  paths <- character(length(codes))
  downloaded <- logical(length(codes))
  reasons <- character(length(codes))
  requests <- integer(length(codes))
  names(paths) <- codes

  manifest <- .tempo_manifest_read(directory)
  manifest_changed <- FALSE

  for (index in seq_along(codes)) {
    code <- codes[[index]]
    metadata <- .tempo_matrix_metadata(code, language)
    destination <- file.path(directory, paste0(code, ".csv"))
    paths[[index]] <- destination

    current <- if (overwrite) {
      structure(FALSE, reason = "overwrite")
    } else {
      .tempo_file_is_current(directory, code, language, metadata, destination)
    }
    reasons[[index]] <- attr(current, "reason")

    if (isTRUE(current)) {
      message(sprintf("Skipping `%s`: the local CSV is up to date.", code))
      next
    }

    written <- .tempo_download_matrix(
      code = code,
      language = language,
      directory = directory,
      metadata = metadata,
      progress = show_progress
    )

    downloaded[[index]] <- TRUE
    requests[[index]] <- attr(written, "requests") %||% NA_integer_
    manifest[[code]] <- .tempo_manifest_entry(
      code = code,
      language = language,
      metadata = metadata,
      path = destination,
      requests = requests[[index]]
    )
    manifest_changed <- TRUE
  }

  if (manifest_changed) {
    .tempo_manifest_write(directory, manifest)
  }

  if (!any(downloaded)) {
    message("All requested CSV files are up to date.")
  }

  attr(paths, "downloaded") <- downloaded
  attr(paths, "reason") <- reasons
  attr(paths, "requests") <- requests
  invisible(paths)
}
