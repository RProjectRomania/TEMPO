#' Download complete TEMPO Online tables
#'
#' Downloads one or more complete tables from the Romanian National Institute
#' of Statistics TEMPO Online database as CSV files.
#'
#' @param codes A character vector (or list) of TEMPO matrix codes, such as
#'   `"ACC101B"`. Use [tempo_toc()] to discover available codes.
#' @param language A single string: `"ro"` for Romanian (the default) or `"en"`
#'   for English.
#' @param directory A single path for the downloaded CSV files. The directory is
#'   created when needed.
#'
#' @return The paths of newly downloaded files, invisibly. Existing requested
#'   files are skipped when their modification date is at least as recent as the
#'   matrix's published update date.
#'
#' @details Large dimensions are requested in sequential chunks to avoid
#' exceeding the TEMPO service's query limit. The function writes only to
#' `directory` and needs an internet connection.
#'
#' @examples
#' \dontrun{
#' data_directory <- file.path(tempdir(), "tempo-data")
#' files <- tempo_bulk("ACC101B", language = "en", directory = data_directory)
#' }
#' @export
tempo_bulk <- function(codes, language = "ro", directory) {
  codes <- .tempo_validate_codes(codes)
  language <- .tempo_validate_language(language)
  directory <- .tempo_prepare_directory(directory)

  downloaded <- character()
  for (code in codes) {
    metadata <- .tempo_matrix_metadata(code, language)
    destination <- file.path(directory, paste0(code, ".csv"))
    update_date <- .tempo_metadata_date(metadata)

    if (.tempo_file_is_current(destination, update_date)) {
      message(sprintf("Skipping `%s`: the local CSV is up to date.", code))
      next
    }

    downloaded[[code]] <- .tempo_download_matrix(
      code = code,
      language = language,
      directory = directory,
      metadata = metadata
    )
  }

  if (!length(downloaded)) {
    message("All requested CSV files are up to date.")
  }

  invisible(downloaded)
}
