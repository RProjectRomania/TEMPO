#' List tables available from TEMPO Online
#'
#' Retrieves the table of contents for the Romanian National Institute of
#' Statistics TEMPO Online database.
#'
#' @param full_description A single logical value. If `TRUE`, retrieve the
#'   domain, sub-domain, survey name, and last-update date for every table.
#'   This makes one additional request per table and can take several minutes.
#' @param language A single string: `"ro"` for Romanian (the default) or `"en"`
#'   for English.
#'
#' @return A data frame with `name` and `code` columns. With
#'   `full_description = TRUE`, it additionally contains `Statistical_domain`,
#'   `Statistical_sub_domain`, `Survey_name`, and `Last_update`.
#'
#' @details The function needs an internet connection. It reports an informative
#'   error when the TEMPO Online service cannot be reached or returns an
#'   unexpected response.
#'
#' @examples
#' \dontrun{
#' tables <- tempo_toc(language = "en")
#' head(tables)
#' }
#' @export
tempo_toc <- function(full_description = FALSE, language = "ro") {
  full_description <- .tempo_validate_flag(full_description, "full_description")
  language <- .tempo_validate_language(language)

  response <- .tempo_request(.tempo_url("matrix/matrices", language = language))
  toc <- .tempo_toc_from_response(response)

  if (full_description) {
    metadata <- lapply(toc$code, .tempo_matrix_metadata, language = language)
    toc$Statistical_domain <- vapply(
      metadata,
      .tempo_ancestor_name,
      character(1),
      position = 2L
    )
    toc$Statistical_sub_domain <- vapply(
      metadata,
      .tempo_ancestor_name,
      character(1),
      position = 3L
    )
    toc$Survey_name <- vapply(
      metadata,
      .tempo_ancestor_name,
      character(1),
      position = 4L
    )
    toc$Last_update <- vapply(metadata, function(item) {
      value <- item$ultimaActualizare
      if (is.null(value) || length(value) != 1L) {
        return(NA_character_)
      }
      as.character(value)
    }, character(1))
  }

  toc
}
