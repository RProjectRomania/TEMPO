#' List tables available from TEMPO Online
#'
#' Retrieves the table of contents for the Romanian National Institute of
#' Statistics TEMPO Online database: one row per statistical matrix, with the
#' matrix code needed by [tempo_bulk()], [tempo_get()] and [tempo_meta()].
#'
#' @param full_description A single logical value. If `TRUE`, also retrieve the
#'   statistical domain, sub-domain, survey name and last-update date of every
#'   table. This needs one request per table, which the service answers for
#'   roughly 1500 matrices, so the first call takes a while even though the
#'   requests are issued concurrently and cached afterwards. Default `FALSE`.
#' @param language A single string: `"ro"` for Romanian (the default) or `"en"`
#'   for English.
#' @param cache A single logical value. If `TRUE` (the default), reuse and
#'   populate the response cache described in [tempo_cache_dir()].
#'
#' @return A data frame with one row per matrix and the columns
#'   \describe{
#'     \item{`name`}{Table name in the requested language.}
#'     \item{`code`}{Matrix code, for example `"ACC101B"`.}
#'     \item{`comment`}{Service comment, when the service supplies any.}
#'   }
#'   With `full_description = TRUE` it additionally contains
#'   \describe{
#'     \item{`Statistical_domain`, `Statistical_sub_domain`, `Survey_name`}{The
#'       first three levels of the service's classification, as plain text.}
#'     \item{`Press_releases`}{Press-release URLs that the service embeds in the
#'       domain label, space separated, or `NA`.}
#'     \item{`Last_update`}{Publication date of the matrix, class `Date`.}
#'   }
#'
#' @section Text and HTML:
#' The service embeds HTML anchors in some labels, most visibly in the
#' top-level statistical domains, which arrive as
#' `AGRICULTURE <a href="https://insse.ro/...">Press releases</a>`. Those tags
#' are stripped from every character column, and the URLs are kept separately
#' in `Press_releases`. Table names are returned without Romanian diacritics,
#' because that is how the service stores them.
#'
#' @section Errors:
#' The function needs an internet connection and raises classed conditions, so
#' a caller can distinguish an unreachable service from a malformed response.
#' See [TEMPO-conditions].
#'
#' @seealso [tempo_search()] to filter the result, [tempo_meta()] for the full
#'   metadata of a single matrix, [tempo_hierarchy()] for the classification
#'   tree.
#'
#' @examples
#' \dontrun{
#' tables <- tempo_toc(language = "en")
#' head(tables)
#' nrow(tables)
#'
#' # The slow, fully described variant. Cached, so a second call is cheap.
#' described <- tempo_toc(full_description = TRUE, language = "en")
#' head(described[order(described$Last_update, decreasing = TRUE), ])
#' }
#' @export
tempo_toc <- function(full_description = FALSE, language = "ro", cache = TRUE) {
  full_description <- .tempo_validate_flag(full_description, "full_description")
  language <- .tempo_validate_language(language)
  cache <- .tempo_validate_flag(cache, "cache")

  fetch_toc <- function() {
    .tempo_toc_from_response(
      .tempo_request(.tempo_url("matrix/matrices", language = language))
    )
  }

  toc <- if (isTRUE(cache)) {
    .tempo_cached(
      .tempo_cache_key("matrices", language, .tempo_base_url()),
      fetch_toc
    )
  } else {
    fetch_toc()
  }

  if (!full_description) {
    return(toc)
  }

  metadata <- .tempo_matrix_metadata_many(
    toc$code,
    language = language,
    progress_label = "Fetching table descriptions"
  )

  domain <- vapply(metadata, .tempo_ancestor, character(1), position = 2L)

  toc$Statistical_domain <- .tempo_strip_html(domain)
  toc$Statistical_sub_domain <- .tempo_strip_html(
    vapply(metadata, .tempo_ancestor, character(1), position = 3L)
  )
  toc$Survey_name <- .tempo_strip_html(
    vapply(metadata, .tempo_ancestor, character(1), position = 4L)
  )
  toc$Press_releases <- .tempo_extract_links(domain)
  toc$Last_update <- as.Date(vapply(
    seq_along(metadata),
    function(index) {
      as.numeric(.tempo_metadata_date(metadata[[index]], code = toc$code[[index]]))
    },
    numeric(1)
  ), origin = "1970-01-01")

  toc
}
