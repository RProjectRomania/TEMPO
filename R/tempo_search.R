#' Search the TEMPO table of contents
#'
#' A convenience wrapper around [tempo_toc()] for the common task of finding
#' the matrix codes whose name matches a pattern.
#'
#' @param pattern A regular expression, or a fixed string when `fixed = TRUE`.
#'   Several patterns are combined with a logical OR.
#' @param language A single string: `"ro"` for Romanian (the default) or `"en"`
#'   for English. The service stores table names without diacritics, so
#'   search for the unaccented spelling, as in `"sanatate"`.
#' @param fields Character vector of columns to search. Defaults to all
#'   character columns of `toc`.
#' @param ignore_case A single logical value, `TRUE` by default.
#' @param fixed A single logical value. If `TRUE`, match `pattern` literally
#'   rather than as a regular expression. Default `FALSE`.
#' @param toc An existing table of contents, as returned by [tempo_toc()]. Pass
#'   one to search a table you already hold, including a fully described one,
#'   instead of fetching it again.
#'
#' @return The matching rows of `toc`, with row names reset.
#'
#' @seealso [tempo_toc()]
#'
#' @examples
#' \dontrun{
#' # Everything about population, in English.
#' tempo_search("population", language = "en")
#'
#' # Two topics at once, searching a table of contents fetched once.
#' tables <- tempo_toc(language = "en")
#' tempo_search(c("unemployment", "employment"), toc = tables)
#'
#' # Search the described table of contents by statistical domain.
#' described <- tempo_toc(full_description = TRUE, language = "en")
#' tempo_search("AGRICULTURE", fields = "Statistical_domain", toc = described)
#' }
#' @export
tempo_search <- function(pattern,
                         language = "ro",
                         fields = NULL,
                         ignore_case = TRUE,
                         fixed = FALSE,
                         toc = NULL) {
  if (!is.character(pattern) || !length(pattern) || anyNA(pattern)) {
    .tempo_abort(
      "`pattern` must be a character vector with at least one element.",
      class = "tempo_input_error"
    )
  }

  ignore_case <- .tempo_validate_flag(ignore_case, "ignore_case")
  fixed <- .tempo_validate_flag(fixed, "fixed")

  if (is.null(toc)) {
    toc <- tempo_toc(language = language)
  }

  if (!is.data.frame(toc) || !nrow(toc)) {
    .tempo_abort(
      "`toc` must be a data frame with at least one row, as returned by tempo_toc().",
      class = "tempo_input_error"
    )
  }

  if (is.null(fields)) {
    fields <- names(toc)[vapply(toc, is.character, logical(1))]
  }

  unknown <- setdiff(fields, names(toc))
  if (length(unknown)) {
    .tempo_abort(
      sprintf(
        "`fields` names columns that are not in `toc`: %s.",
        paste(unknown, collapse = ", ")
      ),
      class = "tempo_input_error"
    )
  }

  haystack <- do.call(
    paste,
    c(lapply(toc[fields], function(column) {
      column <- as.character(column)
      column[is.na(column)] <- ""
      column
    }), list(sep = "\n"))
  )

  matched <- Reduce(
    `|`,
    lapply(pattern, function(one) {
      grepl(one, haystack, ignore.case = ignore_case, fixed = fixed)
    })
  )

  result <- toc[matched, , drop = FALSE]
  rownames(result) <- NULL
  result
}
