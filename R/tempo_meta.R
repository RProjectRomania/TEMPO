#' Describe a TEMPO matrix without downloading it
#'
#' Returns the structure of a single matrix: its dimensions, the selectable
#' values of each dimension, the publication date, the position in the
#' statistical classification, and how large a full download would be. This is
#' the information needed to choose sensible `filters` for [tempo_get()], and
#' it answers "how big is this table?" in one request instead of a download.
#'
#' @param code A single TEMPO matrix code, such as `"ACC101B"`. Use
#'   [tempo_toc()] or [tempo_search()] to find one.
#' @param language A single string: `"ro"` for Romanian (the default) or `"en"`
#'   for English. Dimension and value labels are returned in this language.
#' @param cache A single logical value. If `TRUE` (the default), reuse and
#'   populate the response cache described in [tempo_cache_dir()].
#'
#' @return An object of class `"tempo_meta"`: a list with components
#'   \describe{
#'     \item{`code`, `name`, `language`}{Identification.}
#'     \item{`last_update`}{Publication date as a `Date`, or `NA` when the
#'       service reports a date this package cannot parse.}
#'     \item{`service_update`}{The service's own update string, verbatim.}
#'     \item{`domain`, `sub_domain`, `survey`}{Classification, as plain text.}
#'     \item{`dimensions`}{A data frame with one row per dimension, giving its
#'       `position`, `name` and number of `values`.}
#'     \item{`values`}{A named list with one data frame per dimension, each
#'       holding the `id` and `label` of every selectable value.}
#'     \item{`cells`}{Number of cells a full download would request.}
#'     \item{`requests`}{Number of HTTP requests a full download would need
#'       under the current query-size options.}
#'     \item{`details`}{The service's query parameters (`matMaxDim`,
#'       `matUMSpec`).}
#'   }
#'   The unparsed service response is attached as the `"raw"` attribute.
#'
#' @section Undocumented field names:
#' TEMPO Online has no published API contract. The dimension and value labels
#' are read from the first of several plausible field names
#' (`label`, `name`, `nomItemName`, `text`), and dimensions fall back to
#' `dimension_1`, `dimension_2`, ... if none of them is present. The value
#' identifiers, which the query payload actually needs, come from the
#' documented-by-use `nomItemId` field and are validated.
#'
#' @seealso [tempo_get()] to download a subset, [tempo_bulk()] for whole
#'   tables, [tempo_toc()] to list matrices.
#'
#' @examples
#' \dontrun{
#' meta <- tempo_meta("ACC101B", language = "en")
#' meta
#'
#' # What can be filtered, and how large is the table?
#' meta$dimensions
#' meta$cells
#'
#' # The selectable values of the first dimension.
#' head(meta$values[[1]])
#' }
#' @export
tempo_meta <- function(code, language = "ro", cache = TRUE) {
  code <- .tempo_validate_code(code)
  language <- .tempo_validate_language(language)
  cache <- .tempo_validate_flag(cache, "cache")

  metadata <- .tempo_matrix_metadata(code, language, cache = cache)

  names <- .tempo_dimension_names(metadata, code)
  ids <- .tempo_option_ids(metadata, code)
  labels <- .tempo_option_labels(metadata, code)

  values <- lapply(seq_along(ids), function(index) {
    data.frame(
      id = ids[[index]],
      label = labels[[index]],
      stringsAsFactors = FALSE
    )
  })
  names(values) <- names

  sizes <- vapply(ids, length, integer(1))
  plan <- tryCatch(
    .tempo_plan_query(ids, code = code),
    tempo_error = function(condition) NULL
  )

  structure(
    list(
      code = code,
      name = .tempo_first_field(
        metadata,
        c("matrixName", "name", "nume", "label"),
        default = NA_character_
      ),
      language = language,
      last_update = .tempo_metadata_date(metadata, code = code),
      service_update = .tempo_metadata_update_string(metadata),
      domain = .tempo_strip_html(.tempo_ancestor(metadata, 2L)),
      sub_domain = .tempo_strip_html(.tempo_ancestor(metadata, 3L)),
      survey = .tempo_strip_html(.tempo_ancestor(metadata, 4L)),
      dimensions = data.frame(
        position = seq_along(names),
        name = names,
        values = sizes,
        stringsAsFactors = FALSE
      ),
      values = values,
      cells = prod(as.numeric(sizes)),
      requests = if (is.null(plan)) NA_integer_ else attr(plan, "requests"),
      details = list(
        matMaxDim = metadata$details$matMaxDim,
        matUMSpec = metadata$details$matUMSpec
      )
    ),
    class = "tempo_meta",
    raw = metadata
  )
}

#' @param x A `tempo_meta` object.
#' @param ... Ignored.
#' @rdname tempo_meta
#' @export
print.tempo_meta <- function(x, ...) {
  cat(sprintf("<tempo_meta> %s", x$code))
  if (!is.na(x$name)) {
    cat(sprintf(" \u2014 %s", x$name))
  }
  cat("\n")

  classification <- c(x$domain, x$sub_domain, x$survey)
  classification <- classification[!is.na(classification)]
  if (length(classification)) {
    cat(sprintf("  %s\n", paste(classification, collapse = " > ")))
  }

  cat(sprintf("  language:    %s\n", x$language))
  cat(sprintf(
    "  last update: %s\n",
    if (is.na(x$last_update)) {
      sprintf("unknown (service reported \"%s\")", x$service_update)
    } else {
      format(x$last_update, "%Y-%m-%d")
    }
  ))
  cat(sprintf(
    "  full table:  %s cells, %s request(s)\n",
    format(x$cells, big.mark = ",", scientific = FALSE, trim = TRUE),
    if (is.na(x$requests)) "too many" else x$requests
  ))

  cat(sprintf("  dimensions:  %d\n", nrow(x$dimensions)))
  for (index in seq_len(nrow(x$dimensions))) {
    row <- x$dimensions[index, ]
    examples <- utils::head(x$values[[index]]$label, 3L)
    examples <- examples[!is.na(examples)]
    cat(sprintf(
      "    %d. %s (%d value%s)%s\n",
      row$position,
      row$name,
      row$values,
      if (row$values == 1L) "" else "s",
      if (length(examples)) {
        sprintf(
          ": %s%s",
          paste(examples, collapse = ", "),
          if (row$values > length(examples)) ", ..." else ""
        )
      } else {
        ""
      }
    ))
  }

  invisible(x)
}
