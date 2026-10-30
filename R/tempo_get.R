# Filter resolution -----------------------------------------------------------

.tempo_escape_regex <- function(x) {
  gsub("([.|()\\^{}+$*?\\[\\]])", "\\\\\\1", x, perl = TRUE)
}

.tempo_normalise <- function(x) {
  tolower(trimws(as.character(x)))
}

.tempo_match_dimension <- function(key, names) {
  exact <- which(.tempo_normalise(names) == .tempo_normalise(key))
  if (length(exact) == 1L) {
    return(exact)
  }
  if (length(exact) > 1L) {
    .tempo_abort(
      sprintf(
        "`filters` name \"%s\" matches %d dimensions; use the position instead.",
        key,
        length(exact)
      ),
      class = "tempo_input_error"
    )
  }

  index <- suppressWarnings(as.integer(key))
  if (!is.na(index) && index >= 1L && index <= length(names)) {
    return(index)
  }

  partial <- grep(
    .tempo_escape_regex(trimws(key)),
    names,
    ignore.case = TRUE
  )
  if (length(partial) == 1L) {
    return(partial)
  }

  .tempo_abort(
    sprintf(
      "`filters` names an unknown dimension \"%s\". Available dimensions: %s.",
      key,
      paste(sprintf("%d. %s", seq_along(names), names), collapse = "; ")
    ),
    class = "tempo_input_error"
  )
}

.tempo_match_values <- function(want, ids, labels, dimension) {
  want <- as.character(unlist(want, use.names = FALSE))
  want <- want[!is.na(want)]
  if (!length(want)) {
    .tempo_abort(
      sprintf("`filters` for dimension \"%s\" selects no values.", dimension),
      class = "tempo_input_error"
    )
  }

  chosen <- integer()
  unmatched <- character()

  for (value in want) {
    hit <- which(.tempo_normalise(labels) == .tempo_normalise(value))

    if (!length(hit)) {
      # Whole-token match, so that 2020 finds "Anul 2020" but not "12020".
      hit <- grep(
        sprintf(
          "(^|[^[:alnum:]])%s([^[:alnum:]]|$)",
          .tempo_escape_regex(trimws(value))
        ),
        labels,
        ignore.case = TRUE
      )
    }

    if (!length(hit)) {
      hit <- which(ids == trimws(value))
    }

    if (!length(hit)) {
      unmatched <- c(unmatched, value)
    } else {
      chosen <- c(chosen, hit)
    }
  }

  if (length(unmatched)) {
    available <- labels[!is.na(labels)]
    shown <- utils::head(available, 10L)
    .tempo_abort(
      sprintf(
        paste0(
          "No value of dimension \"%s\" matches %s. ",
          "Available values%s: %s."
        ),
        dimension,
        paste(sprintf("\"%s\"", unmatched), collapse = ", "),
        if (length(available) > length(shown)) {
          sprintf(" (first %d of %d)", length(shown), length(available))
        } else {
          ""
        },
        paste(shown, collapse = "; ")
      ),
      class = "tempo_input_error"
    )
  }

  ids[sort(unique(chosen))]
}

.tempo_resolve_filters <- function(filters, metadata, code) {
  ids <- .tempo_option_ids(metadata, code)

  if (is.null(filters) || !length(filters)) {
    return(ids)
  }

  if (!is.list(filters) || is.null(names(filters)) ||
      any(is.na(names(filters))) || any(!nzchar(names(filters)))) {
    .tempo_abort(
      paste0(
        "`filters` must be a named list, where each name is a dimension of the ",
        "matrix and each value selects from that dimension, as in ",
        "list(Judete = \"Cluj\", Ani = 2015:2024)."
      ),
      class = "tempo_input_error"
    )
  }

  names <- .tempo_dimension_names(metadata, code)
  labels <- .tempo_option_labels(metadata, code)
  selection <- ids

  for (key in names(filters)) {
    position <- .tempo_match_dimension(key, names)
    wanted <- filters[[key]]
    if (is.null(wanted)) {
      next
    }

    selection[[position]] <- .tempo_match_values(
      wanted,
      ids[[position]],
      labels[[position]],
      names[[position]]
    )
  }

  selection
}

#' Download part of a TEMPO Online table into R
#'
#' Fetches a matrix, or a selection from it, and returns it as a data frame.
#' Nothing is written to disk. Unlike [tempo_bulk()], which always downloads a
#' whole table to CSV, `tempo_get()` sends the dimension selection to the
#' service, so asking for one county and ten years costs one small request
#' rather than a download of the entire table.
#'
#' @param code A single TEMPO matrix code, such as `"ACC101B"`.
#' @param filters A named list selecting values from one or more dimensions, or
#'   `NULL` (the default) for the complete table. Names are dimension names, as
#'   reported by [tempo_meta()]; a partial, case-insensitive name or a
#'   dimension position also works. Dimensions you do not mention, or set to
#'   `NULL`, are taken whole. See Matching below.
#' @param language A single string: `"ro"` for Romanian (the default) or `"en"`
#'   for English. Dimension names, value labels and the values you filter on
#'   are all in this language.
#' @param tidy A single logical value. If `TRUE` (the default), the result is
#'   passed through [tempo_tidy()]: a numeric `value` column, the unit of
#'   measure as a column, parsed periods and no footer rows. Set `FALSE` to get
#'   the service's own CSV layout, every column a character vector.
#' @param cache A single logical value, passed to [tempo_meta()] for the
#'   metadata lookup. The data itself is never cached. Default `TRUE`.
#' @param progress A single logical value, or `NULL` (the default) to follow
#'   `getOption("TEMPO.progress")`.
#' @param ... Further arguments passed to [tempo_tidy()], such as
#'   `periods = FALSE` or `clean_names = TRUE`.
#'
#' @return A data frame. These attributes record the provenance of the result:
#'   `tempo_code`, `tempo_language`, `tempo_requests`,
#'   `tempo_service_update` and `tempo_retrieved_at`.
#'
#' @section Matching:
#' Each element of a filter is matched against the dimension's value labels,
#' in three steps: an exact match ignoring case and surrounding space; then a
#' whole-token match, so `2020` finds `"Anul 2020"`; then an exact match against
#' the service's own numeric value identifiers. An element that matches nothing
#' is an error listing the available values, rather than a silently smaller
#' result.
#'
#' @section Size:
#' A selection still has to fit the service's query limits, and a large one is
#' split into several requests exactly as in [tempo_bulk()]. Call
#' [tempo_meta()] first to see how many cells and requests a selection implies.
#'
#' @seealso [tempo_meta()] for the dimensions available to filter on,
#'   [tempo_bulk()] for whole tables as CSV files, [tempo_tidy()] for the
#'   cleaning rules.
#'
#' @examples
#' \dontrun{
#' # What can be filtered?
#' tempo_meta("ACC101B", language = "en")
#'
#' # The whole table, tidied, in memory.
#' accidents <- tempo_get("ACC101B", language = "en")
#' head(accidents)
#'
#' # One county and a decade: a small request instead of a full download.
#' cluj <- tempo_get(
#'   "ACC101B",
#'   filters = list(Counties = "Cluj", Years = 2015:2024),
#'   language = "en"
#' )
#' head(cluj)
#'
#' # The service's own layout, untouched.
#' raw <- tempo_get("ACC101B", language = "en", tidy = FALSE)
#' str(raw)
#' }
#' @export
tempo_get <- function(code,
                      filters = NULL,
                      language = "ro",
                      tidy = TRUE,
                      cache = TRUE,
                      progress = NULL,
                      ...) {
  code <- .tempo_validate_code(code)
  language <- .tempo_validate_language(language)
  tidy <- .tempo_validate_flag(tidy, "tidy")
  cache <- .tempo_validate_flag(cache, "cache")
  if (!is.null(progress)) {
    progress <- .tempo_validate_flag(progress, "progress")
  }

  metadata <- .tempo_matrix_metadata(code, language, cache = cache)
  selection <- .tempo_resolve_filters(filters, metadata, code)
  plan <- .tempo_plan_query(selection, code = code)

  show_progress <- if (is.null(progress)) .tempo_progress_enabled() else progress
  label <- if (isTRUE(show_progress) && attr(plan, "requests") > 1L) {
    sprintf("Fetching %s", code)
  } else {
    NULL
  }

  text <- .tempo_fetch_matrix(
    code = code,
    language = language,
    metadata = metadata,
    plan = plan,
    progress_label = label
  )

  result <- .tempo_read_csv_text(text)

  if (tidy) {
    result <- tempo_tidy(result, code = code, ...)
  }

  attr(result, "tempo_code") <- code
  attr(result, "tempo_language") <- language
  attr(result, "tempo_requests") <- attr(text, "requests")
  attr(result, "tempo_service_update") <- .tempo_metadata_update_string(metadata)
  attr(result, "tempo_retrieved_at") <- Sys.time()
  result
}
