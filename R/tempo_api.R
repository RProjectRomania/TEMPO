# Service endpoints, metadata and query planning ------------------------------

.tempo_base_url <- function() {
  as.character(.tempo_opt("base_url"))[[1L]]
}

.tempo_url <- function(path, language = NULL) {
  url <- paste0(.tempo_base_url(), "/", path)
  if (!is.null(language) && identical(language, "en")) {
    url <- paste0(url, "?lang=en")
  }

  url
}

# Input validation ------------------------------------------------------------

.tempo_validate_language <- function(language) {
  if (
    !is.character(language) ||
      length(language) != 1L ||
      is.na(language) ||
      !language %in% c("ro", "en")
  ) {
    .tempo_abort(
      "`language` must be either \"ro\" or \"en\".",
      class = "tempo_input_error"
    )
  }

  language
}

.tempo_validate_flag <- function(value, name) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    .tempo_abort(
      sprintf("`%s` must be either `TRUE` or `FALSE`.", name),
      class = "tempo_input_error"
    )
  }

  value
}

.tempo_validate_codes <- function(codes) {
  if (is.list(codes)) {
    codes <- unlist(codes, recursive = TRUE, use.names = FALSE)
  }

  if (!is.character(codes) || !length(codes) || anyNA(codes)) {
    .tempo_abort(
      "`codes` must contain at least one TEMPO matrix code.",
      class = "tempo_input_error"
    )
  }

  codes <- trimws(codes)
  if (any(!nzchar(codes)) || any(!grepl("^[[:alnum:]]+$", codes))) {
    .tempo_abort(
      "Each element of `codes` must contain only letters and numbers.",
      class = "tempo_input_error"
    )
  }

  unique(codes)
}

.tempo_validate_code <- function(code) {
  code <- .tempo_validate_codes(code)
  if (length(code) != 1L) {
    .tempo_abort(
      "`code` must be a single TEMPO matrix code.",
      class = "tempo_input_error"
    )
  }

  code
}

# JSON helpers ----------------------------------------------------------------

.tempo_parse_json <- function(response, context, simplify_vector = FALSE) {
  text <- .tempo_response_text(response, context)

  tryCatch(
    jsonlite::fromJSON(text, simplifyVector = simplify_vector),
    error = function(error) {
      .tempo_abort(
        sprintf(
          "The TEMPO Online service returned invalid %s JSON: %s",
          context,
          conditionMessage(error)
        ),
        class = "tempo_api_error"
      )
    }
  )
}

# Pick the first non-empty field from a list of candidate names.  The service's
# field names are not formally documented, so the accessors below accept the
# plausible spellings rather than assuming one.
.tempo_first_field <- function(x, names, default = NA_character_) {
  if (!is.list(x)) {
    return(default)
  }

  for (name in names) {
    value <- x[[name]]
    if (!is.null(value) && length(value) == 1L && !is.na(value) &&
        nzchar(as.character(value))) {
      return(as.character(value))
    }
  }

  default
}

# HTML in service labels ------------------------------------------------------
#
# The top-level statistical domains returned by the service embed HTML anchors,
# for example
#
#   AGRICULTURE <a href="https://insse.ro/...">Press releases</a>
#
# so the raw value is unusable in a data frame.  .tempo_strip_html() keeps the
# text, .tempo_extract_links() keeps the URLs.

.tempo_strip_html <- function(x) {
  x <- as.character(x)
  missing <- is.na(x)

  entities <- function(value) {
    value <- gsub("&nbsp;", " ", value, fixed = TRUE)
    value <- gsub("&amp;", "&", value, fixed = TRUE)
    value <- gsub("&lt;", "<", value, fixed = TRUE)
    value <- gsub("&gt;", ">", value, fixed = TRUE)
    value <- gsub("&quot;", "\"", value, fixed = TRUE)
    gsub("&#39;", "'", value, fixed = TRUE)
  }

  tidy <- function(value) {
    value <- gsub("<[^>]*>", "", value)
    value <- entities(value)
    value <- gsub("[[:space:]]+", " ", value)
    value <- trimws(value)
    gsub("^[;,[:space:]]+|[;,[:space:]]+$", "", value)
  }

  # An anchor in a service label is usually navigation chrome appended to the
  # real name, as in `AGRICULTURE <a ...>Press releases</a>`, so the whole
  # element goes.  But a label that is nothing but a link would then vanish, so
  # in that case the link text is kept instead.
  without_anchors <- tidy(
    gsub("<a\\b[^>]*>.*?</a>", " ", x, perl = TRUE, ignore.case = TRUE)
  )
  with_text <- tidy(x)

  result <- ifelse(nzchar(without_anchors), without_anchors, with_text)
  result[missing] <- NA_character_
  result
}

.tempo_extract_links <- function(x, collapse = " ") {
  vapply(as.character(x), function(value) {
    if (is.na(value)) {
      return(NA_character_)
    }

    matches <- regmatches(value, gregexpr('href="[^"]*"', value))[[1L]]
    if (!length(matches)) {
      return(NA_character_)
    }

    paste(sub('^href="', "", sub('"$', "", matches)), collapse = collapse)
  }, character(1), USE.NAMES = FALSE)
}

# Table of contents -----------------------------------------------------------

.tempo_toc_from_response <- function(response) {
  toc <- .tempo_parse_json(response, "table-of-contents", simplify_vector = TRUE)
  if (!is.data.frame(toc) || !all(c("name", "code") %in% names(toc))) {
    .tempo_abort(
      "The TEMPO Online service returned an unexpected table-of-contents format.",
      class = "tempo_api_error"
    )
  }

  result <- data.frame(
    name = .tempo_strip_html(as.character(toc$name)),
    code = as.character(toc$code),
    stringsAsFactors = FALSE
  )

  if ("comment" %in% names(toc)) {
    comment <- .tempo_strip_html(as.character(toc$comment))
    comment[!nzchar(comment)] <- NA_character_
    if (any(!is.na(comment))) {
      result$comment <- comment
    }
  }

  rownames(result) <- NULL
  result
}

# Matrix metadata -------------------------------------------------------------

.tempo_matrix_metadata <- function(code, language, cache = TRUE) {
  fetch <- function() {
    response <- .tempo_request(
      .tempo_url(paste0("matrix/", code), language = language)
    )
    .tempo_validate_metadata(
      .tempo_parse_json(response, sprintf("metadata for `%s`", code)),
      code
    )
  }

  if (!isTRUE(cache)) {
    return(fetch())
  }

  .tempo_cached(
    .tempo_cache_key("matrix", code, language, .tempo_base_url()),
    fetch
  )
}

.tempo_validate_metadata <- function(metadata, code) {
  if (!is.list(metadata) || is.null(metadata$dimensionsMap) ||
      is.null(metadata$details)) {
    .tempo_abort(
      sprintf(
        "The TEMPO Online service returned incomplete metadata for `%s`.",
        code
      ),
      class = "tempo_api_error",
      code = code
    )
  }

  metadata
}

# Several matrices at once, using the concurrent request pool.
.tempo_matrix_metadata_many <- function(codes, language, progress_label = NULL) {
  keys <- vapply(
    codes,
    function(code) .tempo_cache_key("matrix", code, language, .tempo_base_url()),
    character(1),
    USE.NAMES = FALSE
  )

  metadata <- lapply(keys, .tempo_cache_get)
  missing <- which(vapply(metadata, is.null, logical(1)))

  if (length(missing)) {
    urls <- vapply(
      codes[missing],
      function(code) .tempo_url(paste0("matrix/", code), language = language),
      character(1),
      USE.NAMES = FALSE
    )

    responses <- .tempo_request_many(urls, progress_label = progress_label)

    for (position in seq_along(missing)) {
      index <- missing[[position]]
      parsed <- .tempo_validate_metadata(
        .tempo_parse_json(
          responses[[position]],
          sprintf("metadata for `%s`", codes[[index]])
        ),
        codes[[index]]
      )
      metadata[[index]] <- parsed
      .tempo_cache_set(keys[[index]], parsed)
    }
  }

  metadata
}

.tempo_metadata_update_string <- function(metadata) {
  value <- metadata$ultimaActualizare
  if (is.null(value) || length(value) != 1L || is.na(value)) {
    return(NA_character_)
  }

  trimws(as.character(value))
}

# The service has published "dd-mm-yyyy" historically, but the format is not
# contractual.  Try the plausible layouts and say so when none of them fits,
# instead of silently returning NA and re-downloading for ever.
.tempo_date_formats <- c("%d-%m-%Y", "%d.%m.%Y", "%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d")

.tempo_metadata_date <- function(metadata, code = NULL, warn = TRUE) {
  value <- .tempo_metadata_update_string(metadata)
  if (is.na(value) || !nzchar(value)) {
    return(as.Date(NA))
  }

  for (format in .tempo_date_formats) {
    parsed <- as.Date(value, format = format)
    if (!is.na(parsed)) {
      return(parsed)
    }
  }

  if (isTRUE(warn)) {
    .tempo_warn_once(
      paste0("date-format-", value),
      sprintf(
        paste0(
          "Could not parse the update date \"%s\"%s. Freshness checks fall ",
          "back to the service's raw update string, and `Last_update` is NA. ",
          "Please report this at ",
          "https://github.com/RProjectRomania/TEMPO/issues"
        ),
        value,
        if (is.null(code)) "" else sprintf(" reported for `%s`", code)
      ),
      class = "tempo_date_warning"
    )
  }

  as.Date(NA)
}

.tempo_ancestor <- function(metadata, position) {
  ancestors <- metadata$ancestors
  if (!is.list(ancestors) || length(ancestors) < position) {
    return(NA_character_)
  }

  .tempo_first_field(ancestors[[position]], c("name", "label", "context"))
}

# Dimensions ------------------------------------------------------------------

.tempo_dimensions <- function(metadata, code) {
  dimensions <- metadata$dimensionsMap
  if (!is.list(dimensions) || !length(dimensions)) {
    .tempo_abort(
      sprintf("Matrix `%s` has no dimensions to download.", code),
      class = "tempo_api_error",
      code = code
    )
  }

  dimensions
}

.tempo_dimension_names <- function(metadata, code) {
  dimensions <- .tempo_dimensions(metadata, code)
  names <- vapply(
    seq_along(dimensions),
    function(index) {
      .tempo_first_field(
        dimensions[[index]],
        c("label", "name", "dimName", "nomName", "text"),
        default = sprintf("dimension_%d", index)
      )
    },
    character(1)
  )

  .tempo_strip_html(names)
}

.tempo_option_ids <- function(metadata, code) {
  dimensions <- .tempo_dimensions(metadata, code)

  lapply(seq_along(dimensions), function(index) {
    options <- dimensions[[index]]$options
    if (!is.list(options) || !length(options)) {
      .tempo_abort(
        sprintf(
          "Matrix `%s` has no selectable options in dimension %s.",
          code,
          index
        ),
        class = "tempo_api_error",
        code = code
      )
    }

    ids <- vapply(options, function(option) {
      value <- option$nomItemId
      if (is.null(value) || length(value) != 1L || is.na(value)) {
        return(NA_character_)
      }

      as.character(value)
    }, character(1))

    if (anyNA(ids) || any(!nzchar(ids))) {
      .tempo_abort(
        sprintf(
          "Matrix `%s` contains an invalid option identifier in dimension %s.",
          code,
          index
        ),
        class = "tempo_api_error",
        code = code
      )
    }

    ids
  })
}

.tempo_option_labels <- function(metadata, code) {
  dimensions <- .tempo_dimensions(metadata, code)

  lapply(seq_along(dimensions), function(index) {
    options <- dimensions[[index]]$options
    labels <- vapply(
      options,
      .tempo_first_field,
      character(1),
      names = c("label", "name", "nomItemName", "text")
    )
    .tempo_strip_html(labels)
  })
}

# Query planning --------------------------------------------------------------
#
# The service refuses or times out on very large queries.  Two limits matter,
# and the previous implementation only enforced the first:
#
#   * a per-dimension cap on how many options one query may select;
#   * the total number of cells the query asks for, which is the product of the
#     selected option counts across all dimensions.
#
# Chunking each dimension independently against a fixed threshold therefore
# split small matrices needlessly while letting genuinely huge requests through
# untouched: five dimensions of 200 options each is 3.2e11 cells in a single
# request, yet no single dimension crosses a 300-option threshold.
# .tempo_plan_query() budgets on the product instead, shrinking the largest
# dimension until the query fits.

.tempo_chunk <- function(x, size) {
  size <- max(1L, as.integer(size))
  if (length(x) <= size) {
    return(list(x))
  }

  unname(split(x, ceiling(seq_along(x) / size)))
}

.tempo_plan_query <- function(option_ids,
                              code = NULL,
                              max_cells = .tempo_opt("max_cells"),
                              max_options = .tempo_opt("max_options_per_dim"),
                              max_requests = .tempo_opt("max_requests")) {
  sizes <- vapply(option_ids, length, integer(1))
  if (!length(sizes) || any(sizes == 0L)) {
    .tempo_abort(
      sprintf(
        "Matrix `%s` has no selectable options to download.",
        code %||% "unknown"
      ),
      class = "tempo_api_error",
      code = code
    )
  }

  max_cells <- max(1, as.numeric(max_cells))
  max_options <- max(1L, as.integer(max_options))

  caps <- pmin(sizes, max_options)

  # Choose the largest per-dimension cap k for which selecting at most k values
  # from every dimension keeps the query within the cell budget.  Capping every
  # dimension at the same k keeps the split balanced, which needs fewer
  # requests than shrinking one dimension to the bone.
  fits <- function(k) prod(as.numeric(pmin(caps, k))) <= max_cells

  chunk <- if (fits(max(caps))) {
    caps
  } else {
    low <- 1L
    high <- max(caps)
    while (low < high) {
      middle <- as.integer((low + high + 1L) %/% 2L)
      if (fits(middle)) low <- middle else high <- middle - 1L
    }
    pmin(caps, low)
  }

  requests <- prod(ceiling(as.numeric(sizes) / as.numeric(chunk)))
  if (requests > as.numeric(max_requests)) {
    .tempo_abort(
      sprintf(
        paste0(
          "Downloading matrix `%s` would need %.0f requests, above the limit ",
          "of %.0f. Select fewer values with tempo_get(), or raise ",
          "`options(TEMPO.max_requests = )` if you are sure."
        ),
        code %||% "unknown",
        requests,
        as.numeric(max_requests)
      ),
      class = "tempo_input_error",
      code = code,
      requests = requests
    )
  }

  structure(
    Map(.tempo_chunk, option_ids, chunk),
    cells = prod(as.numeric(sizes)),
    requests = as.integer(requests),
    chunk_sizes = as.integer(chunk)
  )
}

# Walk the Cartesian product of the per-dimension chunks.
.tempo_each_selection <- function(option_chunks, callback) {
  positions <- rep.int(1L, length(option_chunks))

  repeat {
    selection <- Map(
      function(chunks, position) chunks[[position]],
      option_chunks,
      positions
    )
    callback(selection)

    position <- length(positions)
    while (position > 0L &&
           positions[[position]] == length(option_chunks[[position]])) {
      positions[[position]] <- 1L
      position <- position - 1L
    }

    if (position == 0L) {
      break
    }

    positions[[position]] <- positions[[position]] + 1L
  }

  invisible(NULL)
}

.tempo_payload <- function(code, language, metadata, option_ids) {
  details <- metadata$details
  if (is.null(details$matMaxDim) || is.null(details$matUMSpec)) {
    .tempo_abort(
      sprintf("Matrix `%s` has incomplete query metadata.", code),
      class = "tempo_api_error",
      code = code
    )
  }

  list(
    language = language,
    encQuery = paste(
      vapply(option_ids, paste, collapse = ", ", character(1)),
      collapse = ":"
    ),
    matCode = code,
    matMaxDim = details$matMaxDim,
    matUMSpec = details$matUMSpec
  )
}

.tempo_pivot <- function(code, language, metadata, selection) {
  payload <- .tempo_payload(code, language, metadata, selection)
  response <- .tempo_request(
    .tempo_url("pivot"),
    method = "POST",
    body = jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  )

  content <- response$content
  if (!is.raw(content) || !length(content)) {
    .tempo_abort(
      sprintf("The TEMPO Online service returned an empty CSV for `%s`.", code),
      class = "tempo_api_error",
      code = code
    )
  }

  content
}

# CSV assembly ----------------------------------------------------------------

.tempo_csv_split <- function(content, code = NULL) {
  newline <- match(as.raw(10L), content)
  if (is.na(newline)) {
    .tempo_abort(
      "The TEMPO Online service returned a CSV response without a header line.",
      class = "tempo_api_error",
      code = code
    )
  }

  body <- if (newline == length(content)) {
    raw()
  } else {
    content[seq.int(newline + 1L, length(content))]
  }

  list(
    header = content[seq_len(newline)],
    body = body
  )
}

.tempo_without_csv_header <- function(content, code = NULL) {
  .tempo_csv_split(content, code = code)$body
}

.tempo_header_text <- function(header) {
  text <- rawToChar(header)
  Encoding(text) <- "UTF-8"
  sub("[\r\n]+$", "", text)
}

.tempo_write_raw <- function(content, path, append = FALSE) {
  connection <- file(path, open = if (append) "ab" else "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(content, connection, useBytes = TRUE)
  invisible(path)
}

# Downloading -----------------------------------------------------------------
#
# Chunked responses are concatenated byte-wise, which keeps memory flat for
# very large matrices.  That is only sound if every chunk carries the same
# header, so each chunk's header is compared with the first one's and a
# mismatch is an error rather than a silently misaligned file.

.tempo_collect_matrix <- function(code, language, metadata, plan, sink,
                                  progress_label = NULL) {
  reporter <- .tempo_reporter(attr(plan, "requests"), progress_label)
  first_header <- NULL
  requests <- 0L

  .tempo_each_selection(plan, function(selection) {
    content <- .tempo_pivot(code, language, metadata, selection)
    parts <- .tempo_csv_split(content, code = code)
    requests <<- requests + 1L

    if (is.null(first_header)) {
      first_header <<- parts$header
      sink(content, append = FALSE)
    } else {
      if (!identical(
        .tempo_header_text(parts$header),
        .tempo_header_text(first_header)
      )) {
        .tempo_abort(
          sprintf(
            paste0(
              "Matrix `%s` returned inconsistent CSV headers across chunked ",
              "requests, so the parts cannot be combined safely.\n  first: %s",
              "\n  later: %s"
            ),
            code,
            .tempo_header_text(first_header),
            .tempo_header_text(parts$header)
          ),
          class = "tempo_api_error",
          code = code
        )
      }
      sink(parts$body, append = TRUE)
    }

    reporter$tick()
  })

  reporter$done()
  requests
}

.tempo_download_matrix <- function(code, language, directory, metadata = NULL,
                                   progress = TRUE) {
  if (is.null(metadata)) {
    metadata <- .tempo_matrix_metadata(code, language)
  }

  plan <- .tempo_plan_query(.tempo_option_ids(metadata, code), code = code)
  destination <- file.path(directory, paste0(code, ".csv"))
  temporary <- tempfile(
    pattern = paste0(".", code, "-"),
    tmpdir = directory,
    fileext = ".csv"
  )
  completed <- FALSE
  on.exit({
    if (!completed && file.exists(temporary)) {
      unlink(temporary)
    }
  }, add = TRUE)

  label <- if (isTRUE(progress) && attr(plan, "requests") > 1L) {
    sprintf("Downloading %s", code)
  } else {
    NULL
  }

  requests <- .tempo_collect_matrix(
    code = code,
    language = language,
    metadata = metadata,
    plan = plan,
    sink = function(content, append) {
      .tempo_write_raw(content, temporary, append = append)
    },
    progress_label = label
  )

  if (!file.rename(temporary, destination)) {
    copied <- file.copy(temporary, destination, overwrite = TRUE)
    unlink(temporary)
    if (!copied) {
      .tempo_abort(
        sprintf("Could not write the CSV file for `%s` to %s.", code, destination),
        class = "tempo_io_error",
        code = code,
        path = destination
      )
    }
  }

  completed <- TRUE
  attr(destination, "requests") <- requests
  destination
}

# In-memory variant used by tempo_get().
.tempo_fetch_matrix <- function(code, language, metadata, plan,
                                progress_label = NULL) {
  pieces <- list()
  requests <- .tempo_collect_matrix(
    code = code,
    language = language,
    metadata = metadata,
    plan = plan,
    sink = function(content, append) {
      pieces[[length(pieces) + 1L]] <<- content
    },
    progress_label = progress_label
  )

  text <- rawToChar(unlist(pieces, use.names = FALSE))
  Encoding(text) <- "UTF-8"
  attr(text, "requests") <- requests
  text
}
