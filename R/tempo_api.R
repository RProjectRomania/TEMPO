.tempo_base_url <- "http://statistici.insse.ro:8077/tempo-ins"

.tempo_abort <- function(message) {
  stop(message, call. = FALSE)
}

.tempo_validate_language <- function(language) {
  if (
    !is.character(language) ||
      length(language) != 1L ||
      is.na(language) ||
      !language %in% c("ro", "en")
  ) {
    .tempo_abort("`language` must be either \"ro\" or \"en\".")
  }

  language
}

.tempo_validate_flag <- function(value, name) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    .tempo_abort(sprintf("`%s` must be either `TRUE` or `FALSE`.", name))
  }

  value
}

.tempo_validate_codes <- function(codes) {
  if (is.list(codes)) {
    codes <- unlist(codes, recursive = TRUE, use.names = FALSE)
  }

  if (!is.character(codes) || !length(codes) || anyNA(codes)) {
    .tempo_abort("`codes` must contain at least one TEMPO matrix code.")
  }

  codes <- trimws(codes)
  if (any(!nzchar(codes)) || any(!grepl("^[[:alnum:]]+$", codes))) {
    .tempo_abort("Each element of `codes` must contain only letters and numbers.")
  }

  unique(codes)
}

.tempo_prepare_directory <- function(directory) {
  if (
    is.null(directory) ||
      !is.character(directory) ||
      length(directory) != 1L ||
      is.na(directory) ||
      !nzchar(directory)
  ) {
    .tempo_abort("`directory` must be a single path where the CSV files can be written.")
  }

  directory <- path.expand(directory)
  if (!dir.exists(directory)) {
    created <- dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    if (!created && !dir.exists(directory)) {
      .tempo_abort(sprintf("Could not create `directory`: %s", directory))
    }
  }

  if (file.access(directory, mode = 2L) != 0L) {
    .tempo_abort(sprintf("`directory` is not writable: %s", directory))
  }

  normalizePath(directory, winslash = "/", mustWork = TRUE)
}

.tempo_url <- function(path, language = NULL) {
  url <- paste0(.tempo_base_url, "/", path)
  if (!is.null(language) && identical(language, "en")) {
    url <- paste0(url, "?lang=en")
  }

  url
}

.tempo_response_text <- function(response, context) {
  content <- response$content
  if (!is.raw(content)) {
    .tempo_abort(sprintf("The TEMPO service returned an invalid %s response.", context))
  }

  tryCatch(
    rawToChar(content),
    error = function(error) {
      .tempo_abort(sprintf(
        "The TEMPO service returned unreadable %s content: %s",
        context,
        conditionMessage(error)
      ))
    }
  )
}

.tempo_request <- function(url, method = c("GET", "POST"), body = NULL) {
  method <- match.arg(method)
  handle <- curl::new_handle()
  curl::handle_setopt(handle, connecttimeout = 30L, timeout = 300L)

  if (identical(method, "POST")) {
    curl::handle_setheaders(handle, "Content-Type" = "application/json")
    curl::handle_setopt(handle, customrequest = "POST", postfields = body)
  }

  response <- tryCatch(
    curl::curl_fetch_memory(url, handle = handle),
    error = function(error) {
      .tempo_abort(sprintf(
        "Could not reach the TEMPO Online service at %s: %s",
        url,
        conditionMessage(error)
      ))
    }
  )

  status_code <- response$status_code
  if (is.null(status_code) || length(status_code) != 1L || is.na(status_code)) {
    .tempo_abort(sprintf("The TEMPO Online service returned no HTTP status for %s.", url))
  }

  if (status_code < 200L || status_code >= 300L) {
    body_text <- .tempo_response_text(response, "error")
    body_text <- gsub("[[:space:]]+", " ", body_text)
    body_text <- substr(body_text, 1L, 200L)
    .tempo_abort(sprintf(
      "The TEMPO Online service returned HTTP %s for %s%s",
      status_code,
      url,
      if (nzchar(body_text)) paste0(": ", body_text) else "."
    ))
  }

  response
}

.tempo_parse_json <- function(response, context, simplify_vector = FALSE) {
  text <- .tempo_response_text(response, context)

  tryCatch(
    jsonlite::fromJSON(text, simplifyVector = simplify_vector),
    error = function(error) {
      .tempo_abort(sprintf(
        "The TEMPO Online service returned invalid %s JSON: %s",
        context,
        conditionMessage(error)
      ))
    }
  )
}

.tempo_toc_from_response <- function(response) {
  toc <- .tempo_parse_json(response, "table-of-contents", simplify_vector = TRUE)
  if (!is.data.frame(toc) || !all(c("name", "code") %in% names(toc))) {
    .tempo_abort("The TEMPO Online service returned an unexpected table-of-contents format.")
  }

  toc <- toc[, c("name", "code"), drop = FALSE]
  toc$name <- as.character(toc$name)
  toc$code <- as.character(toc$code)
  rownames(toc) <- NULL
  toc
}

.tempo_matrix_metadata <- function(code, language) {
  response <- .tempo_request(
    .tempo_url(paste0("matrix/", code), language = language)
  )
  metadata <- .tempo_parse_json(response, sprintf("metadata for `%s`", code))

  if (!is.list(metadata) || is.null(metadata$dimensionsMap) || is.null(metadata$details)) {
    .tempo_abort(sprintf(
      "The TEMPO Online service returned incomplete metadata for `%s`.",
      code
    ))
  }

  metadata
}

.tempo_metadata_date <- function(metadata) {
  date <- metadata$ultimaActualizare
  if (!is.character(date) || length(date) != 1L || is.na(date)) {
    return(as.Date(NA))
  }

  as.Date(date, format = "%d-%m-%Y")
}

.tempo_option_ids <- function(metadata, code) {
  dimensions <- metadata$dimensionsMap
  if (!is.list(dimensions) || !length(dimensions)) {
    .tempo_abort(sprintf("Matrix `%s` has no dimensions to download.", code))
  }

  lapply(seq_along(dimensions), function(index) {
    options <- dimensions[[index]]$options
    if (!is.list(options) || !length(options)) {
      .tempo_abort(sprintf(
        "Matrix `%s` has no selectable options in dimension %s.",
        code,
        index
      ))
    }

    ids <- vapply(options, function(option) {
      value <- option$nomItemId
      if (is.null(value) || length(value) != 1L || is.na(value)) {
        return(NA_character_)
      }

      as.character(value)
    }, character(1))

    if (anyNA(ids) || any(!nzchar(ids))) {
      .tempo_abort(sprintf(
        "Matrix `%s` contains an invalid option identifier in dimension %s.",
        code,
        index
      ))
    }

    ids
  })
}

.tempo_split_options <- function(option_ids, threshold = 300L, size = 100L) {
  if (length(option_ids) <= threshold) {
    return(list(option_ids))
  }

  unname(split(option_ids, ceiling(seq_along(option_ids) / size)))
}

.tempo_each_selection <- function(option_chunks, callback) {
  positions <- rep.int(1L, length(option_chunks))

  repeat {
    selection <- Map(function(chunks, position) chunks[[position]], option_chunks, positions)
    callback(selection)

    position <- length(positions)
    while (position > 0L && positions[[position]] == length(option_chunks[[position]])) {
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
    .tempo_abort(sprintf("Matrix `%s` has incomplete query metadata.", code))
  }

  list(
    language = language,
    encQuery = paste(vapply(option_ids, paste, collapse = ", ", character(1)), collapse = ":"),
    matCode = code,
    matMaxDim = details$matMaxDim,
    matUMSpec = details$matUMSpec
  )
}

.tempo_without_csv_header <- function(content) {
  newline <- match(as.raw(10L), content)
  if (is.na(newline)) {
    .tempo_abort("The TEMPO Online service returned a CSV response without a header line.")
  }

  if (newline == length(content)) {
    return(raw())
  }

  content[seq.int(newline + 1L, length(content))]
}

.tempo_write_raw <- function(content, path, append = FALSE) {
  connection <- file(path, open = if (append) "ab" else "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(content, connection, useBytes = TRUE)
  invisible(path)
}

.tempo_download_matrix <- function(code, language, directory, metadata = NULL) {
  if (is.null(metadata)) {
    metadata <- .tempo_matrix_metadata(code, language)
  }

  option_chunks <- lapply(.tempo_option_ids(metadata, code), .tempo_split_options)
  destination <- file.path(directory, paste0(code, ".csv"))
  temporary <- tempfile(pattern = paste0(".", code, "-"), tmpdir = directory, fileext = ".csv")
  completed <- FALSE
  on.exit({
    if (!completed && file.exists(temporary)) {
      unlink(temporary)
    }
  }, add = TRUE)

  first_response <- TRUE
  .tempo_each_selection(option_chunks, function(selection) {
    payload <- .tempo_payload(code, language, metadata, selection)
    response <- .tempo_request(
      .tempo_url("pivot"),
      method = "POST",
      body = jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
    )
    content <- response$content
    if (!is.raw(content) || !length(content)) {
      .tempo_abort(sprintf("The TEMPO Online service returned an empty CSV for `%s`.", code))
    }

    if (!first_response) {
      content <- .tempo_without_csv_header(content)
    }
    .tempo_write_raw(content, temporary, append = !first_response)
    first_response <<- FALSE
  })

  if (!file.rename(temporary, destination)) {
    copied <- file.copy(temporary, destination, overwrite = TRUE)
    unlink(temporary)
    if (!copied) {
      .tempo_abort(sprintf("Could not write the CSV file for `%s` to %s.", code, destination))
    }
  }

  completed <- TRUE
  destination
}

.tempo_file_is_current <- function(path, update_date) {
  if (!file.exists(path) || is.na(update_date)) {
    return(FALSE)
  }

  modified <- as.Date(file.info(path)$mtime)
  !is.na(modified) && modified >= update_date
}

.tempo_ancestor_name <- function(metadata, position) {
  ancestors <- metadata$ancestors
  if (!is.list(ancestors) || length(ancestors) < position) {
    return(NA_character_)
  }

  name <- ancestors[[position]]$name
  if (!is.character(name) || length(name) != 1L) {
    return(NA_character_)
  }

  name
}
