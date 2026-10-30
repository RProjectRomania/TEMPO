# HTTP layer ------------------------------------------------------------------
#
# All traffic to TEMPO Online goes through this file.  It adds, relative to a
# bare curl::curl_fetch_memory() call:
#
#   * a User-Agent identifying the package, so that INS can see who is calling;
#   * transparent gzip, which matters for the large CSV payloads;
#   * retries with exponential backoff for transport failures and for the
#     statuses that are worth retrying, honouring Retry-After when present;
#   * an optional throttle between consecutive requests;
#   * a concurrent variant with a connection cap, used for request fan-out.

.tempo_state <- new.env(parent = emptyenv())
.tempo_state$last_request <- NULL

.tempo_user_agent <- function() {
  configured <- .tempo_opt("user_agent")
  if (!is.null(configured)) {
    return(as.character(configured)[[1L]])
  }

  version <- tryCatch(
    as.character(utils::packageVersion("TEMPO")),
    error = function(error) "unknown"
  )

  sprintf(
    "TEMPO/%s (R %s.%s; https://github.com/RProjectRomania/TEMPO)",
    version,
    R.version$major,
    R.version$minor
  )
}

.tempo_handle <- function(method = "GET", body = NULL) {
  handle <- curl::new_handle()
  curl::handle_setopt(
    handle,
    connecttimeout = as.integer(.tempo_opt("connecttimeout")),
    timeout = as.integer(.tempo_opt("timeout")),
    useragent = .tempo_user_agent(),
    accept_encoding = "gzip, deflate"
  )

  if (identical(method, "POST")) {
    curl::handle_setheaders(handle, "Content-Type" = "application/json")
    curl::handle_setopt(handle, customrequest = "POST", postfields = body)
  }

  handle
}

# Statuses worth another attempt: request timeout, too early, rate limited and
# anything the server blames on itself.
.tempo_status_retryable <- function(status_code) {
  !is.null(status_code) &&
    length(status_code) == 1L &&
    !is.na(status_code) &&
    (status_code %in% c(408L, 425L, 429L) || status_code >= 500L)
}

.tempo_retry_after <- function(response) {
  headers <- tryCatch(
    curl::parse_headers_list(response$headers),
    error = function(error) NULL
  )
  if (is.null(headers)) {
    return(NA_real_)
  }

  value <- headers[["retry-after"]]
  if (is.null(value) || !length(value)) {
    return(NA_real_)
  }

  seconds <- suppressWarnings(as.numeric(value[[1L]]))
  if (is.na(seconds) || seconds < 0) NA_real_ else min(seconds, 60)
}

.tempo_backoff_delay <- function(attempt, retry_after = NA_real_) {
  if (!is.na(retry_after)) {
    return(retry_after)
  }

  base <- as.numeric(.tempo_opt("backoff"))
  if (!is.finite(base) || base <= 0) {
    return(0)
  }

  # Exponential with a little jitter, so that concurrent callers do not line up.
  base * (2^(attempt - 1L)) * stats::runif(1L, 0.5, 1)
}

.tempo_throttle <- function() {
  wait <- as.numeric(.tempo_opt("throttle"))
  if (!is.finite(wait) || wait <= 0) {
    return(invisible(NULL))
  }

  last <- .tempo_state$last_request
  if (!is.null(last)) {
    elapsed <- as.numeric(Sys.time() - last, units = "secs")
    if (elapsed < wait) {
      Sys.sleep(wait - elapsed)
    }
  }

  .tempo_state$last_request <- Sys.time()
  invisible(NULL)
}

.tempo_response_text <- function(response, context) {
  content <- response$content
  if (!is.raw(content)) {
    .tempo_abort(
      sprintf("The TEMPO service returned an invalid %s response.", context),
      class = "tempo_api_error"
    )
  }

  text <- tryCatch(
    rawToChar(content),
    error = function(error) {
      .tempo_abort(
        sprintf(
          "The TEMPO service returned unreadable %s content: %s",
          context,
          conditionMessage(error)
        ),
        class = "tempo_api_error"
      )
    }
  )

  # The service declares UTF-8; mark it so that downstream parsing and
  # comparisons behave the same on every platform.
  Encoding(text) <- "UTF-8"
  text
}

.tempo_http_error <- function(response, url) {
  body_text <- tryCatch(
    .tempo_response_text(response, "error"),
    error = function(error) ""
  )
  body_text <- gsub("[[:space:]]+", " ", body_text)
  body_text <- trimws(substr(body_text, 1L, 200L))

  .tempo_abort(
    sprintf(
      "The TEMPO Online service returned HTTP %s for %s%s",
      response$status_code,
      url,
      if (nzchar(body_text)) paste0(": ", body_text) else "."
    ),
    class = "tempo_http_error",
    status_code = as.integer(response$status_code),
    url = url,
    body = body_text
  )
}

#' @param url Fully qualified request URL.
#' @param method Either `"GET"` or `"POST"`.
#' @param body Request body for `POST`, as a JSON string.
#' @noRd
.tempo_request <- function(url, method = c("GET", "POST"), body = NULL) {
  method <- match.arg(method)
  attempts <- max(1L, as.integer(.tempo_opt("max_attempts")))
  last_condition <- NULL

  for (attempt in seq_len(attempts)) {
    .tempo_throttle()

    response <- tryCatch(
      curl::curl_fetch_memory(url, handle = .tempo_handle(method, body)),
      error = function(error) error
    )

    if (inherits(response, "error")) {
      last_condition <- .tempo_condition(
        sprintf(
          "Could not reach the TEMPO Online service at %s: %s",
          url,
          conditionMessage(response)
        ),
        class = "tempo_offline_error",
        url = url
      )
      if (attempt < attempts) {
        Sys.sleep(.tempo_backoff_delay(attempt))
        next
      }
      stop(last_condition)
    }

    status_code <- response$status_code
    if (is.null(status_code) || length(status_code) != 1L || is.na(status_code)) {
      .tempo_abort(
        sprintf("The TEMPO Online service returned no HTTP status for %s.", url),
        class = "tempo_api_error",
        url = url
      )
    }

    if (status_code >= 200L && status_code < 300L) {
      return(response)
    }

    if (.tempo_status_retryable(status_code) && attempt < attempts) {
      Sys.sleep(.tempo_backoff_delay(attempt, .tempo_retry_after(response)))
      next
    }

    .tempo_http_error(response, url)
  }

  # Only reachable if the loop fell through without returning or signalling.
  stop(last_condition %||% .tempo_condition(
    sprintf("The request to %s failed.", url),
    class = "tempo_offline_error",
    url = url
  ))
}

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Fetch several URLs concurrently
#'
#' Returns a list of responses in the same order as `urls`. Failures are
#' retried in later rounds; a URL that never succeeds raises the condition it
#' last produced.
#'
#' @noRd
.tempo_request_many <- function(urls, progress_label = NULL) {
  if (!length(urls)) {
    return(list())
  }

  connections <- max(1L, as.integer(.tempo_opt("max_connections")))
  if (length(urls) == 1L || connections == 1L) {
    responses <- vector("list", length(urls))
    reporter <- .tempo_reporter(length(urls), progress_label)
    for (index in seq_along(urls)) {
      responses[[index]] <- .tempo_request(urls[[index]])
      reporter$tick()
    }
    reporter$done()
    return(responses)
  }

  attempts <- max(1L, as.integer(.tempo_opt("max_attempts")))
  responses <- vector("list", length(urls))
  failures <- vector("list", length(urls))
  pending <- seq_along(urls)
  reporter <- .tempo_reporter(length(urls), progress_label)

  for (attempt in seq_len(attempts)) {
    if (!length(pending)) {
      break
    }

    pool <- curl::new_pool(
      total_con = connections,
      host_con = connections,
      multiplex = FALSE
    )
    retry <- integer()

    for (index in pending) {
      local({
        position <- index
        url <- urls[[position]]
        curl::curl_fetch_multi(
          url,
          done = function(response) {
            status_code <- response$status_code
            if (!is.null(status_code) &&
                length(status_code) == 1L &&
                !is.na(status_code) &&
                status_code >= 200L &&
                status_code < 300L) {
              responses[[position]] <<- response
              reporter$tick()
            } else if (.tempo_status_retryable(status_code)) {
              retry <<- c(retry, position)
            } else {
              failures[[position]] <<- tryCatch(
                .tempo_http_error(response, url),
                tempo_error = function(condition) condition
              )
            }
          },
          fail = function(message) {
            retry <<- c(retry, position)
            failures[[position]] <<- .tempo_condition(
              sprintf(
                "Could not reach the TEMPO Online service at %s: %s",
                url,
                message
              ),
              class = "tempo_offline_error",
              url = url
            )
          },
          pool = pool,
          handle = .tempo_handle("GET")
        )
      })
    }

    curl::multi_run(pool = pool)
    pending <- sort(unique(retry))

    if (length(pending) && attempt < attempts) {
      Sys.sleep(.tempo_backoff_delay(attempt))
    }
  }

  reporter$done()

  unresolved <- which(vapply(responses, is.null, logical(1)))
  if (length(unresolved)) {
    condition <- failures[[unresolved[[1L]]]]
    if (is.null(condition)) {
      condition <- .tempo_condition(
        sprintf("Could not fetch %s.", urls[[unresolved[[1L]]]]),
        class = "tempo_offline_error",
        url = urls[[unresolved[[1L]]]]
      )
    }
    stop(condition)
  }

  responses
}

# Minimal progress reporting, deliberately dependency-free.
.tempo_reporter <- function(total, label = NULL) {
  active <- .tempo_progress_enabled() && !is.null(label) && total > 1L
  count <- 0L
  started <- Sys.time()

  list(
    tick = function(n = 1L) {
      count <<- count + n
      if (!active) {
        return(invisible(NULL))
      }
      message(
        sprintf("\r%s: %d/%d", label, count, total),
        appendLF = FALSE
      )
      invisible(NULL)
    },
    done = function() {
      if (!active) {
        return(invisible(NULL))
      }
      message(sprintf(
        "\r%s: %d/%d (%.1fs)",
        label,
        count,
        total,
        as.numeric(Sys.time() - started, units = "secs")
      ))
      invisible(NULL)
    }
  )
}
