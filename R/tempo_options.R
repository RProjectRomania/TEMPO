# Package options -------------------------------------------------------------

.tempo_defaults <- list(
  base_url            = "http://statistici.insse.ro:8077/tempo-ins",
  connecttimeout      = 30L,
  timeout             = 300L,
  max_attempts        = 3L,
  backoff             = 1,
  throttle            = 0,
  max_connections     = 4L,
  max_cells           = 50000,
  max_options_per_dim = 300L,
  max_requests        = 2000L,
  user_agent          = NULL,
  cache_dir           = NULL,
  progress            = NULL
)

.tempo_opt <- function(name) {
  value <- getOption(paste0("TEMPO.", name))
  if (is.null(value)) .tempo_defaults[[name]] else value
}

.tempo_progress_enabled <- function() {
  value <- .tempo_opt("progress")
  if (is.null(value)) interactive() else isTRUE(value)
}

#' Options that control TEMPO
#'
#' `TEMPO` is configured with `base::options()`. Every option is named with a
#' `TEMPO.` prefix and falls back to the default shown below when unset.
#'
#' @section Connection:
#' \describe{
#'   \item{`TEMPO.base_url`}{Base URL of the TEMPO Online service. Default
#'     `"http://statistici.insse.ro:8077/tempo-ins"`. The service is only
#'     reachable over plain HTTP; it does not offer TLS.}
#'   \item{`TEMPO.connecttimeout`}{Seconds to wait for the connection.
#'     Default `30`.}
#'   \item{`TEMPO.timeout`}{Seconds to wait for a complete response.
#'     Default `300`.}
#'   \item{`TEMPO.user_agent`}{`User-Agent` header sent with every request.
#'     Default `NULL`, which sends a string identifying the package, its
#'     version and the R version.}
#' }
#'
#' @section Politeness and resilience:
#' \describe{
#'   \item{`TEMPO.max_attempts`}{How many times a failed request is attempted
#'     in total. Default `3`. Transport failures and HTTP 408, 425, 429 and
#'     5xx statuses are retried; other statuses fail immediately.}
#'   \item{`TEMPO.backoff`}{Base number of seconds for the exponential backoff
#'     between attempts. Default `1`, which waits roughly 1 then 2 seconds.
#'     A `Retry-After` response header takes precedence.}
#'   \item{`TEMPO.throttle`}{Minimum number of seconds between two consecutive
#'     requests. Default `0`.}
#'   \item{`TEMPO.max_connections`}{Maximum number of concurrent connections
#'     used when many requests are issued at once, as in
#'     [tempo_toc()] with `full_description = TRUE`. Default `4`. TEMPO Online
#'     is a small public service; please do not raise this casually.}
#' }
#'
#' @section Query size:
#' \describe{
#'   \item{`TEMPO.max_cells`}{Largest number of cells requested in a single
#'     query. Default `50000`. Larger downloads are split into several
#'     requests.}
#'   \item{`TEMPO.max_options_per_dim`}{Largest number of options selected from
#'     a single dimension in one query. Default `300`.}
#'   \item{`TEMPO.max_requests`}{Refuse to start a download that would need
#'     more than this many requests. Default `2000`.}
#' }
#'
#' @section Output:
#' \describe{
#'   \item{`TEMPO.progress`}{Whether to report progress for long operations.
#'     Default `NULL`, meaning [base::interactive()].}
#'   \item{`TEMPO.cache_dir`}{Directory used to cache service responses. See
#'     [tempo_cache_dir()].}
#' }
#'
#' @name TEMPO-options
#' @examples
#' # Be a little gentler with the service and allow bigger single queries.
#' old <- options(TEMPO.throttle = 0.2, TEMPO.max_cells = 1e5)
#' getOption("TEMPO.throttle")
#' options(old)
NULL
