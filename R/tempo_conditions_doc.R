#' Errors and warnings raised by TEMPO
#'
#' Every error raised by this package carries an S3 class, so a caller can
#' react to the *kind* of failure without matching on English message text.
#' That matters for unattended work: an unreachable service is worth retrying
#' later, whereas a rejected matrix code never will be.
#'
#' @section Error classes:
#' All of them inherit from `tempo_error`, and from `error` and `condition`, so
#' an ordinary `tryCatch(expr, error = ...)` still catches everything.
#' \describe{
#'   \item{`tempo_input_error`}{Invalid arguments, detected before any request
#'     is made. Carries no extra fields.}
#'   \item{`tempo_offline_error`}{The service could not be reached at all, after
#'     exhausting `getOption("TEMPO.max_attempts")`. Field: `url`.}
#'   \item{`tempo_http_error`}{The service answered with a non-2xx status.
#'     Fields: `status_code`, `url`, `body` (the first 200 characters of the
#'     response).}
#'   \item{`tempo_api_error`}{The service answered, but not with the structure
#'     the package expects: malformed JSON, a missing field, inconsistent CSV
#'     headers across chunked requests. Field: `code`, when a matrix is
#'     involved.}
#'   \item{`tempo_io_error`}{A local path could not be created, written or
#'     read. Field: `path`.}
#' }
#'
#' @section Warning classes:
#' Warnings inherit from `tempo_warning` and are raised at most once per
#' session each, so a loop over many matrices does not bury the console.
#' \describe{
#'   \item{`tempo_date_warning`}{The service reported an update date in a format
#'     this package cannot parse. `Last_update` becomes `NA`; freshness checks
#'     fall back to comparing the service's raw update string, so caching keeps
#'     working.}
#'   \item{`tempo_cache_warning`}{A CSV file is being reused although its
#'     language cannot be verified, because it predates the download manifest.}
#'   \item{`tempo_tidy_warning`}{[tempo_tidy()] met something it could not
#'     interpret, such as an observation that is not a number.}
#' }
#'
#' @name TEMPO-conditions
#'
#' @examples
#' # Distinguish a bad argument from a service problem.
#' result <- tryCatch(
#'   tempo_toc(language = "fr"),
#'   tempo_input_error = function(condition) "fix the call",
#'   tempo_offline_error = function(condition) "retry later",
#'   tempo_error = function(condition) "something else"
#' )
#' result
#'
#' \dontrun{
#' # Retry only what is worth retrying, and read the status code.
#' tryCatch(
#'   tempo_get("ACC101B"),
#'   tempo_http_error = function(condition) {
#'     message("HTTP ", condition$status_code, " from ", condition$url)
#'     NULL
#'   },
#'   tempo_offline_error = function(condition) {
#'     Sys.sleep(60)
#'     tempo_get("ACC101B")
#'   }
#' )
#' }
NULL
