# Classed conditions ----------------------------------------------------------
#
# Every error raised by TEMPO carries a class so that callers can react to the
# *kind* of failure instead of matching on English message text.  The hierarchy
# is
#
#   tempo_error
#     +- tempo_input_error    invalid user input, raised before any request
#     +- tempo_offline_error  the service could not be reached at all
#     +- tempo_http_error     the service answered with a non-2xx status
#     +- tempo_api_error      the service answered, but not with what we expect
#     +- tempo_io_error       a local file could not be written
#
# and all of them also inherit from "error" and "condition", so plain
# tryCatch(expr, error = ...) keeps working.

.tempo_condition <- function(message, class, ...) {
  structure(
    class = c(class, "tempo_error", "error", "condition"),
    c(list(message = message, call = NULL), list(...))
  )
}

.tempo_abort <- function(message, class = "tempo_api_error", ...) {
  stop(.tempo_condition(message, class = class, ...))
}

.tempo_warn <- function(message, class = "tempo_warning", ...) {
  warning(
    structure(
      class = c(class, "tempo_warning", "warning", "condition"),
      c(list(message = message, call = NULL), list(...))
    )
  )
}

# Registry used to emit a given warning only once per session, so that a loop
# over hundreds of matrices does not produce hundreds of identical warnings.
.tempo_warned <- new.env(parent = emptyenv())

.tempo_warn_once <- function(key, message, class = "tempo_warning", ...) {
  if (isTRUE(.tempo_warned[[key]])) {
    return(invisible(FALSE))
  }

  assign(key, TRUE, envir = .tempo_warned)
  .tempo_warn(message, class = class, ...)
  invisible(TRUE)
}

.tempo_reset_warnings <- function() {
  rm(list = ls(envir = .tempo_warned), envir = .tempo_warned)
  invisible(NULL)
}
