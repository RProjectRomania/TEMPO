# Reading the service's CSV ---------------------------------------------------

.tempo_read_csv_text <- function(text) {
  utils::read.csv(
    text = as.character(text),
    sep = ",",
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character",
    na.strings = character(),
    encoding = "UTF-8"
  )
}

.tempo_read_csv_file <- function(path) {
  utils::read.csv(
    path,
    sep = ",",
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character",
    na.strings = character(),
    encoding = "UTF-8"
  )
}

# Period labels ---------------------------------------------------------------
#
# The service labels time values in words: "Anul 2020", "luna ianuarie",
# "Trimestrul III".  Parsing is best-effort and deliberately matches on the
# month or ordinal itself rather than on an exact label format, because the
# label layout is not contractual.

.tempo_month_lookup <- c(
  ianuarie = 1L, februarie = 2L, martie = 3L, aprilie = 4L, mai = 5L,
  iunie = 6L, iulie = 7L, august = 8L, septembrie = 9L, octombrie = 10L,
  noiembrie = 11L, decembrie = 12L,
  january = 1L, february = 2L, march = 3L, april = 4L, may = 5L,
  june = 6L, july = 7L, september = 9L, october = 10L,
  november = 11L, december = 12L
)

.tempo_roman_lookup <- c(
  i = 1L, ii = 2L, iii = 3L, iv = 4L, v = 5L, vi = 6L,
  vii = 7L, viii = 8L, ix = 9L, x = 10L, xi = 11L, xii = 12L
)

.tempo_parse_year <- function(x) {
  value <- tolower(trimws(as.character(x)))
  stripped <- sub("^(anul|ani|year|years)[[:space:]]*", "", value)
  direct <- suppressWarnings(as.integer(stripped))

  embedded <- rep(NA_integer_, length(value))
  match <- regexpr("(18|19|20|21)[0-9]{2}", value)
  found <- match > 0L
  if (any(found)) {
    embedded[found] <- as.integer(regmatches(value, match))
  }

  ifelse(!is.na(direct), direct, embedded)
}

.tempo_parse_month <- function(x) {
  value <- tolower(trimws(as.character(x)))
  out <- rep(NA_integer_, length(value))

  for (name in names(.tempo_month_lookup)) {
    pending <- is.na(out) & nzchar(value)
    if (!any(pending)) {
      break
    }
    hit <- pending & grepl(
      sprintf("(^|[^[:alpha:]])%s([^[:alpha:]]|$)", name),
      value
    )
    out[hit] <- .tempo_month_lookup[[name]]
  }

  out
}

.tempo_parse_ordinal <- function(x, keywords, maximum) {
  value <- tolower(trimws(as.character(x)))
  pattern <- sprintf(
    "(?:^|[^[:alpha:]])(?:%s)[^[:alnum:]]*([ivx]+|[0-9]+)",
    paste(keywords, collapse = "|")
  )

  out <- rep(NA_integer_, length(value))
  match <- regexpr(pattern, value, perl = TRUE)
  found <- match > 0L
  if (!any(found)) {
    return(out)
  }

  token <- sub(pattern, "\\1", regmatches(value, match), perl = TRUE)
  parsed <- ifelse(
    grepl("^[0-9]+$", token),
    suppressWarnings(as.integer(token)),
    unname(.tempo_roman_lookup[token])
  )
  parsed[is.na(parsed) | parsed < 1L | parsed > maximum] <- NA_integer_
  out[found] <- parsed
  out
}

.tempo_parse_quarter <- function(x) {
  .tempo_parse_ordinal(
    x,
    c("trimestr[a-z]*", "trim\\.?", "quarters?", "q"),
    4L
  )
}

.tempo_parse_semester <- function(x) {
  .tempo_parse_ordinal(
    x,
    c("semestr[a-z]*", "sem\\.?", "semesters?"),
    2L
  )
}

# Classify a column as a time column, by name first and then by content.
#
# TEMPO uses two time layouts.  Some matrices carry a dedicated "Ani"/"Years"
# dimension, optionally alongside "Luni"/"Months".  Others carry a single
# "Perioade"/"Periods" dimension whose labels mix frequencies, as in
# "Year 2010", "January 2010", "February 2010".  The second layout must not be
# treated as a year column: reducing every label to its year would make an
# annual total and a January value indistinguishable.
.tempo_time_role <- function(name, values) {
  normalised <- tolower(trimws(name))
  if (grepl("^(ani|anul|anii|years?|year)$", normalised)) {
    return("year")
  }
  if (grepl("^(luni|luna|lunile|months?|month)$", normalised)) {
    return("month")
  }
  if (grepl("trimestr|quarter", normalised)) {
    return("quarter")
  }
  if (grepl("semestr|semester", normalised)) {
    return("semester")
  }
  if (grepl("^(perioade|perioada|periods?|period)$", normalised)) {
    return("period")
  }

  distinct <- unique(trimws(values))
  distinct <- distinct[nzchar(distinct) & !is.na(distinct)]
  if (!length(distinct)) {
    return(NA_character_)
  }

  months <- .tempo_parse_month(distinct)
  quarters <- .tempo_parse_quarter(distinct)
  years <- .tempo_parse_year(distinct)

  if (length(distinct) <= 12L && !anyNA(months)) {
    return("month")
  }
  if (length(distinct) <= 4L && !anyNA(quarters)) {
    return("quarter")
  }
  # A year together with a month or quarter in the same label.
  if (!anyNA(years) && (any(!is.na(months)) || any(!is.na(quarters)))) {
    return("period")
  }
  if (!anyNA(years) &&
      all(grepl("(^|[^0-9])(18|19|20|21)[0-9]{2}([^0-9]|$)", distinct))) {
    return("year")
  }

  NA_character_
}

#' Turn a downloaded TEMPO table into analysis-ready data
#'
#' The CSV that TEMPO Online returns is not directly usable: the observation
#' column is text, the unit of measure hides in a column *header*, years read
#' `"Anul 2020"` rather than `2020`, and missing observations are marked `":"`.
#' `tempo_tidy()` resolves all of that and leaves one row per observation.
#'
#' @param x A data frame as returned by [tempo_get()] with `tidy = FALSE`, the
#'   path of a CSV file written by [tempo_bulk()], or a single string holding
#'   CSV text.
#' @param code Optional matrix code, recorded on the result as the
#'   `tempo_code` attribute.
#' @param periods A single logical value. If `TRUE` (the default), parse the
#'   time dimensions. See Time layouts below. Default `TRUE`.
#' @param na Character vector of strings that mark a missing observation.
#'   Defaults to `c(":", "")`, the service's own markers.
#' @param drop_notes A single logical value. If `TRUE` (the default), drop rows
#'   in which every dimension column is empty. Those are the footnote and
#'   copyright lines the service appends after the data; rows with a genuine
#'   missing observation are kept, because their dimension columns are filled.
#' @param clean_names A single logical value. If `TRUE`, pass the column names
#'   through [base::make.names()] so they can be used unquoted. Default
#'   `FALSE`, which keeps the service's own names.
#' @param value_name,unit_name Names for the observation and unit-of-measure
#'   columns. Default `"value"` and `"unit"`.
#'
#' @return A data frame with one row per observation: the dimension columns as
#'   received, `unit` holding the unit of measure, `value` as a numeric vector,
#'   and `period` when a sub-annual period could be reconstructed. The
#'   attributes `tempo_code` and `tempo_unit` are set when known.
#'
#' @section Time layouts:
#' TEMPO uses two, and they are handled differently.
#'
#' Some matrices carry a dedicated `Ani`/`Years` dimension, optionally
#' alongside `Luni`/`Months`, `Trimestre` or `Semestre`. The year column
#' becomes an integer, the sub-annual labels are left readable, and a `period`
#' column of class `Date` is added when the two can be combined.
#'
#' Others carry a single `Perioade`/`Periods` dimension whose labels mix
#' frequencies, as in `"Year 2010"`, `"January 2010"`, `"February 2010"`. Here
#' the label is left exactly as received, because it is the only thing
#' distinguishing an annual total from a monthly figure, and two columns are
#' added instead: `year`, and `period` holding the first day of the month,
#' quarter or semester named. `period` is `NA` for an annual row, so
#' `subset(x, !is.na(period))` selects the sub-annual rows and
#' `subset(x, is.na(period))` the annual ones.
#'
#' @section What changed since 0.1.0:
#' Version 0.1.0 shipped no cleaning at all; every caller rewrote these rules.
#' Earlier versions had a `tempo_clean()` which did part of the work but
#' assigned its result into the global environment as a side effect, so it
#' could not be used inside a pipeline. `tempo_tidy()` is a plain function of
#' its input: it returns a value and modifies nothing.
#'
#' @section Unparsed observations:
#' Values that are neither numeric nor listed in `na` are reported once with a
#' warning, and the original text is kept in a `value_raw` column so nothing is
#' lost. This is how an unexpected flag or thousands separator becomes visible
#' instead of turning quietly into `NA`.
#'
#' @seealso [tempo_get()], which applies this by default, and [tempo_bulk()].
#'
#' @examples
#' # The structure the service returns, including a footer line and a
#' # missing observation marked ":".
#' csv <- paste(
#'   "Judete,Ani,UM: Numar persoane,Valoare",
#'   "Cluj,Anul 2019,Numar persoane,1234",
#'   "Cluj,Anul 2020,Numar persoane,:",
#'   "Bihor,Anul 2019,Numar persoane,987",
#'   ",,,(c) INS 1998-2026",
#'   sep = "\n"
#' )
#'
#' tidy <- tempo_tidy(csv, code = "DEMO101A")
#' tidy
#' str(tidy)
#'
#' # A combined Periods dimension: the annual total stays distinguishable
#' # from the monthly figures, which share its year.
#' mixed <- paste(
#'   "Counties,Periods,UM: Persons,Value",
#'   "Cluj,Year 2010,Number of persons,100",
#'   "Cluj,January 2010,Number of persons,8",
#'   "Cluj,February 2010,Number of persons,9",
#'   sep = "\n"
#' )
#' tempo_tidy(mixed)
#' subset(tempo_tidy(mixed), is.na(period))   # the annual row
#'
#' # Monthly labels gain a Date column.
#' monthly <- paste(
#'   "Luni,Ani,UM: Numar,Valoare",
#'   "luna ianuarie,Anul 2024,Numar,10",
#'   "luna februarie,Anul 2024,Numar,12",
#'   sep = "\n"
#' )
#' tempo_tidy(monthly)
#'
#' \dontrun{
#' # From a file written by tempo_bulk().
#' files <- tempo_bulk("ACC101B", language = "en",
#'                     directory = file.path(tempdir(), "tempo-data"))
#' head(tempo_tidy(files[["ACC101B"]], code = "ACC101B"))
#' }
#' @export
tempo_tidy <- function(x,
                       code = NULL,
                       periods = TRUE,
                       na = c(":", ""),
                       drop_notes = TRUE,
                       clean_names = FALSE,
                       value_name = "value",
                       unit_name = "unit") {
  periods <- .tempo_validate_flag(periods, "periods")
  drop_notes <- .tempo_validate_flag(drop_notes, "drop_notes")
  clean_names <- .tempo_validate_flag(clean_names, "clean_names")

  data <- .tempo_as_table(x)
  if (!ncol(data)) {
    .tempo_abort(
      "`x` holds no columns to tidy.",
      class = "tempo_input_error"
    )
  }

  names(data) <- trimws(names(data))

  # Unit of measure ---------------------------------------------------------
  unit_column <- grep("^(um|mu)[[:space:]]*:", tolower(names(data)))
  unit <- NA_character_
  unit_values <- NULL

  if (length(unit_column)) {
    unit_column <- unit_column[[1L]]
    unit <- trimws(sub(
      "^(um|mu)[[:space:]]*:[[:space:]]*", "",
      names(data)[[unit_column]],
      ignore.case = TRUE
    ))
    if (!nzchar(unit)) {
      unit <- NA_character_
    }

    column_values <- trimws(as.character(data[[unit_column]]))
    unit_values <- if (any(nzchar(column_values))) {
      column_values[!nzchar(column_values)] <- NA_character_
      column_values
    } else {
      rep(unit, nrow(data))
    }

    data <- data[, -unit_column, drop = FALSE]
  }

  # Observation column ------------------------------------------------------
  value_column <- which(tolower(names(data)) %in% c("valoare", "value"))
  value_column <- if (length(value_column)) value_column[[1L]] else NA_integer_

  if (is.na(value_column)) {
    .tempo_warn_once(
      "tidy-no-value-column",
      paste0(
        "No \"Valoare\"/\"Value\" column found, so no numeric observation ",
        "column was produced. Pass tidy = FALSE to keep the service layout, ",
        "and please report the table at ",
        "https://github.com/RProjectRomania/TEMPO/issues"
      ),
      class = "tempo_tidy_warning"
    )
  }

  dimension_columns <- setdiff(seq_along(data), value_column)

  # Footer rows -------------------------------------------------------------
  if (drop_notes && nrow(data) && length(dimension_columns)) {
    filled <- Reduce(`|`, lapply(dimension_columns, function(index) {
      column <- trimws(as.character(data[[index]]))
      !is.na(column) & nzchar(column)
    }))
    data <- data[filled, , drop = FALSE]
    if (!is.null(unit_values)) {
      unit_values <- unit_values[filled]
    }
  }

  rownames(data) <- NULL

  # Periods -----------------------------------------------------------------
  period <- NULL
  derived_year <- NULL
  if (periods && nrow(data)) {
    roles <- vapply(
      dimension_columns,
      function(index) .tempo_time_role(names(data)[[index]], data[[index]]),
      character(1)
    )

    year <- NULL
    sub_annual <- NULL
    combined <- NULL

    for (position in seq_along(dimension_columns)) {
      index <- dimension_columns[[position]]
      role <- roles[[position]]
      if (is.na(role)) {
        next
      }

      if (identical(role, "year") && is.null(year)) {
        parsed <- .tempo_parse_year(data[[index]])
        if (!all(is.na(parsed))) {
          year <- parsed
          # Replace the label with the integer year, as the service's
          # "Anul 2020" is never what a caller wants to compute with.  Safe
          # here because a year column carries nothing else.
          if (!anyNA(parsed)) {
            data[[index]] <- parsed
          }
        }
      } else if (identical(role, "month") && is.null(sub_annual)) {
        parsed <- .tempo_parse_month(data[[index]])
        if (!all(is.na(parsed))) {
          sub_annual <- list(value = parsed, step = 1L)
        }
      } else if (identical(role, "quarter") && is.null(sub_annual)) {
        parsed <- .tempo_parse_quarter(data[[index]])
        if (!all(is.na(parsed))) {
          sub_annual <- list(value = parsed, step = 3L)
        }
      } else if (identical(role, "semester") && is.null(sub_annual)) {
        parsed <- .tempo_parse_semester(data[[index]])
        if (!all(is.na(parsed))) {
          sub_annual <- list(value = parsed, step = 6L)
        }
      } else if (identical(role, "period") && is.null(combined)) {
        label <- data[[index]]
        parsed_year <- .tempo_parse_year(label)
        if (!all(is.na(parsed_year))) {
          # The label is left exactly as received: it is the only thing that
          # distinguishes an annual figure from a monthly one.
          combined <- list(
            year = parsed_year,
            month = .tempo_parse_month(label),
            quarter = .tempo_parse_quarter(label),
            semester = .tempo_parse_semester(label)
          )
        }
      }
    }

    if (!is.null(combined)) {
      month <- ifelse(
        !is.na(combined$month),
        combined$month,
        ifelse(
          !is.na(combined$quarter),
          (combined$quarter - 1L) * 3L + 1L,
          ifelse(
            !is.na(combined$semester),
            (combined$semester - 1L) * 6L + 1L,
            NA_integer_
          )
        )
      )
      # NA where the label names no sub-annual period, which is how an annual
      # row stays distinguishable from a monthly one.
      period <- as.Date(ifelse(
        is.na(combined$year) | is.na(month),
        NA_character_,
        sprintf("%04d-%02d-01", combined$year, month)
      ))
      derived_year <- combined$year
    } else if (!is.null(year) && !is.null(sub_annual)) {
      month <- (sub_annual$value - 1L) * sub_annual$step + 1L
      period <- as.Date(ifelse(
        is.na(year) | is.na(month),
        NA_character_,
        sprintf("%04d-%02d-01", year, month)
      ))
    }
  }

  # Assemble ----------------------------------------------------------------
  if (!is.null(unit_values)) {
    data[[unit_name]] <- unit_values
  }

  if (!is.na(value_column)) {
    raw <- trimws(as.character(data[[names(data)[[value_column]]]]))
    cleaned <- gsub("[[:space:]]", "", raw)
    # Non-breaking spaces are used as thousands separators.  Matched
    # literally: a non-ASCII character inside a bracket expression is
    # parsed differently depending on the locale.
    cleaned <- gsub("\u00a0", "", cleaned, fixed = TRUE)
    cleaned[raw %in% na | is.na(raw)] <- NA_character_

    numeric_value <- suppressWarnings(as.numeric(cleaned))
    unexpected <- is.na(numeric_value) & !is.na(cleaned) & nzchar(cleaned)

    data[[names(data)[[value_column]]]] <- NULL
    data[[value_name]] <- numeric_value

    if (any(unexpected)) {
      examples <- utils::head(unique(raw[unexpected]), 5L)
      .tempo_warn_once(
        "tidy-unparsed-values",
        sprintf(
          paste0(
            "%d observation(s) could not be read as numbers and became NA; ",
            "the original text is kept in `value_raw`. Examples: %s."
          ),
          sum(unexpected),
          paste(sprintf("\"%s\"", examples), collapse = ", ")
        ),
        class = "tempo_tidy_warning"
      )
      data$value_raw <- raw
    }
  }

  if (!is.null(derived_year) && !"year" %in% names(data)) {
    data$year <- derived_year
  }

  if (!is.null(period)) {
    data$period <- period
  }

  if (clean_names) {
    names(data) <- make.names(names(data), unique = TRUE)
  }

  rownames(data) <- NULL
  attr(data, "tempo_code") <- code
  attr(data, "tempo_unit") <- unit
  data
}

.tempo_as_table <- function(x) {
  if (is.data.frame(x)) {
    for (index in seq_along(x)) {
      x[[index]] <- as.character(x[[index]])
    }
    return(x)
  }

  if (is.character(x) && length(x) == 1L && !is.na(x)) {
    if (grepl("[\r\n]", x)) {
      return(.tempo_read_csv_text(x))
    }
    if (file.exists(x)) {
      return(.tempo_read_csv_file(x))
    }
    .tempo_abort(
      sprintf("`x` is neither CSV text nor an existing file: %s", x),
      class = "tempo_input_error"
    )
  }

  .tempo_abort(
    paste0(
      "`x` must be a data frame, the path of a CSV file, or a single string ",
      "of CSV text."
    ),
    class = "tempo_input_error"
  )
}
