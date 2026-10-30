#' TEMPO: Access Romanian National Institute of Statistics Data
#'
#' Tools for the Romanian National Institute of Statistics TEMPO Online
#' database: find the statistical tables, inspect their structure, and download
#' whole tables or selected parts of them.
#'
#' @section Finding a table:
#' [tempo_toc()] lists every matrix with its code, [tempo_search()] filters
#' that list, and [tempo_hierarchy()] returns the classification tree.
#'
#' @section Looking before downloading:
#' [tempo_meta()] reports a matrix's dimensions, the values selectable in each,
#' the publication date, and how many cells and requests a full download would
#' cost.
#'
#' @section Getting the data:
#' [tempo_get()] downloads a table, or a filtered part of one, straight into a
#' data frame. [tempo_bulk()] writes whole tables to CSV files and keeps them
#' up to date across sessions. [tempo_tidy()] turns the service's CSV layout
#' into analysis-ready data, and is applied by [tempo_get()] by default.
#'
#' @section Configuration:
#' [TEMPO-options] documents the options controlling timeouts, retries,
#' concurrency and query size; [tempo_cache_dir()] controls response caching;
#' [TEMPO-conditions] documents the classed errors and warnings.
#'
#' @section The service:
#' TEMPO Online is a public service of the Romanian National Institute of
#' Statistics, reachable only over plain HTTP and offering no documented API
#' contract or published rate limit. This package identifies itself with a
#' `User-Agent`, retries only what is worth retrying, and caps concurrency at
#' four connections by default. Please keep it modest.
#'
#' @keywords internal
"_PACKAGE"
