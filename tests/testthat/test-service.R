# End-to-end tests against the fake service in helper-tempo-service.R.

test_that("tempo_toc lists matrices and strips HTML from names", {
  local_tempo_service()

  toc <- tempo_toc()

  expect_s3_class(toc, "data.frame")
  expect_equal(toc$code, c("SML101A", "BIG101A", "TAG101A"))
  expect_equal(toc$name[[3L]], "Tagged table")
  expect_equal(toc$comment, c(NA, "a comment", NA))
})

test_that("tempo_toc caches the listing", {
  process <- local_tempo_service()

  tempo_toc()
  tempo_toc()

  expect_equal(.tempo_cache_get(
    .tempo_cache_key("matrices", "ro", .tempo_base_url())
  )$code[[1L]], "SML101A")
})

test_that("tempo_toc(full_description = TRUE) returns plain-text classification", {
  local_tempo_service()

  described <- tempo_toc(full_description = TRUE)

  expect_equal(unique(described$Statistical_domain), "AGRICULTURE")
  expect_equal(unique(described$Statistical_sub_domain), "Sub-domain")
  expect_equal(unique(described$Survey_name), "Survey name")
  expect_equal(unique(described$Press_releases), "https://insse.ro/press?x=1")
  expect_s3_class(described$Last_update, "Date")
  expect_equal(unique(described$Last_update), as.Date("2025-11-05"))
})

test_that("tempo_search filters the listing", {
  local_tempo_service()

  expect_equal(tempo_search("big")$code, "BIG101A")
  expect_equal(nrow(tempo_search("fake")), 2L)
  expect_equal(nrow(tempo_search("nothing here")), 0L)
})

test_that("tempo_meta reports structure without downloading", {
  local_tempo_service()

  meta <- tempo_meta("BIG101A")

  expect_s3_class(meta, "tempo_meta")
  expect_equal(meta$code, "BIG101A")
  expect_equal(meta$dimensions$name, c("Judete", "Ani"))
  expect_equal(meta$dimensions$values, c(7L, 5L))
  expect_equal(meta$cells, 35)
  expect_equal(meta$domain, "AGRICULTURE")
  expect_equal(meta$last_update, as.Date("2025-11-05"))
  expect_equal(meta$values$Judete$label[[1L]], "County 1")
  expect_output(print(meta), "BIG101A")
  expect_output(print(meta), "35 cells")
})

test_that("tempo_get returns a tidy data frame", {
  local_tempo_service()

  data <- tempo_get("SML101A")

  expect_s3_class(data, "data.frame")
  expect_equal(nrow(data), 6L)
  expect_named(data, c("Judete", "Ani", "unit", "value"), ignore.order = TRUE)
  expect_type(data$value, "double")
  expect_type(data$Ani, "integer")
  expect_equal(attr(data, "tempo_code"), "SML101A")
  expect_equal(attr(data, "tempo_requests"), 1L)
})

test_that("tempo_get sends the filter to the service", {
  local_tempo_service()

  data <- tempo_get(
    "BIG101A",
    filters = list(Judete = c("County 1", "County 3"), Ani = 2020:2021)
  )

  expect_equal(nrow(data), 4L)
  expect_setequal(data$Judete, c("County 1", "County 3"))
  expect_setequal(data$Ani, c(2020L, 2021L))
})

test_that("a bad filter is rejected before any download", {
  local_tempo_service()

  expect_error(
    tempo_get("SML101A", filters = list(Judete = "Atlantis")),
    class = "tempo_input_error"
  )
})

test_that("tempo_get(tidy = FALSE) keeps the service layout", {
  local_tempo_service()

  raw <- tempo_get("SML101A", tidy = FALSE)

  expect_true("Valoare" %in% names(raw))
  expect_true(any(grepl("^UM:", names(raw))))
  expect_type(raw$Valoare, "character")
})

test_that("a chunked download reassembles every row exactly once", {
  process <- local_tempo_service()
  old <- options(TEMPO.max_cells = 6)
  on.exit(options(old), add = TRUE)

  # 7 x 5 = 35 cells with a 6-cell budget: several requests, each returning
  # its own header and its own footer line.
  data <- tempo_get("BIG101A")

  expect_gt(attr(data, "tempo_requests"), 1L)
  expect_equal(nrow(data), 35L)
  expect_equal(nrow(unique(data[c("Judete", "Ani")])), 35L)
  # No footer line survived into the data.
  expect_false(any(is.na(data$Judete)))
})

test_that("inconsistent headers across chunks are an error, not a silent mess", {
  local_tempo_service()
  old <- options(TEMPO.max_cells = 1)
  on.exit(options(old), add = TRUE)

  expect_error(
    tempo_get("BADHDR"),
    "inconsistent CSV headers"
  )
  expect_error(tempo_get("BADHDR"), class = "tempo_api_error")
})

test_that("tempo_bulk writes a CSV, a manifest, and reports what it did", {
  local_tempo_service()
  directory <- tempfile("tempo-dl-")

  files <- tempo_bulk("SML101A", directory = directory)

  expect_equal(names(files), "SML101A")
  expect_true(file.exists(files[["SML101A"]]))
  expect_true(attr(files, "downloaded"))
  expect_equal(attr(files, "reason"), "missing")
  expect_true(file.exists(file.path(dirname(files[[1L]]), ".tempo-manifest.json")))

  tidy <- tempo_tidy(files[["SML101A"]], code = "SML101A")
  expect_equal(nrow(tidy), 6L)
})

test_that("tempo_bulk returns paths for skipped files too", {
  local_tempo_service()
  directory <- tempfile("tempo-dl-")

  tempo_bulk("SML101A", directory = directory)
  expect_message(
    again <- tempo_bulk("SML101A", directory = directory),
    "up to date"
  )

  # Version 0.1.0 returned character(0) here, so callers had to rebuild paths.
  expect_equal(names(again), "SML101A")
  expect_true(file.exists(again[["SML101A"]]))
  expect_false(attr(again, "downloaded"))
  expect_equal(attr(again, "reason"), "current")
})

test_that("switching language re-downloads instead of reusing the other one", {
  local_tempo_service()
  directory <- tempfile("tempo-dl-")

  tempo_bulk("SML101A", language = "ro", directory = directory)
  switched <- tempo_bulk("SML101A", language = "en", directory = directory)

  expect_true(attr(switched, "downloaded"))
  expect_equal(attr(switched, "reason"), "language")
})

test_that("overwrite forces a download of a current file", {
  local_tempo_service()
  directory <- tempfile("tempo-dl-")

  tempo_bulk("SML101A", directory = directory)
  forced <- tempo_bulk("SML101A", directory = directory, overwrite = TRUE)

  expect_true(attr(forced, "downloaded"))
  expect_equal(attr(forced, "reason"), "overwrite")
})

test_that("a retryable status is retried and then succeeds", {
  local_tempo_service()

  # The fake service fails this endpoint twice with HTTP 500.
  response <- .tempo_request(.tempo_url("flaky/matrices"))
  expect_equal(response$status_code, 200L)
})

test_that("a non-retryable status raises a tempo_http_error with its code", {
  local_tempo_service()

  condition <- tryCatch(tempo_meta("NOSUCH"), condition = identity)

  expect_s3_class(condition, "tempo_http_error")
  expect_equal(condition$status_code, 404L)
  expect_match(condition$url, "matrix/NOSUCH")
  expect_match(conditionMessage(condition), "HTTP 404")
})

test_that("an unreachable service raises a tempo_offline_error", {
  old <- options(
    TEMPO.base_url = "http://127.0.0.1:1/tempo-ins",
    TEMPO.max_attempts = 1L,
    TEMPO.backoff = 0
  )
  on.exit(options(old), add = TRUE)

  expect_error(tempo_toc(cache = FALSE), class = "tempo_offline_error")
})

test_that("concurrent fetching returns responses in request order", {
  local_tempo_service()

  urls <- vapply(
    c("SML101A", "BIG101A", "TAG101A"),
    function(code) .tempo_url(paste0("matrix/", code)),
    character(1),
    USE.NAMES = FALSE
  )

  responses <- .tempo_request_many(urls)
  names <- vapply(
    responses,
    function(response) {
      .tempo_parse_json(response, "metadata")$matrixName
    },
    character(1)
  )

  expect_equal(names, paste("Fake matrix", c("SML101A", "BIG101A", "TAG101A")))
})

test_that("an unparseable service date warns but keeps caching working", {
  .tempo_reset_warnings()
  local_tempo_service()
  directory <- tempfile("tempo-dl-")

  expect_warning(
    tempo_bulk("ODDDATE", directory = directory),
    class = "tempo_date_warning"
  )

  # The raw update string still identifies the revision, so the second call
  # reuses the file rather than downloading for ever.
  again <- tempo_bulk("ODDDATE", directory = directory)
  expect_false(attr(again, "downloaded"))
  expect_equal(attr(again, "reason"), "current")
})

test_that("tempo_hierarchy returns the branch structure in one request", {
  local_tempo_service()

  branches <- tempo_hierarchy(matrices = FALSE)

  expect_s3_class(branches, "data.frame")
  expect_equal(nrow(branches), 3L)
  expect_false(any(branches$is_matrix))
  # HTML chrome stripped, 1-based levels, root parent reported as NA not "0".
  expect_equal(branches$name[[1L]], "AGRICULTURE")
  expect_equal(branches$level, c(1L, 2L, 3L))
  expect_true(is.na(branches$parent_code[[1L]]))
  expect_equal(branches$parent_code[[3L]], "10")
  expect_equal(branches$path[[3L]], "AGRICULTURE > Land use > Areas and yields")
})

test_that("tempo_hierarchy expands matrices under the deepest branches", {
  local_tempo_service()

  tree <- tempo_hierarchy()

  expect_setequal(tree$code[tree$is_matrix], c("SML101A", "BIG101A"))
  expect_equal(unique(tree$level[tree$is_matrix]), 4L)
  expect_equal(
    tree$path[tree$code == "SML101A"],
    "AGRICULTURE > Land use > Areas and yields > Small fake table"
  )
  expect_equal(tree$parent_code[tree$code == "SML101A"], "1010")
  # Only the deepest branch is expanded, not every branch.
  expect_equal(nrow(tree), 5L)
})

test_that("an unexpected context-tree shape is reported, not guessed at", {
  local_tempo_service()
  old <- options(TEMPO.base_url = paste0(.tempo_base_url(), "/nonexistent"))
  on.exit(options(old), add = TRUE)

  condition <- tryCatch(tempo_hierarchy(cache = FALSE), condition = identity)

  expect_s3_class(condition, "tempo_api_error")
  expect_match(conditionMessage(condition), "unexpected context-tree format")
  expect_match(conditionMessage(condition), "tempo_toc\\(full_description")
})

test_that("the context parsers handle each shape in isolation", {
  root <- list(
    list(parentCode = "0", level = 0L,
         context = list(name = "DOMAIN <a href=\"x\">Press releases</a>",
                        code = "1", childrenUrl = "context")),
    list(parentCode = "1", level = 1L,
         context = list(name = "Sub", code = "10", childrenUrl = "context"))
  )

  parsed <- .tempo_context_root(root)
  expect_equal(parsed$code, c("1", "10"))
  expect_equal(parsed$name, c("DOMAIN", "Sub"))
  expect_equal(parsed$level, c(1L, 2L))
  expect_true(is.na(parsed$parent_code[[1L]]))

  node <- list(
    context = list(name = "Survey", code = "1010", childrenUrl = "context"),
    ancestors = list(list(name = "home", code = "", childrenUrl = "context")),
    children = list(
      list(name = "A table", code = "POP105A", childrenUrl = "matrix")
    )
  )

  children <- .tempo_context_children(node)
  expect_equal(children$code, "POP105A")
  expect_equal(children$children_url, "matrix")

  expect_null(.tempo_context_root(list()))
  expect_null(.tempo_context_children(list(children = list())))
})
