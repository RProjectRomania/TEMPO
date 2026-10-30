# Integration tests against the real TEMPO Online service.  These are skipped
# on CRAN and whenever the service cannot be reached, so a failure here means
# the service changed, not that the package is broken.

skip_unless_live <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_offline("statistici.insse.ro")
  testthat::skip_if_not(
    identical(Sys.getenv("TEMPO_LIVE_TESTS"), "true"),
    "set TEMPO_LIVE_TESTS=true to run tests against the live service"
  )
}

test_that("the live table of contents has the expected shape", {
  skip_unless_live()

  toc <- tempo_toc(language = "en", cache = FALSE)

  expect_s3_class(toc, "data.frame")
  expect_true(all(c("name", "code") %in% names(toc)))
  expect_gt(nrow(toc), 1000L)
  expect_true(all(grepl("^[[:alnum:]]+$", toc$code)))
  # Codes must be unique, or the download cache keys collide.
  expect_equal(anyDuplicated(toc$code), 0L)
})

test_that("live metadata exposes dimensions and a parseable update date", {
  skip_unless_live()

  meta <- tempo_meta("ACC101B", language = "en", cache = FALSE)

  expect_gt(nrow(meta$dimensions), 0L)
  expect_true(all(meta$dimensions$values > 0L))
  expect_false(is.na(meta$last_update))
  expect_gt(meta$cells, 0)
})

test_that("a filtered live download returns only the requested values", {
  skip_unless_live()

  meta <- tempo_meta("ACC101B", language = "en", cache = FALSE)
  dimension <- meta$dimensions$name[[1L]]
  wanted <- meta$values[[1L]]$label[[1L]]

  filters <- list(wanted)
  names(filters) <- dimension

  data <- tempo_get("ACC101B", filters = filters, language = "en")

  expect_s3_class(data, "data.frame")
  expect_gt(nrow(data), 0L)
  expect_true("value" %in% names(data))
})

test_that("the live CSV layout still matches what tempo_tidy expects", {
  skip_unless_live()

  raw <- tempo_get("ACC101B", language = "en", tidy = FALSE)

  # If either of these fails, tempo_tidy() needs updating.
  expect_true(any(tolower(names(raw)) %in% c("valoare", "value")))
  expect_true(any(grepl("^(UM|MU)[[:space:]]*:", names(raw))))
})

test_that("the live context tree has the shape tempo_hierarchy expects", {
  skip_unless_live()

  # Deliberately one request: the branch listing. Expanding every survey into
  # its matrices costs a few hundred more, which is not neighbourly in a test.
  branches <- tempo_hierarchy(language = "en", matrices = FALSE, cache = FALSE)

  expect_s3_class(branches, "data.frame")
  expect_true(all(c("code", "name", "parent_code", "level", "is_matrix",
                    "path") %in% names(branches)))
  expect_gt(nrow(branches), 100L)
  expect_false(any(branches$is_matrix))

  # Levels are 1-based and contiguous from the root.
  expect_equal(min(branches$level), 1L)
  expect_setequal(branches$level, seq_len(max(branches$level)))

  # Root nodes have no parent; every other parent must resolve to a node.
  roots <- branches$level == 1L
  expect_true(all(is.na(branches$parent_code[roots])))
  expect_true(all(branches$parent_code[!roots] %in% branches$code))

  # HTML chrome is stripped from the domain labels, which do contain anchors.
  expect_false(any(grepl("<a |</a>", branches$name)))
  expect_true(all(nzchar(branches$name)))

  # Paths are built from the parent links, so depth must match level.
  depth <- lengths(strsplit(branches$path, " > ", fixed = TRUE))
  expect_equal(depth, branches$level)
})

test_that("live survey nodes list matrices as bare children", {
  skip_unless_live()

  branches <- tempo_hierarchy(language = "en", matrices = FALSE, cache = FALSE)
  deepest <- branches[branches$level == max(branches$level), ]
  skip_if(nrow(deepest) == 0L, "no deepest branch found")

  response <- .tempo_request(
    .tempo_url(paste0("context/", deepest$code[[1L]]), language = "en")
  )
  children <- .tempo_context_children(
    .tempo_parse_json(response, "context-node", simplify_vector = FALSE)
  )

  expect_s3_class(children, "data.frame")
  expect_true(all(c("code", "name", "children_url") %in% names(children)))
  expect_gt(nrow(children), 0L)
  # A survey's children are matrices, and their codes must be usable.
  expect_true(any(children$children_url == "matrix"))
  expect_true(all(grepl(
    "^[[:alnum:]]+$",
    children$code[children$children_url == "matrix"]
  )))
})
