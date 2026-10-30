metadata <- list(
  dimensionsMap = list(
    list(label = "Judete", options = list(
      list(nomItemId = 1L, label = "Cluj"),
      list(nomItemId = 2L, label = "Bihor"),
      list(nomItemId = 3L, label = "Bucuresti")
    )),
    list(label = "Ani", options = list(
      list(nomItemId = 10L, label = "Anul 2019"),
      list(nomItemId = 11L, label = "Anul 2020"),
      list(nomItemId = 12L, label = "Anul 2021")
    ))
  ),
  details = list(matMaxDim = 2L, matUMSpec = 0L)
)

test_that("no filters selects everything", {
  expect_equal(
    .tempo_resolve_filters(NULL, metadata, "X"),
    list(c("1", "2", "3"), c("10", "11", "12"))
  )
})

test_that("labels are matched exactly, ignoring case and space", {
  selection <- .tempo_resolve_filters(
    list(Judete = c("cluj", "  Bihor ")),
    metadata, "X"
  )

  expect_equal(selection[[1L]], c("1", "2"))
  expect_equal(selection[[2L]], c("10", "11", "12"))
})

test_that("a year matches its label as a whole token", {
  selection <- .tempo_resolve_filters(list(Ani = 2019:2020), metadata, "X")
  expect_equal(selection[[2L]], c("10", "11"))
})

test_that("service identifiers can be used directly", {
  selection <- .tempo_resolve_filters(list(Ani = "12"), metadata, "X")
  expect_equal(selection[[2L]], "12")
})

test_that("dimensions can be named partially or by position", {
  expect_equal(
    .tempo_resolve_filters(list(jude = "Cluj"), metadata, "X")[[1L]],
    "1"
  )
  expect_equal(
    .tempo_resolve_filters(list(`2` = 2021), metadata, "X")[[2L]],
    "12"
  )
})

test_that("a NULL filter means the whole dimension", {
  expect_equal(
    .tempo_resolve_filters(list(Judete = NULL), metadata, "X")[[1L]],
    c("1", "2", "3")
  )
})

test_that("an unmatched value is an error that lists what is available", {
  expect_error(
    .tempo_resolve_filters(list(Judete = "Atlantis"), metadata, "X"),
    "Available values"
  )
  expect_error(
    .tempo_resolve_filters(list(Judete = "Atlantis"), metadata, "X"),
    class = "tempo_input_error"
  )
  # The message names the offending value, not just the dimension.
  expect_error(
    .tempo_resolve_filters(list(Judete = "Atlantis"), metadata, "X"),
    "Atlantis"
  )
})

test_that("an unknown dimension is an error that lists the real ones", {
  expect_error(
    .tempo_resolve_filters(list(Nowhere = "x"), metadata, "X"),
    "Available dimensions"
  )
})

test_that("filters must be a named list", {
  expect_error(
    .tempo_resolve_filters(list("Cluj"), metadata, "X"),
    "must be a named list"
  )
  expect_error(
    .tempo_resolve_filters(list(Judete = character()), metadata, "X"),
    "selects no values"
  )
})

test_that("partial matching does not mask a wrong token", {
  # "202" must not quietly match "Anul 2020"; whole tokens only.
  expect_error(
    .tempo_resolve_filters(list(Ani = "202"), metadata, "X"),
    class = "tempo_input_error"
  )
})
