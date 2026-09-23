test_that("tempo_toc validates inputs before making a request", {
  expect_error(
    tempo_toc(full_description = NA),
    "`full_description` must be either `TRUE` or `FALSE`"
  )
  expect_error(
    tempo_toc(language = "fr"),
    "`language` must be either"
  )
})

test_that("tempo_bulk validates its inputs before making a request", {
  expect_error(
    tempo_bulk(character(), directory = tempdir()),
    "`codes` must contain"
  )
  expect_error(
    tempo_bulk("ACC 101B", directory = tempdir()),
    "only letters and numbers"
  )
  expect_error(
    tempo_bulk("ACC101B", directory = NULL),
    "`directory` must be a single path"
  )
})
