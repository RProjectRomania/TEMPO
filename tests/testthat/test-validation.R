test_that("tempo_toc validates inputs before making a request", {
  expect_error(
    tempo_toc(full_description = NA),
    "`full_description` must be either `TRUE` or `FALSE`"
  )
  expect_error(tempo_toc(language = "fr"), "`language` must be either")
  expect_error(tempo_toc(language = "fr"), class = "tempo_input_error")
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
  expect_error(
    tempo_bulk("ACC101B", directory = tempdir(), overwrite = "yes"),
    "`overwrite` must be either"
  )
})

test_that("single-code functions reject vectors", {
  expect_error(tempo_meta(c("A1", "B2")), "`code` must be a single")
  expect_error(tempo_get(c("A1", "B2")), class = "tempo_input_error")
})

test_that("tempo_search validates pattern, fields and toc", {
  toc <- data.frame(name = "x", code = "X1", stringsAsFactors = FALSE)

  expect_error(tempo_search(character(), toc = toc), "`pattern` must be")
  expect_error(tempo_search(1, toc = toc), "`pattern` must be")
  expect_error(
    tempo_search("x", fields = "nope", toc = toc),
    "not in `toc`"
  )
  expect_error(
    tempo_search("x", toc = toc[0, , drop = FALSE]),
    "at least one row"
  )
})

test_that("tempo_tidy rejects unusable input", {
  expect_error(tempo_tidy(42), class = "tempo_input_error")
  expect_error(tempo_tidy("no-such-file.csv"), "neither CSV text nor an existing file")
})

test_that("tempo_cache_dir validates its path", {
  expect_error(tempo_cache_dir(123), "`path` must be a single directory path")
})
