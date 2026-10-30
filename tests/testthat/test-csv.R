test_that("the header is separated from the body", {
  parts <- .tempo_csv_split(charToRaw("a,b\n1,2\n3,4\n"))

  expect_equal(rawToChar(parts$header), "a,b\n")
  expect_equal(rawToChar(parts$body), "1,2\n3,4\n")
})

test_that("a header-only response yields an empty body", {
  parts <- .tempo_csv_split(charToRaw("a,b\n"))
  expect_length(parts$body, 0L)
})

test_that("CSV chunks retain one header", {
  expect_equal(
    rawToChar(.tempo_without_csv_header(charToRaw("a,b\n1,2\n"))),
    "1,2\n"
  )
})

test_that("a response with no header line is an error", {
  expect_error(
    .tempo_csv_split(charToRaw("a,b")),
    "without a header line"
  )
  expect_error(
    .tempo_csv_split(charToRaw("a,b")),
    class = "tempo_api_error"
  )
})

test_that("header text ignores trailing newlines", {
  expect_equal(.tempo_header_text(charToRaw("a,b\r\n")), "a,b")
  expect_equal(.tempo_header_text(charToRaw("a,b")), "a,b")
})
