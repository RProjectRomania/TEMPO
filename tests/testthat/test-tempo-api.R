test_that("the table-of-contents parser keeps the public columns", {
  response <- list(content = charToRaw(paste0(
    '[{"name":"First table","code":"AAA101A","extra":"ignored"},',
    '{"name":"Second table","code":"BBB101B","extra":"ignored"}]'
  )))

  toc <- TEMPO:::.tempo_toc_from_response(response)

  expect_s3_class(toc, "data.frame")
  expect_named(toc, c("name", "code"))
  expect_equal(toc$code, c("AAA101A", "BBB101B"))
})

test_that("the table-of-contents parser rejects malformed responses", {
  response <- list(content = charToRaw('[{"title":"Missing code"}]'))

  expect_error(
    TEMPO:::.tempo_toc_from_response(response),
    "unexpected table-of-contents format"
  )
})

test_that("metadata helpers build a valid query payload", {
  metadata <- list(
    dimensionsMap = list(
      list(options = list(list(nomItemId = 1L), list(nomItemId = 2L))),
      list(options = list(list(nomItemId = 10L)))
    ),
    details = list(matMaxDim = 2L, matUMSpec = 0L),
    ultimaActualizare = "05-11-2025"
  )

  option_ids <- TEMPO:::.tempo_option_ids(metadata, "AAA101A")
  payload <- TEMPO:::.tempo_payload("AAA101A", "en", metadata, option_ids)

  expect_equal(option_ids, list(c("1", "2"), "10"))
  expect_equal(payload$encQuery, "1, 2:10")
  expect_equal(payload$matCode, "AAA101A")
  expect_equal(TEMPO:::.tempo_metadata_date(metadata), as.Date("2025-11-05"))
})

test_that("large dimensions are chunked and combined without omissions", {
  chunks <- TEMPO:::.tempo_split_options(as.character(seq_len(301L)))
  selections <- list()

  TEMPO:::.tempo_each_selection(
    list(chunks, list(c("x"), c("y"))),
    function(selection) selections[[length(selections) + 1L]] <<- selection
  )

  expect_length(chunks, 4L)
  expect_equal(vapply(chunks, length, integer(1)), c(100L, 100L, 100L, 1L))
  expect_length(selections, 8L)
  expect_equal(selections[[1L]], list(chunks[[1L]], "x"))
  expect_equal(selections[[8L]], list(chunks[[4L]], "y"))
})

test_that("CSV chunks retain one header", {
  csv <- charToRaw("a,b\n1,2\n")

  expect_equal(
    rawToChar(TEMPO:::.tempo_without_csv_header(csv)),
    "1,2\n"
  )
  expect_error(
    TEMPO:::.tempo_without_csv_header(charToRaw("a,b")),
    "without a header line"
  )
})
