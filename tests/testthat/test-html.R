test_that("HTML anchors are stripped from service labels", {
  # This is exactly what the service returns for a top-level domain.
  domain <- paste0(
    'AGRICULTURE <a href="https://insse.ro/cms/en/comunicate-de-presa-view',
    '?field_categorie_value_i18n%5B%5D=1" target="_blank">Press releases</a>'
  )

  expect_equal(.tempo_strip_html(domain), "AGRICULTURE")
})

test_that("stripping handles several anchors, entities and NA", {
  value <- paste0(
    'LABOUR FORCE <a href="https://insse.ro/a">One-Press releases</a>; ',
    '<a href="https://insse.ro/b">Two-Press releases</a>'
  )

  expect_equal(.tempo_strip_html(value), "LABOUR FORCE")
  expect_equal(.tempo_strip_html("Trade &amp; industry"), "Trade & industry")
  expect_equal(.tempo_strip_html("  spaced   out  "), "spaced out")
  expect_true(is.na(.tempo_strip_html(NA_character_)))
  expect_equal(.tempo_strip_html("plain text"), "plain text")
  # A label that is nothing but a link keeps its text, or it would vanish.
  expect_equal(
    .tempo_strip_html('<a href="https://insse.ro/x">Only a link</a>'),
    "Only a link"
  )
})

test_that("the press-release URLs are kept rather than discarded", {
  value <- paste0(
    'LABOUR FORCE <a href="https://insse.ro/a">One</a>; ',
    '<a href="https://insse.ro/b">Two</a>'
  )

  expect_equal(
    .tempo_extract_links(value),
    "https://insse.ro/a https://insse.ro/b"
  )
  expect_true(is.na(.tempo_extract_links("no links here")))
})

test_that("the table-of-contents parser strips HTML and keeps comments", {
  response <- list(content = charToRaw(paste0(
    '[{"name":"First table <a href=\\"https://insse.ro/x\\">Press releases</a>",',
    '"code":"AAA101A","comment":null,"extra":"ignored"},',
    '{"name":"Second table","code":"BBB101B","comment":"note"}]'
  )))

  toc <- .tempo_toc_from_response(response)

  expect_s3_class(toc, "data.frame")
  expect_equal(toc$code, c("AAA101A", "BBB101B"))
  expect_equal(toc$name, c("First table", "Second table"))
  expect_equal(toc$comment, c(NA, "note"))
})

test_that("the table-of-contents parser rejects malformed responses", {
  response <- list(content = charToRaw('[{"title":"Missing code"}]'))

  expect_error(
    .tempo_toc_from_response(response),
    "unexpected table-of-contents format"
  )
  expect_error(
    .tempo_toc_from_response(response),
    class = "tempo_api_error"
  )
})
