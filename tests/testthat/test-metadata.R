metadata <- list(
  dimensionsMap = list(
    list(label = "Judete", options = list(
      list(nomItemId = 1L, label = "Cluj"),
      list(nomItemId = 2L, label = "Bihor")
    )),
    list(label = "Ani", options = list(
      list(nomItemId = 10L, label = "Anul 2020")
    ))
  ),
  details = list(matMaxDim = 2L, matUMSpec = 0L),
  ultimaActualizare = "05-11-2025",
  ancestors = list(
    list(name = "ROOT"),
    list(name = 'AGRICULTURE <a href="https://insse.ro/x">Press releases</a>')
  )
)

test_that("option identifiers and labels are read from the metadata", {
  expect_equal(.tempo_option_ids(metadata, "AAA"), list(c("1", "2"), "10"))
  expect_equal(
    .tempo_option_labels(metadata, "AAA"),
    list(c("Cluj", "Bihor"), "Anul 2020")
  )
  expect_equal(.tempo_dimension_names(metadata, "AAA"), c("Judete", "Ani"))
})

test_that("dimension names fall back to positions when absent", {
  bare <- metadata
  bare$dimensionsMap[[1L]]$label <- NULL

  expect_equal(.tempo_dimension_names(bare, "AAA"), c("dimension_1", "Ani"))
})

test_that("labels are read from any of the plausible field names", {
  variants <- list(
    dimensionsMap = list(list(
      name = "Named dimension",
      options = list(
        list(nomItemId = 1L, nomItemName = "From nomItemName"),
        list(nomItemId = 2L, text = "From text")
      )
    )),
    details = list(matMaxDim = 1L, matUMSpec = 0L)
  )

  expect_equal(.tempo_dimension_names(variants, "V"), "Named dimension")
  expect_equal(
    .tempo_option_labels(variants, "V")[[1L]],
    c("From nomItemName", "From text")
  )
})

test_that("the query payload is built from the selected identifiers", {
  payload <- .tempo_payload("AAA101A", "en", metadata,
                            .tempo_option_ids(metadata, "AAA101A"))

  expect_equal(payload$encQuery, "1, 2:10")
  expect_equal(payload$matCode, "AAA101A")
  expect_equal(payload$language, "en")
  expect_equal(payload$matMaxDim, 2L)
})

test_that("ancestors are returned as plain text", {
  expect_equal(.tempo_strip_html(.tempo_ancestor(metadata, 2L)), "AGRICULTURE")
  expect_true(is.na(.tempo_ancestor(metadata, 9L)))
})

test_that("incomplete metadata is rejected with a classed error", {
  expect_error(
    .tempo_validate_metadata(list(details = list()), "AAA"),
    class = "tempo_api_error"
  )
  expect_error(
    .tempo_payload("AAA", "ro", list(details = list()), list("1")),
    "incomplete query metadata"
  )
})

test_that("the update date is parsed in every format the service might use", {
  expect_equal(.tempo_metadata_date(metadata), as.Date("2025-11-05"))

  for (value in c("05.11.2025", "05/11/2025", "2025-11-05")) {
    expect_equal(
      .tempo_metadata_date(list(ultimaActualizare = value)),
      as.Date("2025-11-05"),
      info = value
    )
  }
})

test_that("an unparseable update date warns instead of failing silently", {
  .tempo_reset_warnings()

  expect_warning(
    result <- .tempo_metadata_date(
      list(ultimaActualizare = "November 5th, 2025"),
      code = "AAA"
    ),
    class = "tempo_date_warning"
  )
  expect_true(is.na(result))

  # Warned once, not once per matrix.
  expect_silent(
    .tempo_metadata_date(list(ultimaActualizare = "November 5th, 2025"))
  )
})

test_that("a missing update date is NA without a warning", {
  .tempo_reset_warnings()
  expect_silent(result <- .tempo_metadata_date(list()))
  expect_true(is.na(result))
  expect_true(is.na(.tempo_metadata_update_string(list())))
})
