service_csv <- paste(
  "Judete,Ani,UM: Numar persoane,Valoare",
  "Cluj,Anul 2019,Numar persoane,1234",
  "Cluj,Anul 2020,Numar persoane,:",
  "Bihor,Anul 2019,Numar persoane,987",
  ",,,(c) INS 1998-2026",
  sep = "\n"
)

test_that("the unit of measure is lifted out of the column header", {
  tidy <- tempo_tidy(service_csv, code = "DEMO")

  expect_false(any(grepl("^UM:", names(tidy))))
  expect_equal(unique(tidy$unit), "Numar persoane")
  expect_equal(attr(tidy, "tempo_unit"), "Numar persoane")
  expect_equal(attr(tidy, "tempo_code"), "DEMO")
})

test_that("observations become numeric and ':' becomes NA", {
  tidy <- tempo_tidy(service_csv)

  expect_type(tidy$value, "double")
  expect_equal(tidy$value, c(1234, NA, 987))
  expect_false("Valoare" %in% names(tidy))
})

test_that("footer lines are dropped but missing observations are kept", {
  tidy <- tempo_tidy(service_csv)

  # Three data rows survive; the copyright line does not.
  expect_equal(nrow(tidy), 3L)
  expect_false(any(grepl("INS 1998", unlist(lapply(tidy, as.character)))))
  # The row whose value is ":" is a real observation and must remain.
  expect_equal(sum(is.na(tidy$value)), 1L)
})

test_that("footer dropping can be switched off", {
  .tempo_reset_warnings()

  # Keeping the footer means its text reaches the observation column, which is
  # exactly the case that must warn rather than turn quietly into NA.
  expect_warning(
    tidy <- tempo_tidy(service_csv, drop_notes = FALSE),
    class = "tempo_tidy_warning"
  )
  expect_equal(nrow(tidy), 4L)
  expect_true(is.na(tidy$value[[4L]]))
  expect_equal(tidy$value_raw[[4L]], "(c) INS 1998-2026")
})

test_that("the year label becomes an integer", {
  tidy <- tempo_tidy(service_csv)

  expect_type(tidy$Ani, "integer")
  expect_equal(tidy$Ani, c(2019L, 2020L, 2019L))
})

test_that("monthly labels produce a Date period", {
  monthly <- paste(
    "Luni,Ani,UM: Numar,Valoare",
    "luna ianuarie,Anul 2024,Numar,10",
    "luna februarie,Anul 2024,Numar,12",
    "luna decembrie,Anul 2024,Numar,14",
    sep = "\n"
  )

  tidy <- tempo_tidy(monthly)

  expect_s3_class(tidy$period, "Date")
  expect_equal(
    tidy$period,
    as.Date(c("2024-01-01", "2024-02-01", "2024-12-01"))
  )
  # The readable label is left alone.
  expect_equal(tidy$Luni[[1L]], "luna ianuarie")
})

test_that("quarterly labels produce a Date period", {
  quarterly <- paste(
    "Trimestre,Ani,UM: Numar,Valoare",
    "Trimestrul I,Anul 2023,Numar,1",
    "Trimestrul III,Anul 2023,Numar,3",
    sep = "\n"
  )

  tidy <- tempo_tidy(quarterly)
  expect_equal(tidy$period, as.Date(c("2023-01-01", "2023-07-01")))
})

test_that("English labels are parsed too", {
  english <- paste(
    "Months,Years,MU: Number,Value",
    "month January,Year 2024,Number,10",
    "month July,Year 2024,Number,20",
    sep = "\n"
  )

  tidy <- tempo_tidy(english)
  expect_equal(tidy$period, as.Date(c("2024-01-01", "2024-07-01")))
  expect_equal(unique(tidy$unit), "Number")
  expect_equal(tidy$value, c(10, 20))
})

test_that("period parsing can be switched off", {
  tidy <- tempo_tidy(service_csv, periods = FALSE)

  expect_type(tidy$Ani, "character")
  expect_equal(tidy$Ani[[1L]], "Anul 2019")
  expect_false("period" %in% names(tidy))
})

test_that("annual data gets no period column, only an integer year", {
  tidy <- tempo_tidy(service_csv)
  expect_false("period" %in% names(tidy))
})

test_that("unreadable observations warn and are preserved", {
  .tempo_reset_warnings()
  odd <- paste(
    "Judete,Ani,UM: Numar,Valoare",
    "Cluj,Anul 2019,Numar,1234",
    "Bihor,Anul 2019,Numar,confidential",
    sep = "\n"
  )

  expect_warning(tidy <- tempo_tidy(odd), class = "tempo_tidy_warning")
  expect_true("value_raw" %in% names(tidy))
  expect_equal(tidy$value_raw, c("1234", "confidential"))
  expect_equal(tidy$value, c(1234, NA))
})

test_that("thousands separators are tolerated", {
  spaced <- paste(
    "Judete,Ani,UM: Lei,Valoare",
    "Cluj,Anul 2019,Lei,\"1 234 567\"",
    sep = "\n"
  )

  expect_equal(tempo_tidy(spaced)$value, 1234567)
})

test_that("clean_names produces syntactic names on request", {
  spaced <- paste(
    "Macroregiuni si judete,Ani,UM: Numar,Valoare",
    "Cluj,Anul 2019,Numar,1",
    sep = "\n"
  )

  expect_true("Macroregiuni si judete" %in% names(tempo_tidy(spaced)))
  expect_true(
    "Macroregiuni.si.judete" %in% names(tempo_tidy(spaced, clean_names = TRUE))
  )
})

test_that("a table with no recognised value column warns once", {
  .tempo_reset_warnings()
  odd <- "Judete,Ani\nCluj,Anul 2019"

  expect_warning(tidy <- tempo_tidy(odd), class = "tempo_tidy_warning")
  expect_equal(names(tidy), c("Judete", "Ani"))
})

test_that("tempo_tidy accepts a data frame and a file path", {
  frame <- .tempo_read_csv_text(service_csv)
  expect_equal(nrow(tempo_tidy(frame)), 3L)

  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(service_csv, path)
  expect_equal(nrow(tempo_tidy(path)), 3L)
})

test_that("period parsers handle the labels in isolation", {
  expect_equal(.tempo_parse_year(c("Anul 2020", "2021", "Year 1999")),
               c(2020L, 2021L, 1999L))
  expect_equal(.tempo_parse_month(c("luna mai", "May", "decembrie")),
               c(5L, 5L, 12L))
  expect_equal(.tempo_parse_quarter(c("Trimestrul IV", "Quarter 2",
                                      "Trim. II")),
               c(4L, 2L, 2L))
  expect_equal(.tempo_parse_semester(c("Semestrul I", "Semester 2")), c(1L, 2L))
  expect_true(is.na(.tempo_parse_month("Total")))
})
