test_that("every TEMPO error inherits from tempo_error and error", {
  condition <- tryCatch(
    .tempo_abort("boom", class = "tempo_api_error", code = "X1"),
    condition = identity
  )

  expect_s3_class(condition, "tempo_api_error")
  expect_s3_class(condition, "tempo_error")
  expect_s3_class(condition, "error")
  expect_equal(condition$code, "X1")
  expect_equal(conditionMessage(condition), "boom")
})

test_that("input errors are catchable by class, not by message text", {
  handled <- tryCatch(
    .tempo_validate_language("fr"),
    tempo_input_error = function(condition) "input",
    tempo_error = function(condition) "other"
  )

  expect_equal(handled, "input")
})

test_that("a plain error handler still catches TEMPO conditions", {
  expect_equal(
    tryCatch(.tempo_validate_language("fr"), error = function(e) "caught"),
    "caught"
  )
})

test_that("warnings are raised once per session and reset on demand", {
  .tempo_reset_warnings()

  expect_warning(
    .tempo_warn_once("demo", "first time", class = "tempo_tidy_warning"),
    class = "tempo_tidy_warning"
  )
  expect_silent(.tempo_warn_once("demo", "second time"))

  .tempo_reset_warnings()
  expect_warning(.tempo_warn_once("demo", "again"))
})

test_that("retryable statuses are classified correctly", {
  expect_true(.tempo_status_retryable(429L))
  expect_true(.tempo_status_retryable(503L))
  expect_true(.tempo_status_retryable(500L))
  expect_false(.tempo_status_retryable(404L))
  expect_false(.tempo_status_retryable(400L))
  expect_false(.tempo_status_retryable(NA_integer_))
  expect_false(.tempo_status_retryable(NULL))
})

test_that("the user agent names the package", {
  expect_match(.tempo_user_agent(), "^TEMPO/")

  old <- options(TEMPO.user_agent = "custom/1.0")
  on.exit(options(old), add = TRUE)
  expect_equal(.tempo_user_agent(), "custom/1.0")
})
