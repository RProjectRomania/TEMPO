test_that("a small matrix is fetched in a single request", {
  plan <- .tempo_plan_query(list(as.character(1:5), as.character(1:4)),
                            code = "SML", max_cells = 1000)

  expect_length(plan, 2L)
  expect_equal(attr(plan, "requests"), 1L)
  expect_equal(attr(plan, "cells"), 20)
  expect_equal(plan[[1L]], list(as.character(1:5)))
})

test_that("the budget is the product of selected values, not one dimension", {
  # Five dimensions of 200 options each: no dimension crosses the historical
  # 300-option threshold, yet the query asks for 3.2e11 cells.  This is the
  # case the previous per-dimension chunking let through untouched.
  option_ids <- replicate(5L, as.character(seq_len(200L)), simplify = FALSE)

  plan <- .tempo_plan_query(option_ids, code = "HUGE", max_cells = 50000,
                            max_requests = Inf)

  expect_gt(attr(plan, "requests"), 1L)
  expect_lte(prod(attr(plan, "chunk_sizes")), 50000)
  # Every option still appears exactly once per dimension.
  for (index in seq_along(option_ids)) {
    expect_equal(sort(unlist(plan[[index]])), sort(option_ids[[index]]))
  }
})

test_that("a single huge dimension is split and fully covered", {
  plan <- .tempo_plan_query(list(as.character(seq_len(1000L))),
                            code = "ONE", max_cells = 100)

  expect_equal(prod(attr(plan, "chunk_sizes")), 100L)
  expect_equal(attr(plan, "requests"), 10L)
  expect_equal(sort(as.integer(unlist(plan[[1L]]))), 1:1000)
})

test_that("the per-dimension cap is honoured even when the product is small", {
  plan <- .tempo_plan_query(list(as.character(seq_len(400L))),
                            code = "CAP", max_cells = 1e9, max_options = 300L)

  expect_equal(attr(plan, "chunk_sizes"), 300L)
  expect_equal(attr(plan, "requests"), 2L)
})

test_that("an absurd download is refused rather than attempted", {
  option_ids <- replicate(4L, as.character(seq_len(500L)), simplify = FALSE)

  expect_error(
    .tempo_plan_query(option_ids, code = "NOPE", max_cells = 10,
                      max_requests = 100),
    "would need"
  )
  expect_error(
    .tempo_plan_query(option_ids, code = "NOPE", max_cells = 10,
                      max_requests = 100),
    class = "tempo_input_error"
  )
})

test_that("empty dimensions are rejected", {
  expect_error(
    .tempo_plan_query(list(character()), code = "EMPTY"),
    class = "tempo_api_error"
  )
})

test_that("chunking splits into equal pieces with a remainder", {
  chunks <- .tempo_chunk(as.character(seq_len(301L)), 100L)

  expect_length(chunks, 4L)
  expect_equal(vapply(chunks, length, integer(1)), c(100L, 100L, 100L, 1L))
  expect_equal(.tempo_chunk(1:3, 10L), list(1:3))
})

test_that("the Cartesian walk visits every combination once, in order", {
  chunks <- .tempo_chunk(as.character(seq_len(301L)), 100L)
  selections <- list()

  .tempo_each_selection(
    list(chunks, list("x", "y")),
    function(selection) selections[[length(selections) + 1L]] <<- selection
  )

  expect_length(selections, 8L)
  expect_equal(selections[[1L]], list(chunks[[1L]], "x"))
  expect_equal(selections[[8L]], list(chunks[[4L]], "y"))
  expect_equal(length(unique(selections)), 8L)
})
