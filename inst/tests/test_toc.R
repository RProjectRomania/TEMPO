context("test toc")

test_that("tempo_toc", {
  toc<-tempo_toc()
  
  expect_is(toc, "data.frame")
  expect_type(toc, "list")
  expect_named(toc)
  expect_identical(names(toc)[1], "Denumire")
  expect_identical(names(toc)[2], "Cod")
  
})