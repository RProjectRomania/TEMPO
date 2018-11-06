context("test toc")

test_that("tempo_toc", {
  toc<-tempo_toc()
  
  expect_is(toc, "data.frame")
  expect_type(toc, "list")
  expect_length(toc, 2)
  expect_type(toc[1], "list")
  expect_type(toc[2], "list")
  expect_type(toc[[1]], "character")
  expect_type(toc[[2]], "character")
  expect_named(toc)
  expect_identical(names(toc)[1], "Denumire")
  expect_identical(names(toc)[2], "Cod")
  
})