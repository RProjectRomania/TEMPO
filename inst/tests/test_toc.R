context("test toc")

test_that("tempo_toc", {
  toc<-tempo_toc()
  
  expect_is(toc, "data.frame")
  expect_type(toc, "list")
  
  
})