context("rqcQA")

test_that("rqcQA should always return list", {
  folder <- system.file(package = "ShortRead", "extdata/E-MTAB-1147")
  files <- list.files(full.names = TRUE, path = folder)
  rqcResultSet <- rqcQA(files, pair = c(1,1), workers = 1)
  expect_is(rqcResultSet, "list")
  
  rqcResultSet <- rqcQA(files[1], pair = c(1,1), workers = 1)
  expect_is(rqcResultSet, "list")
})
