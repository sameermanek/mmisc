context("Check that expandData works as expected")

test_that("Works with character column names", {
  df <- data.frame(a = 1:4, b = c('A','B','C','D'), c = runif(4), stringsAsFactors=FALSE)
  expect_equal(nrow(expandData(df, c('a','b'))), 16)
})

test_that("Works with mxt.data as a list", {
  df <- data.frame(a = 1:4, b = c('A','B','C','D'), c = runif(4), stringsAsFactors=FALSE)
  expect_equal(nrow(expandData(df, list(a=c(1:10), b=LETTERS))), 260)
})