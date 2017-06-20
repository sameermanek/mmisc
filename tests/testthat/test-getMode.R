context("Check that getMode works as expected")

test_that("Works with numeric vectors", {
  expect_equal(getMode(c(1,2,3,3)), 3)
})

test_that("Works with character vectors", {
  expect_equal(getMode(c(LETTERS,'A')), 'A')
})