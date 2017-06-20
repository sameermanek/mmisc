context("Check that coalesce works as expected")

test_that("Works with numeric vectors", {
  expect_equal(coalesce(c(1,2,NA), c(2,3,4)), c(1,2,4))
})

test_that("Works with vectors of different lengths", {
  expect_equal(coalesce(c(1,2,NA),0), c(1,2,0))
})

test_that("Works with character vectors", {
  expect_equal(coalesce(c('a','b',NA,'d'), 'b'), c('a','b','b','d'))
})

test_that("Works with 0 length vectors", {
  expect_true(length(coalesce(numeric(0), LETTERS, letters)) == 0)
})
