context("Check that date functions work as expected")

test_that("toDate works", {
  expect_equal(toDate(c('03/15/2013','03MAR2015','2015-03-12')), 
               c('2013-03-15', '2015-03-03', '2015-03-12'))
})

test_that("diffDate works", {
  expect_equal(
    diffDate(c('2013-03-01', '2016-03-16', '2016-09-09'),
             c('2017-01-01','2016-03-01','2015-01-01')),
    c(-1402, 15, 617)
    )
})

test_that("getTrailingDays works", {
  expect_equal(getTrailingDays('2017-04-01', 12), 
               c("2017-03-21", "2017-03-22", "2017-03-23", "2017-03-24", "2017-03-25", 
                 "2017-03-26", "2017-03-27", "2017-03-28", "2017-03-29", "2017-03-30", 
                 "2017-03-31", "2017-04-01"))
})

test_that("getFutureDays works", {
  expect_equal(getFutureDays('2017-04-01', 12),
               c("2017-04-01", "2017-04-02", "2017-04-03", "2017-04-04", "2017-04-05", 
                 "2017-04-06", "2017-04-07", "2017-04-08", "2017-04-09", "2017-04-10", 
                 "2017-04-11", "2017-04-12"))
})

test_that("getDateSequence works", {
  expect_equal(getDateSequence('2017-04-01', '2017-06-01', 7),
               c("2017-04-01", "2017-04-08", "2017-04-15", "2017-04-22", "2017-04-29", 
                 "2017-05-06", "2017-05-13", "2017-05-20", "2017-05-27"))
})

test_that("getMonth works", {
  expect_equal(getMonth(c('2013-03-12','2013-09-02')),
               c("2013-03-01", "2013-09-01"))
})

test_that("getYear works", {
  expect_equal(getYear(c('2013-03-01','2014-02-01')),
               c(2013,2014))
})

test_that("getSeason works", {
  expect_equal(getSeason(c('2013-03-01','2014-02-01')),
               c('Spring','Winter'))
})

test_that("getMonthName works", {
  expect_equal(getMonthName(c('2013-03-01','2014-02-01')),
              c('Mar','Feb'))
})


