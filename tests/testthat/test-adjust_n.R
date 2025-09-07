testthat::test_that("adjust_n works with intended inputs", {
  
  # Test default behavior
  expect_equal(
    adjust_n(as.Date("2020-01-01"), freq = "week", n_size = 1), 
    1
  )
  
  # Reduces n_size when it's > length of dates
  dates_week <- as.Date(
    c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04", "2020-01-05", "2020-01-06", 
      "2020-01-07")
  )
  expect_equal(
    adjust_n(dates_week, freq = "week", n_size = 8), 
    7
  )
  
  # Returns 1 with very few sample dates
  dates_month <- as.Date(c("2020-01-01", "2020-01-15", "2020-01-30"))
  expect_equal(
    adjust_n(dates_month, freq = "month", n_size = 8), 
    1
  )  # 3 days in January
  
  # Test quarterly frequency (leap year)
  dates_quarter <- seq.Date(as.Date("2020-01-01"), as.Date("2020-03-31"), by = "day")
  expect_equal(
    adjust_n(dates_quarter, freq = "quarter", n_size = 100), 
    91
  )
  
  # Test quarterly frequency (non-leap year)
  dates_quarter <- seq.Date(as.Date("2021-01-01"), as.Date("2021-03-31"), by = "day")
  expect_equal(
    adjust_n(dates_quarter, freq = "quarter", n_size = 91), 
    90
  )
  
  # Test yearly frequency (non-leap year)
  dates_year <- seq.Date(as.Date("2019-01-01"), as.Date("2019-12-31"), by = "day")
  expect_equal(
    adjust_n(dates_year, freq = "year", n_size = 365), 
    365
  )
  
  # Test yearly frequency (leap year)
  dates_year <- seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day")
  expect_equal(
    adjust_n(dates_year, freq = "year", n_size = 366), 
    366
  )
  
})

testthat::test_that("adjust_n works with edge cases", {
  
  dates_edge <- as.Date(c("2020-01-01"))  # Only one date
  expect_equal(
    adjust_n(dates_edge, freq = "month", n_size = 8), 
    1
  )
  
})