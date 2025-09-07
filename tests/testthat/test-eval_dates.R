testthat::test_that("eval_dates works with different `by` parameters", {
  
  dates <- seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day")
  rec_start <- as.Date("2020-01-01")
  rec_end <- as.Date("2020-12-31")
  
  # Test for `by = "day"`
  results <- eval_dates(dates, rec_start, rec_end, by = "day")
  testthat::expect_equal(results$pct_complete, 100)
  testthat::expect_equal(results$n_miss, 0)
  
  # Test for `by = "week"`
  results <- eval_dates(dates, rec_start, rec_end, by = "week")
  testthat::expect_equal(results$pct_complete, 100)
  testthat::expect_equal(results$n_miss, 0)
  
  # Test for `by = "month"`
  results <- eval_dates(dates, rec_start, rec_end, by = "month")
  testthat::expect_equal(results$pct_complete, 100)
  testthat::expect_equal(results$n_miss, 0)
  
  # Test for `by = "quarter"`
  results <- eval_dates(dates, rec_start, rec_end, by = "quarter")
  testthat::expect_equal(results$pct_complete, 100)
  testthat::expect_equal(results$n_miss, 0)
  
  # Test for `by = "year"`
  results <- eval_dates(dates, rec_start, rec_end, by = "year")
  testthat::expect_equal(results$pct_complete, 100)
  testthat::expect_equal(results$n_miss, 0)
  
})

testthat::test_that("eval_dates handles missing dates correctly", {
  
  dates <- seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "month")
  rec_start <- as.Date("2020-01-01")
  rec_end <- as.Date("2021-12-31")
  
  # Test for `by = "day"`
  results <- eval_dates(dates, rec_start, rec_end, by = "day")
  testthat::expect_lt(results$pct_complete, 100)
  testthat::expect_gt(results$n_miss, 0)
  
  # Test for `by = "week"`
  results <- eval_dates(dates, rec_start, rec_end, by = "week")
  testthat::expect_lt(results$pct_complete, 100)
  testthat::expect_gt(results$n_miss, 0)
  
  # Test for `by = "month"`
  results <- eval_dates(dates, rec_start, rec_end, by = "month")
  testthat::expect_lt(results$pct_complete, 100)
  testthat::expect_gt(results$n_miss, 0)
  
  # Test for `by = "quarter"`
  results <- eval_dates(dates, rec_start, rec_end, by = "quarter")
  testthat::expect_lt(results$pct_complete, 100)
  testthat::expect_gt(results$n_miss, 0)
  
  # Test for `by = "year"`
  results <- eval_dates(dates, rec_start, rec_end, by = "year")
  testthat::expect_lt(results$pct_complete, 100)
  testthat::expect_gt(results$n_miss, 0)
  
})

testthat::test_that("eval_dates handles NA dates correctly", {
  
  dates <- seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day")
  dates[c(10, 20, 30)] <- NA
  rec_start <- as.Date("2020-01-01")
  rec_end <- as.Date("2020-12-31")
  
  # Test for `by = "day"`
  results <- eval_dates(dates, rec_start, rec_end, by = "day")
  testthat::expect_lt(results$pct_complete, 100)
  testthat::expect_gt(results$n_miss, 0)
  
})

testthat::test_that("eval_dates handles errors correctly", {
  
  dates <- seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day")
  rec_start <- as.Date("2020-01-01")
  rec_end <- as.Date("2020-12-31")
  
  # Test for incorrect `by` parameter
  testthat::expect_error(
    eval_dates(dates, rec_start, rec_end, by = "hour"), 
    "'by' must be one of: day, week, month, quarter, year"
  )
  
  # Test for non-Date class dates
  testthat::expect_error(
    eval_dates(as.character(dates), rec_start, rec_end, by = "day"), 
    "Input 'dates' must be 'Date' class"
  )
  
  # Test for non-Date class in rec_start and rec_end
  testthat::expect_error(
    eval_dates(dates, as.character(rec_start), rec_end, by = "day"), 
    "Input 'rec_start' and 'rec_end' must be 'Date' class"
  )
  
  # Test for non-Date class in rec_start and rec_end
  testthat::expect_error(
    eval_dates(dates, rec_start, as.character(rec_end), by = "day"), 
    "Input 'rec_start' and 'rec_end' must be 'Date' class"
  )
  
})

testthat::test_that("input checking works correctly", {
  
  # Test valid inputs
  valid_dates <- as.Date(c("2020-01-01", "2020-02-01"))
  valid_rec_start <- as.Date("2020-01-01")
  valid_rec_end <- as.Date("2020-12-31")
  valid_by <- "month"
  
  testthat::expect_false(
    check_eval_dates_inputs(valid_dates, valid_rec_start, valid_rec_end, valid_by)
  )
  
  # Test empty dates
  testthat::expect_error(
    eval_dates(numeric(0), valid_rec_start, valid_rec_end, valid_by), 
    "No non-NA 'dates'"
  )
  
  # Test all NA dates
  testthat::expect_error(
    eval_dates(c(NA, NA), valid_rec_start, valid_rec_end, valid_by), 
    "No non-NA 'dates'"
  )
  
  # Test invalid dates class
  testthat::expect_error(
    eval_dates(c("2020-01-01", "2020-02-01"), valid_rec_start, valid_rec_end, valid_by), 
    "Input 'dates' must be 'Date' class"
  )
  
  # Test invalid rec_start and rec_end class
  testthat::expect_error(
    eval_dates(valid_dates, "2020-01-01", valid_rec_end, valid_by), 
    "Input 'rec_start' and 'rec_end' must be 'Date' class"
  )
  testthat::expect_error(
    eval_dates(valid_dates, valid_rec_start, "2020-12-31", valid_by), 
    "Input 'rec_start' and 'rec_end' must be 'Date' class"
  )
  
  # Test invalid length of by
  testthat::expect_error(
    eval_dates(valid_dates, valid_rec_start, valid_rec_end, c("month", "week")), 
    "Input 'by' must be of length 1"
  )
  
  # Test invalid class of by
  testthat::expect_error(
    eval_dates(valid_dates, valid_rec_start, valid_rec_end, 5), 
    "Input 'by' must be 'character' class"
  )
  
  # Test invalid value of by
  testthat::expect_error(
    eval_dates(valid_dates, valid_rec_start, valid_rec_end, "inv_int"), 
    "Input 'by' must be one of: day, week, month, quarter, year"
  )
  
})