testthat::test_that("subsample_routine works with intended input", {
  
  date_sequence <- seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day")
  set.seed(123)
  values_sequence <- rnorm(length(date_sequence))
  
  result <- subsample_routine(date_sequence, values_sequence)
  
  testthat::expect_type(result, "list")
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(dim(result), c(365, 3))
  
  result <- subsample_routine(date_sequence, values_sequence, day = 1, freq = "week")
  
  testthat::expect_type(result, "list")
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(dim(result), c(365, 3))
  
  result <- subsample_routine(date_sequence, values_sequence, day = 30, freq = "day")
  
  testthat::expect_type(result, "list")
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(dim(result), c(365, 3))
  
})

testthat::test_that("subsample_routine selects correct dates for monthly frequency", {
  
  date_sequence <- seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day")
  set.seed(123)
  values_sequence <- rnorm(length(date_sequence))
  
  result <- subsample_routine(date_sequence, values_sequence, day = 15, freq = "month")
  
  selected_dates <- result[result$selection_type == "routine", "date"]
  expected_dates <- seq.Date(as.Date("2023-12-15"), as.Date("2023-01-15"), by = "-1 month") |> 
    rev()
  
  # Check that the dates are the 15th of each month
  testthat::expect_equal(selected_dates, expected_dates)
  
})

testthat::test_that("subsample_routine selects correct dates for weekly frequency", {
  
  date_sequence <- seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day")
  set.seed(123)
  values_sequence <- rnorm(length(date_sequence))
  
  result <- subsample_routine(date_sequence, values_sequence, day = 1, freq = "week")
  
  # Check that the dates are the first day of each week
  selected_dates <- result[result$selection_type == "routine", "date"]
  expected_dates <- seq(as.Date("2023-01-01"), by = "week", length.out = 53)
  testthat::expect_equal(selected_dates, expected_dates)
  
  # When 1st date is not a sunday
  date_sequence <- seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day")
  result <- subsample_routine(date_sequence, values_sequence, day = 1, freq = "week")
  selected_dates <- result[result$selection_type == "routine", "date"]
  expected_dates <- seq(as.Date("2025-01-05"), as.Date("2025-12-28"), by = "week")
  testthat::expect_equal(selected_dates, expected_dates)
  
  # All dates are same day of week
  selected_dow <- weekdays(selected_dates)
  expected_dow <- weekdays(expected_dates)
  testthat::expect_equal(selected_dow, expected_dow)
  
})

testthat::test_that("subsample_routine handles invalid days in month frequency", {
  
  date_sequence <- seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day")
  set.seed(123)
  values_sequence <- rnorm(length(date_sequence))
  
  testthat::expect_warning(
    subsample_routine(date_sequence, values_sequence, day = 30, freq = "month"), 
    "There are fewer than 29 days in each month. Invalid days of the month are counted forward into the next month."
  )
  
  result <- suppressWarnings(
    subsample_routine(date_sequence, values_sequence, day = 30, freq = "month")
  )
  
  # Check that the dates are the last day of each month
  selected_dates <- result[result$selection_type == "routine", "date"]
  expected_dates <- as.Date(
    c(
      "2023-01-30", "2023-03-02", "2023-03-30", "2023-04-30", 
      "2023-05-30", "2023-06-30", "2023-07-30", "2023-08-30", 
      "2023-09-30", "2023-10-30", "2023-11-30", "2023-12-30"
    )
  )
  testthat::expect_equal(selected_dates, expected_dates)
  
})

testthat::test_that("subsample_routine gives errors", {
  
  testthat::expect_error(
    subsample_routine(
      c("2023-01-01", "2023-01-02"), 
      c(1, 2)
    ), 
    "Input 'dates' must be 'Date' class"
  )
  
  testthat::expect_error(
    subsample_routine(
      as.Date(c("2023-01-01", "2023-01-02")), 
      c("a", "b")
    ),
  "Input 'values' must be 'numeric' or 'integer' class")
  
  testthat::expect_error(
    subsample_routine(
      as.Date(c("2023-01-01", "2023-01-02")), 
      numeric(0)
    ), "No non-NA 'values'"
  )
  
  testthat::expect_error(
    subsample_routine(
      as.Date(c("2023-01-01")), 
      c(1, 2)
    ),
    "Input 'dates', 'values', and 'thresh_ref' must be the same length"
  )
  
  testthat::expect_error(
    subsample_routine(
      as.Date(c("2023-01-01", "2023-01-02")), 
      c(1, 2), 
      freq = "invalid"
    ),
    "Input 'freq' must be one of: day, week, month"
  )
  
  testthat::expect_error(
    subsample_routine(
      as.Date(c("2023-01-01", "2023-01-02")), 
      c(1, 2), 
      freq = 123),
    "Input 'freq' must be 'character' class"
  )
  
  testthat::expect_error(
    subsample_routine(
      as.Date(c("2023-01-01", "2023-01-02")), 
      c(1, 2), 
      day = -1, 
      freq = "day"
    ),
    "Input 'day' must be an integer equal to or greater than 1"
  )
  
  testthat::expect_error(
    subsample_routine(
      as.Date(c("2023-01-01", "2023-01-02")), 
      c(1, 2), 
      day = 8, 
      freq = "week"
    ),
    "Input 'day' must be an integer between 1 and 7"
  )
  
  testthat::expect_error(
    subsample_routine(
      as.Date(c("2023-01-01", "2023-01-02")), 
      c(1, 2), 
      day = 32, 
      freq = "month"
    ),
    "Input 'day' must be an integer between 1 and 31"
  )
  
})