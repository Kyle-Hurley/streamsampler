testthat::test_that("summarize_seasons identifies correct seasons", {
  
  dates <- seq.Date(
   from = as.Date("2020-10-01"), 
   to = as.Date("2021-09-30"), 
   by = "month"
  )
  values <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120)
  
  results <- summarize_seasons(
    dates = dates, 
    values = values, 
    season_start = 10, 
    n_seasons = 4
  )
  testthat::expect_equal(results$seasonal$season, 1:4)
  
})

testthat::test_that("summarize_seasons handles valid inputs correctly", {
  
  dates <- as.Date(
    c("2020-01-01", "2020-02-01", "2020-03-01", "2020-10-01", "2020-11-01", "2020-12-01")
  )
  values <- c(1.2, 2.3, 3.1, 4.5, 5.6, 6.7)
  
  results <- summarize_seasons(
    dates = dates, 
    values = values, 
    season_start = 1, n_seasons = 2
  )
  
  # Check the structure of the returned list
  testthat::expect_named(results, c("monthly", "seasonal"))
  
  # Check the structure of the monthly data
  expected_monthly <- data.frame(
    year = c(2020, 2020, 2020, 2020, 2020, 2020),
    month = c(1, 2, 3, 10, 11, 12),
    avg_value = c(1.2, 2.3, 3.1, 4.5, 5.6, 6.7)
  )
  testthat::expect_equal(results$monthly, expected_monthly)
  
  # Check the structure of the seasonal data
  expected_seasonal <- data.frame(
    adj_year = c(2020, 2020),
    season = c(1, 2),
    avg_value = c(2.2, 5.6),
    ys_rank = c(2, 1)
  )
  testthat::expect_equal(results$seasonal, expected_seasonal)
  
})

testthat::test_that("summarize_seasons handles n_seasons = 1 and season_start = 3 correctly", {
  
  dates <- as.Date(
    c("2020-01-01", "2020-02-01", "2020-03-01", "2020-10-01", "2020-11-01", "2020-12-01")
  )
  values <- c(1.2, 2.3, 3.1, 4.5, 5.6, 6.7)
  
  results <- summarize_seasons(
    dates = dates, 
    values = values, 
    season_start = 3, n_seasons = 1
  )
  
  # Check the structure of the returned list
  testthat::expect_named(results, c("monthly", "seasonal"))
  
  # Check the structure of the monthly data
  expected_monthly <- data.frame(
    year = c(2020, 2020, 2020, 2020, 2020, 2020),
    month = c(1, 2, 3, 10, 11, 12),
    avg_value = c(1.2, 2.3, 3.1, 4.5, 5.6, 6.7)
  )
  testthat::expect_equal(results$monthly, expected_monthly)
  
  # Check the structure of the seasonal data
  expected_seasonal <- data.frame(
    adj_year = c(2020, 2021),
    season = c(1, 1),
    avg_value = c(1.75, 4.975),
    ys_rank = c(1, 1)
  )
  testthat::expect_equal(results$seasonal, expected_seasonal)
  
})

testthat::test_that("input checking works correctly", {
  
  # Test valid inputs
  valid_dates <- as.Date(c("2020-01-01", "2020-02-01"))
  valid_values <- c(10, 20)
  testthat::expect_false(
    check_summarize_seasons_inputs(
      valid_dates,
      valid_values,
      season_start = 1,
      n_seasons = 4
    )
  )
  
  # Test empty values
  testthat::expect_error(
    summarize_seasons(
      valid_dates,
      numeric(0),
      season_start = 1,
      n_seasons = 4
    ),
    "No non-NA 'values'"
  )
  
  # Test all NA values
  testthat::expect_error(
    summarize_seasons(
      valid_dates,
      c(NA, NA),
      season_start = 1,
      n_seasons = 4
    ),
    "No non-NA 'values'"
  )
  
  # Test empty dates
  testthat::expect_error(
    summarize_seasons(
      numeric(0),
      valid_values,
      season_start = 1,
      n_seasons = 4
    ),
    "No non-NA 'dates'"
  )
  
  # Test all NA dates
  testthat::expect_error(
    summarize_seasons(
      c(NA, NA),
      valid_values,
      season_start = 1,
      n_seasons = 4
    ),
    "No non-NA 'dates'"
  )
  
  # Test invalid dates class
  testthat::expect_error(
    summarize_seasons(
      as.POSIXct(c("2020-01-01", "2020-02-01")),
      valid_values,
      season_start = 1,
      n_seasons = 4
    ), 
    "Input 'dates' must be 'Date' class"
  )
  
  # Test invalid values class
  testthat::expect_error(
    summarize_seasons(
      valid_dates,
      c("10", "20"),
      season_start = 1,
      n_seasons = 4
    ),
    "Input 'values' must be 'numeric' or 'integer' class"
  )
  
  # Test invalid season start
  testthat::expect_error(
    summarize_seasons(
      valid_dates,
      valid_values,
      season_start = 0,
      n_seasons = 4
    ),
    "Input 'season_start' must be an integer between 1 and 12"
  )
  
  testthat::expect_error(
    summarize_seasons(
      valid_dates,
      valid_values,
      season_start = 13,
      n_seasons = 4
    ),
    "Input 'season_start' must be an integer between 1 and 12"
  )
  
  testthat::expect_error(
    summarize_seasons(
      valid_dates,
      valid_values,
      season_start = 2.99,
      n_seasons = 4
    ),
    "Input 'season_start' must be an integer between 1 and 12"
  )
  
  # Test invalid number of seasons
  testthat::expect_error(
    summarize_seasons(
      valid_dates,
      valid_values,
      season_start = 1,
      n_seasons = 5
    ),
    "Input 'n_seasons' must be an integer between 1 and 12"
  )
  
  testthat::expect_error(
    summarize_seasons(
      valid_dates,
      valid_values,
      season_start = 1,
      n_seasons = 13
    ),
    "Input 'n_seasons' must be an integer between 1 and 12"
  )
  
  testthat::expect_error(
    summarize_seasons(
      valid_dates,
      valid_values,
      season_start = 1,
      n_seasons = 1.99
    ),
    "Input 'n_seasons' must be an integer between 1 and 12"
  )
  
})