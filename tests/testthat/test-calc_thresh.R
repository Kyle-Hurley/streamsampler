testthat::test_that("treshold data frame returned", {
  
  values <- c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.1)
  seasons <- rep(1:2, each = 5)
  years <- 2010:2019
  center_year <- 2015
  
  results <- calc_thresh(
    values = values, 
    seasons = seasons, 
    years = years, 
    center_year = center_year, 
    half_win = 2, 
    threshold = 0.8
  )
  
  testthat::expect_true(is.data.frame(results))
  
})


testthat::test_that("calc_thresh calculates thresholds correctly for valid inputs", {
  
  values <- c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.1)
  seasons <- rep(1:2, each = 5)
  years <- 2010:2019
  center_year <- 2015
  
  results <- calc_thresh(
    values = values, 
    seasons = seasons, 
    years = years, 
    center_year = center_year, 
    half_win = 2, 
    threshold = 0.8
  )
  
  expected_result <- data.frame(
    season = 1:2,
    threshold = c(
      stats::quantile(c(4.4, 5.5), 0.8)[[1]], 
      stats::quantile(c(6.6, 7.7, 8.8), 0.8)[[1]]
    ),
    center_year = 2015
  )
  
  testthat::expect_equal(results, expected_result)
  
})

testthat::test_that("calc_thresh handles window range greater than min or max year", {
  
  values <- c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.1)
  seasons <- rep(1:2, each = 5)
  years <- 2010:2019
  center_year <- 2015
  
  testthat::expect_warning(
    calc_thresh(
      values = values, 
      seasons = seasons, 
      years = years, 
      center_year = center_year, 
      half_win = 10, 
      threshold = 0.8
    ), 
    "Window exceeds range of years. Results are for all 'values'."
  )
  
  results <- suppressWarnings(
    calc_thresh(
      values = values, 
      seasons = seasons, 
      years = years, 
      center_year = center_year, 
      half_win = 10, 
      threshold = 0.8
    )
  )
  
  expected_result <- data.frame(
    season = 1:2,
    threshold = c(
      stats::quantile(c(1.1, 2.2, 3.3, 4.4, 5.5), 0.8)[[1]], 
      stats::quantile(c(6.6, 7.7, 8.8, 9.9, 10.1), 0.8)[[1]]
    ),
    center_year = 2015
  )
  
  testthat::expect_equal(results, expected_result)
  
})

testthat::test_that("calc_thresh handles different threshold correctly", {
  
  values <- c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.1)
  seasons <- rep(1:2, each = 5)
  years <- 2010:2019
  center_year <- 2015
  
  results <- calc_thresh(
    values = values, 
    seasons = seasons, 
    years = years, 
    center_year = center_year, 
    half_win = 2, 
    threshold = 0.5
  )
  
  expected_result <- data.frame(
    season = 1:2,
    threshold = c(
      stats::quantile(c(4.4, 5.5), 0.5)[[1]], 
      stats::quantile(c(6.6, 7.7, 8.8), 0.5)[[1]]
    ),
    center_year = 2015
  )
  
  testthat::expect_equal(results, expected_result)
  
})

testthat::test_that("calc_thresh handles edge cases with single year correctly", {
  
  values <- c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.1)
  seasons <- rep(1:2, each = 5)
  years <- rep(2010:2011, 5)
  center_year <- 2010
  
  results <- calc_thresh(
    values = values, 
    seasons = seasons, 
    years = years, 
    center_year = center_year, 
    half_win = 0, 
    threshold = 0.8
  )
  
  expected_result <- data.frame(
    season = c(1, 2),
    threshold = c(
      stats::quantile(c(1.1, 3.3, 5.5), 0.8)[[1]], 
      stats::quantile(c(7.7, 9.9), 0.8)[[1]]
    ),
    center_year = 2010
  )
  
  testthat::expect_equal(results, expected_result)
  
})

testthat::test_that("calc_thresh handles negative half_win correctly", {
  
  values <- c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.1)
  seasons <- rep(1:2, each = 5)
  years <- 2010:2019
  center_year <- 2015
  
  results <- calc_thresh(
    values = values, 
    seasons = seasons, 
    years = years, 
    center_year = center_year, 
    half_win = -2, 
    threshold = 0.8
  )
  
  expected_result <- data.frame(
    season = 1:2,
    threshold = c(
      stats::quantile(c(4.4, 5.5), 0.8)[[1]], 
      stats::quantile(c(6.6, 7.7, 8.8), 0.8)[[1]]
    ),
    center_year = 2015
  )
  
  testthat::expect_equal(results, expected_result)
  
})