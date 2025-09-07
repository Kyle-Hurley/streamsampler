testthat::test_that("calc_seasons returns correct seasons for default parameters", {
  
  dates <- as.Date(c("2020-01-01", "2020-03-01", "2020-06-01", "2020-09-01", "2020-12-01"))
  
  results <- calc_seasons(dates)
  
  expected_results <- c(2, 2, 3, 4, 1)
  names(expected_results) <- c("01", "03", "06", "09", "12")
  
  testthat::expect_equal(results, expected_results)
  
})

testthat::test_that("calc_seasons returns correct seasons with n_seasons = 2", {
  
  dates <- as.Date(c("2020-01-01", "2020-06-01", "2020-07-01", "2020-12-01"))
  
  results <- calc_seasons(dates, n_seasons = 2)
  
  expected_results <- c(1, 2, 2, 1)
  names(expected_results) <- c("01", "06", "07", "12")
  
  testthat::expect_equal(results, expected_results)

})

testthat::test_that("calc_seasons returns correct seasons with season_start = 1", {
  
  dates <- as.Date(c("2020-01-01", "2020-03-01", "2020-07-01", "2020-09-01", "2020-12-01"))
  
  results <- calc_seasons(dates, season_start = 1)
  
  expected_results <- c(1, 1, 3, 3, 4)
  names(expected_results) <- c("01", "03", "07", "09", "12")
  
  testthat::expect_equal(results, expected_results)
  
})

testthat::test_that("calc_seasons handles a full year correctly", {
  
  dates <- seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "month")
  
  results <- calc_seasons(dates, season_start = 1)
  
  expected_results <- rep(1:4, each = 3)
  names(expected_results) <- c(
    "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"
  )
  
  testthat::expect_equal(results, expected_results)
  
})

testthat::test_that("calc_seasons handles edge cases correctly", {
  
  dates <- as.Date(c("2020-12-31", "2021-01-01"))
  
  results <- calc_seasons(dates)
  
  expected_results <- c(1, 2)
  names(expected_results) <- c("12", "01")
  
  testthat::expect_equal(results, expected_results)
  
})

testthat::test_that(
  "calc_seasons returns correct seasons with n_seasons = 3 and season_start = 4", 
  {
  
  dates <- as.Date(c("2020-01-01", "2020-04-01", "2020-08-01", "2020-12-01"))
  
  results <- calc_seasons(dates, n_seasons = 3, season_start = 4)
  
  expected_results <- c(3, 1, 2, 3)
  names(expected_results) <- c("01", "04", "08", "12")
  
  testthat::expect_equal(results, expected_results)
  
})
