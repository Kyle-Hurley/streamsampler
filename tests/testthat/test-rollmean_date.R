testthat::test_that("rollmean_date calculates rolling mean correctly with default look_back", {
  
  df <- data.frame(
    date_col = as.Date('2020-01-01') + 0:30,
    var_col = 1:31
  )
  
  result <- rollmean_date(dates = df$date_col, values = df$var_col)
  expected_result <- c(1.0, 1.5, seq(2.0, 30.0))
  
  testthat::expect_equal(result, expected_result)
  
})

testthat::test_that("rollmean_date handles look_behind = 5 correctly", {
  
  df <- data.frame(
    date_col = as.Date('2020-01-01') + 0:10,
    var_col = 1:11
  )
  
  result <- rollmean_date(dates = df$date_col, values = df$var_col, look_behind = 5)
  
  expected_result <- c(seq(1.0, 3.5, by = 0.5), seq(4.5, 8.5))
  
  testthat::expect_equal(result, expected_result)
  
})

testthat::test_that("rollmean_date handles missing values correctly", {
  
  df <- data.frame(
    date_col = as.Date('2020-01-01') + 0:10,
    var_col = c(1:5, NA, 7:11)
  )
  
  result <- rollmean_date(dates = df$date_col, values = df$var_col, look_behind = 5)
  
  expected_result <- c(seq(1.0, 3.0, by = 0.5), seq(3.0, 9.0, by = 1.2))
  
  testthat::expect_equal(result, expected_result)
  
})

testthat::test_that("rollmean_date handles non-default ordering of dates correctly", {
  
  df <- data.frame(
    date_col = as.Date(c('2020-01-01', '2020-01-03', '2020-01-02', '2020-01-05', '2020-01-04')),
    var_col = c(1, 3, 2, 5, 4)
  )
  
  testthat::expect_error(
    rollmean_date(dates = df$date_col, values = df$var_col), 
    "Input 'dates' must be in ascending order"
  )
  
})

testthat::test_that("rollmean_date handles empty data correctly", {
  
  df <- data.frame(
    date_col = as.Date(character(0)),
    var_col = numeric(0)
  )
  
  testthat::expect_error(
    rollmean_date(dates = df$date_col, values = df$var_col), 
    "No non-NA 'values'"
  )
  
})