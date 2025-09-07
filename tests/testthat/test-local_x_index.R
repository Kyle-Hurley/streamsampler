test_that("local_max and min_index work with default input", {
  
  date_vec <- seq.Date(as.Date("2020-01-01"), as.Date("2020-01-07"), by = "day")
  vals <- c(1, 2, 1, 1, 2, 1, 1)
  
  max_output <- local_max_index(dates = date_vec, values = vals)
  min_output <- local_min_index(dates = date_vec, values = vals)
  
  testthat::expect_equal(
    max_output, 
    c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)
  )
  testthat::expect_equal(
    min_output, 
    c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE)
  )
  
})

test_that("local_max and min_index give errors", {
  
  date_vec <- seq.Date(as.Date("2020-01-01"), as.Date("2020-01-07"), by = "day")
  vals <- c(1, 2, 1, 1, 2, 1, 1)
  
  # Dates
  testthat::expect_error(
    local_max_index(
      as.Date(numeric(0)), 
      values = vals
    ), 
    "Input 'dates' cannot have NA values or length 0"
  )
  testthat::expect_error(
    local_min_index(
      as.Date(numeric(0)), 
      values = vals
    ), 
    "Input 'dates' cannot have NA values or length 0"
  )
  testthat::expect_error(
    local_max_index(
      dates = c(date_vec[c(1, 2, 3)], NA, date_vec[c(5, 6, 7)]), 
      values = vals
    ), 
    "Input 'dates' cannot have NA values or length 0"
  )
  testthat::expect_error(
    local_min_index(
      dates = c(date_vec[c(1, 2, 3)], NA, date_vec[c(5, 6, 7)]), 
      values = vals
    ), 
    "Input 'dates' cannot have NA values or length 0"
  )
  
  # Values
  testthat::expect_error(
    local_max_index(
      dates = date_vec, 
      values = rep(NA, 7)
    ), 
    "No non-NA 'values'"
  )
  testthat::expect_error(
    local_min_index(
      dates = date_vec, 
      values = rep(NA, 7)
    ), 
    "No non-NA 'values'"
  )
  testthat::expect_error(
    local_max_index(
      dates = date_vec, 
      values = vector("numeric", 0)
    ), 
    "No non-NA 'values'"
  )
  testthat::expect_error(
    local_min_index(
      dates = date_vec, 
      values = vector("numeric", 0)
    ), 
    "No non-NA 'values'"
  )
  
  # Look
  testthat::expect_error(
    local_max_index(
      dates = date_vec, 
      values = vals, 
      look_behind = "a", 
      look_ahead = 1
    ), 
    "Inputs 'look_behind' and 'look_ahead' must be class 'numeric'"
  )
  testthat::expect_error(
    local_min_index(
      dates = date_vec, 
      values = vals, 
      look_behind = "a", 
      look_ahead = 1
    ), 
    "Inputs 'look_behind' and 'look_ahead' must be class 'numeric'"
  )
  testthat::expect_error(
    local_max_index(
      dates = date_vec, 
      values = vals, 
      look_behind = 1, 
      look_ahead = "a"
    ), 
    "Inputs 'look_behind' and 'look_ahead' must be class 'numeric'"
  )
  testthat::expect_error(
    local_min_index(
      dates = date_vec, 
      values = vals, 
      look_behind = 1, 
      look_ahead = "a"
    ), 
    "Inputs 'look_behind' and 'look_ahead' must be class 'numeric'"
  )
  testthat::expect_error(
    local_max_index(
      dates = date_vec, 
      values = vals, 
      look_behind = 1, 
      look_ahead = 1, 
      look_units = "years"
    ), 
    "Input 'look_units' must be one of: days, weeks, months"
  )
  testthat::expect_error(
    local_min_index(
      dates = date_vec, 
      values = vals, 
      look_behind = 1, 
      look_ahead = 1, 
      look_units = "years"
    ), 
    "Input 'look_units' must be one of: days, weeks, months"
  )
  
})