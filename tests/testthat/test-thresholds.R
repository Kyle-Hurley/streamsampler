test_that("thresholds works with expected input", {
  
  date_vec <- seq.Date(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "day")
  vals <- sample(20:2000, length(date_vec), replace = TRUE)
  
  thresh <- thresholds(
    dates =  date_vec, 
    values = vals
  )
  
  testthat::expect_type(thresh, "list")
  testthat::expect_s3_class(thresh, "data.frame")
  testthat::expect_equal(dim(thresh), c(4, 3))
  
})

test_that("thresholds gives errors", {
  
  date_vec <- NA
  vals <- sample(20:2000, length(date_vec), replace = TRUE)
  
  testthat::expect_error(
    thresholds(
      dates =  date_vec, 
      values = vals
    ), 
    "No non-NA 'dates'"
  )
  
  
  
})