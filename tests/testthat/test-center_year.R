testthat::test_that("center_year handles years within the range correctly", {
  
  year <- 2000:2010
  
  results <- center_year(year, start_year = 2000, end_year = 2010, half_win = 2)
  expected_result <- c(2002, 2002, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2008, 2008)
  
  testthat::expect_equal(results, expected_result)
  
})

testthat::test_that("center_year handles years below the start range correctly", {
  
  year <- 2005:2010
  
  results <- suppressWarnings(center_year(year, start_year = 2000, end_year = 2010, half_win = 2))
  expected_result <- c(2005, 2006, 2007, 2008, 2008, 2008)
  
  testthat::expect_equal(results, expected_result)
  testthat::expect_warning(
    center_year(year, start_year = 2000, end_year = 2010, half_win = 1), 
    "Start and end year outside year range. Unexpected behavior may occur."
  )
  
})

testthat::test_that("center_year handles years above the end range correctly", {
  
  year <- 2000:2005
  
  results <- suppressWarnings(center_year(year, start_year = 2000, end_year = 2010, half_win = 2))
  expected_result <- c(2002, 2002, 2002, 2003, 2004, 2005)
  
  testthat::expect_equal(results, expected_result)
  testthat::expect_warning(
    center_year(year, start_year = 2000, end_year = 2010, half_win = 1), 
    "Start and end year outside year range. Unexpected behavior may occur."
  )
  
})

testthat::test_that("center_year handles edge cases correctly", {
  
  year <- 2000:2010
  
  results <- center_year(year, start_year = 2000, end_year = 2010, half_win = 1)
  expected_result <- c(2001, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2009)
  
  testthat::expect_equal(results, expected_result)
  
})

testthat::test_that("center_year handles single year input correctly", {
  
  year <- 2010
  
  results <- center_year(year, start_year = 2000, end_year = 2020, half_win = 2)
  expected_result <- 2010
  
  testthat::expect_equal(results, expected_result)
  
})

testthat::test_that("center_year handles half_win larger than year range", {
  
  year <- 2000:2005
  
  results <- suppressWarnings(center_year(year, start_year = 2000, end_year = 2005, half_win = 10))
  expected_result <- year
  
  testthat::expect_equal(results, expected_result)
  testthat::expect_warning(
    center_year(year, start_year = 2000, end_year = 2005, half_win = 10), 
    "Half window is larger than year range. Try a smaller half_win. Returning year vector."
  )
  
})