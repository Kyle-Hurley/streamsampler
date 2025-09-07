testthat::test_that("find_gaps handles proper Date class input correctly", {
  
  dates <- as.Date(c("2020-01-01", "2020-01-03", "2020-01-04", "2020-01-10", "2020-01-15"))
  
  result <- find_gaps(dates)
  
  expected_gaps <- data.frame(
    n_days = c(5, 4, 1),
    start = as.Date(c("2020-01-05", "2020-01-11", "2020-01-02")),
    end = as.Date(c("2020-01-09", "2020-01-14", "2020-01-02")),
    location = c("4", "5", "2")
  )
  row.names(expected_gaps) <- 1:3
  
  testthat::expect_equal(result, expected_gaps)
  
})

testthat::test_that("find_gaps handles no gaps correctly", {
  
  dates <- seq.Date(as.Date("2020-01-01"), as.Date("2020-01-05"), by = "day")
  
  result <- find_gaps(dates)
  
  expected_gaps <- data.frame(
    n_days = integer(0),
    start = as.Date(character(0)),
    end = as.Date(character(0)),
    location = character(0)
  )
  
  testthat::expect_equal(result, expected_gaps)
  
})

testthat::test_that("find_gaps handles NA values in dates correctly", {
  
  dates <- as.Date(c("2020-01-01", "2020-01-03", NA, "2020-01-10"))
  
  result <- find_gaps(dates)
  
  expected_gaps <- data.frame(
    n_days = c(6, 1),
    start = as.Date(c("2020-01-04", "2020-01-02")),
    end = as.Date(c("2020-01-09", "2020-01-02")),
    location = c("3", "2")
  )
  row.names(expected_gaps) <- 1:2
  
  testthat::expect_equal(result, expected_gaps)
  
})

testthat::test_that("find_gaps handles error for non-Date input", {
  
  dates <- c("2020-01-01", "2020-01-03", "2020-01-04", "2020-01-10")
  
  testthat::expect_error(find_gaps(dates), "Input 'dates' must be 'Date' class")
  
})

testthat::test_that("find_gaps gives warning for 1 date", {
  
  dates <- as.Date("2020-01-01")
  
  testthat::expect_warning(
    find_gaps(dates), 
    "Input 'dates' has length 1 or fewer. Returning NA."
  )
  testthat::expect_equal(
    suppressWarnings(find_gaps(dates)), 
    data.frame(
      n_days = NA, start = NA, end = NA, location = NA
    )
  )
  
})