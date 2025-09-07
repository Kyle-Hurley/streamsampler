testthat::test_that("eval_sign calculates pos and neg counts and percentages correctly", {
  
  data <- c(-1, -2, 0, 1, 2, 3, -3, -4, 4)
  
  result <- eval_sign(data)
  
  expected_result <- list(
    n_pos = 4,
    pos_pct = (4 / 9) * 100,
    n_neg = 5,
    neg_pct = (5 / 9) * 100
  )
  
  testthat::expect_equal(result$n_pos, expected_result$n_pos)
  testthat::expect_equal(result$pos_pct, expected_result$pos_pct)
  testthat::expect_equal(result$n_neg, expected_result$n_neg)
  testthat::expect_equal(result$neg_pct, expected_result$neg_pct)
  
})

testthat::test_that("eval_sign handles all positive data correctly", {
  
  data <- c(1, 2, 3, 4, 5)
  
  result <- eval_sign(data)
  
  expected_result <- list(
    n_pos = 5,
    pos_pct = 100,
    n_neg = 0,
    neg_pct = 0
  )
  
  testthat::expect_equal(result$n_pos, expected_result$n_pos)
  testthat::expect_equal(result$pos_pct, expected_result$pos_pct)
  testthat::expect_equal(result$n_neg, expected_result$n_neg)
  testthat::expect_equal(result$neg_pct, expected_result$neg_pct)
  
})

testthat::test_that("eval_sign handles all negative data correctly", {
  
  data <- c(-1, -2, -3, -4, -5)
  
  result <- eval_sign(data)
  
  expected_result <- list(
    n_pos = 0,
    pos_pct = 0,
    n_neg = 5,
    neg_pct = 100
  )
  
  testthat::expect_equal(result$n_pos, expected_result$n_pos)
  testthat::expect_equal(result$pos_pct, expected_result$pos_pct)
  testthat::expect_equal(result$n_neg, expected_result$n_neg)
  testthat::expect_equal(result$neg_pct, expected_result$neg_pct)
  
})

testthat::test_that("eval_sign handles empty data correctly", {
  
  data <- numeric(0)
  
  result <- eval_sign(data)
  
  expected_result <- list(
    n_pos = 0,
    pos_pct = NaN,
    n_neg = 0,
    neg_pct = NaN
  )
  
  testthat::expect_equal(result$n_pos, expected_result$n_pos)
  testthat::expect_equal(result$pos_pct, expected_result$pos_pct)
  testthat::expect_equal(result$n_neg, expected_result$n_neg)
  testthat::expect_equal(result$neg_pct, expected_result$neg_pct)
  
})

testthat::test_that("eval_sign handles data with no negative values correctly", {
  
  data <- c(0, 1, 2, 3, 4, 5)
  
  result <- eval_sign(data)
  
  expected_result <- list(
    n_pos = 5,
    pos_pct = (5 / 6) * 100,
    n_neg = 1,
    neg_pct = (1 / 6) * 100
  )
  
  testthat::expect_equal(result$n_pos, expected_result$n_pos)
  testthat::expect_equal(result$pos_pct, expected_result$pos_pct)
  testthat::expect_equal(result$n_neg, expected_result$n_neg)
  testthat::expect_equal(result$neg_pct, expected_result$neg_pct)
  
})

testthat::test_that("eval_sign handles data with no positive values correctly", {
  
  data <- c(-5, -4, -3, -2, -1)
  
  result <- eval_sign(data)
  
  expected_result <- list(
    n_pos = 0,
    pos_pct = 0,
    n_neg = 5,
    neg_pct = 100
  )
  
  testthat::expect_equal(result$n_pos, expected_result$n_pos)
  testthat::expect_equal(result$pos_pct, expected_result$pos_pct)
  testthat::expect_equal(result$n_neg, expected_result$n_neg)
  testthat::expect_equal(result$neg_pct, expected_result$neg_pct)
  
})

testthat::test_that("eval_sign gives error for non-numeric", {
  
  testthat::expect_error(
    eval_sign(c("a", "b", "c")), 
    "Input 'values' must be 'numeric' class"
  )
  
})