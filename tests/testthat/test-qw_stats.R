testthat::test_that("qw_stats output data frame has correct structure", {
  
  test_df <- data.frame(
    date_col = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04", "2020-01-05")),
    var_col = c(1.2, 2.3, 3.1, NA, 4.5)
  )
  rec_start <- as.Date("2020-01-01")
  rec_end <- as.Date("2020-01-05")
  
  result <- qw_stats(test_df$date_col, test_df$var_col, rec_start, rec_end, by = "day")
  
  expected_result <- data.frame(
    rec_startDate = rec_start,
    rec_endDate = rec_end, 
    n_miss_dates = 0, 
    pct_complete_dates = 100, 
    value_startDate = as.Date("2020-01-01"),
    value_endDate = as.Date("2020-01-05"),
    n_value = 4,
    n_pos = 4,
    pos_pct = 80, 
    n_neg = 0, 
    neg_pct = 0, 
    n_na = 1, 
    na_pct = 20, 
    tot_pct = 100, 
    Min. = 1.2,
    Q1 = 2.025,
    Median = 2.7,
    Mean = 2.775,
    Q3 = 3.45,
    Max. = 4.5,
    std = 1.3889444,
    vari = 1.9291667
  )
  
  testthat::expect_true(is.data.frame(result))
  testthat::expect_equal(ncol(result), 22)
  testthat::expect_true(
    all(
      c(
        "rec_startDate", "rec_endDate", "n_miss_dates", "pct_complete_dates", "value_startDate", 
        "value_endDate", "n_value", "n_pos", "pos_pct", "n_neg", "neg_pct", "n_na", "na_pct", 
        "tot_pct", "Min.", "Q1", "Median", "Mean", "Q3", "Max.", "std", "vari"
      ) %in% names(result)
    )
  )
  testthat::expect_type(result$rec_startDate, "double")
  testthat::expect_type(result$rec_endDate, "double")
  testthat::expect_type(result$n_miss_dates, "integer")
  testthat::expect_type(result$pct_complete_dates, "double")
  testthat::expect_type(result$value_startDate, "double")
  testthat::expect_type(result$value_endDate, "double")
  testthat::expect_type(result$n_value, "integer")
  testthat::expect_type(result$n_pos, "integer")
  testthat::expect_type(result$pos_pct, "double")
  testthat::expect_type(result$n_neg, "integer")
  testthat::expect_type(result$neg_pct, "double")
  testthat::expect_type(result$n_na, "integer")
  testthat::expect_type(result$na_pct, "double")
  testthat::expect_type(result$Min., "double")
  testthat::expect_type(result$Q1, "double")
  testthat::expect_type(result$Median, "double")
  testthat::expect_type(result$Mean, "double")
  testthat::expect_type(result$Q3, "double")
  testthat::expect_type(result$Max., "double")
  testthat::expect_type(result$std, "double")
  testthat::expect_type(result$vari, "double")
  
})


testthat::test_that("qw_stats handles proper Date class input correctly", {
  
  test_df <- data.frame(
    date_col = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04", "2020-01-05")),
    var_col = c(1.2, 2.3, 3.1, NA, 4.5)
  )
  rec_start <- as.Date("2020-01-01")
  rec_end <- as.Date("2020-01-05")
  
  result <- qw_stats(test_df$date_col, test_df$var_col, rec_start, rec_end, by = "day")
  
  expected_result <- data.frame(
    rec_startDate = rec_start,
    rec_endDate = rec_end, 
    n_miss_dates = 0, 
    pct_complete_dates = 100, 
    value_startDate = as.Date("2020-01-01"),
    value_endDate = as.Date("2020-01-05"),
    n_value = 4,
    n_pos = 4,
    pos_pct = 80, 
    n_neg = 0, 
    neg_pct = 0, 
    n_na = 1, 
    na_pct = 20, 
    tot_pct = 100, 
    Min. = 1.2,
    Q1 = 2.025,
    Median = 2.7,
    Mean = 2.775,
    Q3 = 3.45,
    Max. = 4.5,
    std = 1.3889444,
    vari = 1.9291667
  )
  
  testthat::expect_equal(result, expected_result, tolerance = 1e-7)

})

testthat::test_that("qw_stats handles data with no missing values correctly", {
  
  test_df <- data.frame(
    date_col = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04", "2020-01-05")),
    var_col = c(1.2, 2.3, 3.1, 4.5, 5.6)
  )
  rec_start <- as.Date("2020-01-01")
  rec_end <- as.Date("2020-01-05")
  
  result <- qw_stats(test_df$date_col, test_df$var_col, rec_start, rec_end, by = "day")
  
  expected_result <- data.frame(
    rec_startDate = rec_start,
    rec_endDate = rec_end, 
    n_miss_dates = 0, 
    pct_complete_dates = 100, 
    value_startDate = as.Date("2020-01-01"),
    value_endDate = as.Date("2020-01-05"),
    n_value = 5,
    n_pos = 5,
    pos_pct = 100, 
    n_neg = 0, 
    neg_pct = 0, 
    n_na = 0, 
    na_pct = 0, 
    tot_pct = 100, 
    Min. = 1.2,
    Q1 = 2.3,
    Median = 3.1,
    Mean = 3.34,
    Q3 = 4.5,
    Max. = 5.6,
    std = 1.7444197,
    vari = 3.043
  )
  
  testthat::expect_equal(result, expected_result, tolerance = 1e-7)

})

testthat::test_that("qw_stats handles empty data correctly", {
  
  rec_start <- as.Date("2020-01-01")
  rec_end <- as.Date("2020-01-05")
  
  testthat::expect_error(
    qw_stats(
      as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04", "2020-01-05")), 
      numeric(0), 
      rec_start, rec_end, by = "day"
    ), 
    "No non-NA 'values'"
  )
  
  testthat::expect_error(
    qw_stats(
      as.Date(character(0)), 
      c(1.2, 2.3, 3.1, 4.5, 5.6), 
      rec_start, rec_end, by = "day"
    ), 
    "No non-NA 'dates'"
  )
  
})

testthat::test_that("qw_stats handles data with all NA values correctly", {
  
  test_df <- data.frame(
    date_col = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04", "2020-01-05")),
    var_col = c(NA, NA, NA, NA, NA)
  )
  rec_start <- as.Date("2020-01-01")
  rec_end <- as.Date("2020-01-05")
  
  testthat::expect_error(
    qw_stats(test_df$date_col, test_df$var_col, rec_start, rec_end, by = "day"), 
    "No non-NA 'values'"
  )
  
  test_df <- data.frame(
    date_col = as.Date(c(NA, NA, NA, NA, NA)),
    var_col = c(1.2, 2.3, 3.1, 4.5, 5.6)
  )
  
  testthat::expect_error(
    qw_stats(test_df$date_col, test_df$var_col, rec_start, rec_end, by = "day"), 
    "No non-NA 'dates'"
  )
  
  testthat::expect_error(
    qw_stats(
      as.character(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04", "2020-01-05")), 
      test_df$var_col, 
      rec_start, rec_end, by = "day"
    ), 
    "Input 'dates' must be 'Date' class"
  )
  
  testthat::expect_error(
    qw_stats(
      as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04", "2020-01-05")), 
      as.character(test_df$var_col), 
      rec_start, rec_end, by = "day"
    ), 
    "Input 'values' must be 'numeric' or 'integer' class"
  )

})

testthat::test_that("qw_stats handles different `by` parameters correctly", {
  
  test_df <- data.frame(
    date_col = as.Date(c("2020-01-01", "2020-01-08", "2020-01-15", "2020-01-22", "2020-01-29")),
    var_col = c(1.2, 2.3, 3.1, 4.5, 5.6)
  )
  rec_start <- as.Date("2020-01-01")
  rec_end <- as.Date("2020-01-31")
  
  result <- qw_stats(test_df$date_col, test_df$var_col, rec_start, rec_end, by = "week")
  
  expected_result <- data.frame(
    rec_startDate = rec_start,
    rec_endDate = rec_end, 
    n_miss_dates = 0, 
    pct_complete_dates = 100, 
    value_startDate = as.Date("2020-01-01"),
    value_endDate = as.Date("2020-01-29"),
    n_value = 5,
    n_pos = 5,
    pos_pct = 100, 
    n_neg = 0, 
    neg_pct = 0, 
    n_na = 0, 
    na_pct = 0, 
    tot_pct = 100, 
    Min. = 1.2,
    Q1 = 2.3,
    Median = 3.1,
    Mean = 3.34,
    Q3 = 4.5,
    Max. = 5.6,
    std = 1.7444197,
    vari = 3.043
  )
  
  testthat::expect_equal(result, expected_result, tolerance = 1e-7)
  
  testthat::expect_error(
    qw_stats(test_df$date_col, test_df$var_col, rec_start, rec_end, by = "d"), 
    "'by' must be one of: day, week, month, quarter, year"
  )
  
})