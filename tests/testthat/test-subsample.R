test_that("subsample works with intended inputs", {
  
  date_vec <- seq.Date(as.Date("2015-01-01"), as.Date("2024-12-31"), by = "day")
  set.seed(123)
  vals <- sample(20:500, length(date_vec), replace = TRUE)
  set.seed(123)
  tref <- sample(20:5000, length(date_vec), replace = TRUE)
  
  result <- subsample(
    dates = date_vec, 
    values = vals, 
    thresh_ref = tref
  )
  
  testthat::expect_type(result, "list")
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(dim(result), c(length(date_vec), 9))
  testthat::expect_contains(
    unique(result$selection_type), 
    c("not_selected", "exceeds_threshold", "below_threshold")
  )
  testthat::expect_contains(
    unique(result$season), c(1, 2, 3, 4)
  )
  testthat::expect_contains(
    unique(result$ys_rank), c(1, 2, 3, 4)
  )
  testthat::expect_true(
    table(result$selection_type)[["below_threshold"]] == 120
  )
  sample_count <- table(result$selection_type)
  testthat::expect_true(
    (sample_count[["exceeds_threshold"]] < sample_count[["below_threshold"]]) < sample_count[["not_selected"]]
  )
  
  result <- subsample(
    dates = date_vec, 
    values = vals, 
    thresh_ref = tref, 
    freq = "week"
  )
  
  testthat::expect_type(result, "list")
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(dim(result), c(length(date_vec), 9))
  testthat::expect_contains(
    unique(result$selection_type), 
    c("not_selected", "exceeds_threshold", "below_threshold")
  )
  testthat::expect_contains(
    unique(result$season), c(1, 2, 3, 4)
  )
  testthat::expect_contains(
    unique(result$ys_rank), c(1, 2, 3, 4)
  )
  testthat::expect_true(
    table(result$selection_type)[["below_threshold"]] == 528
  )
  sample_count <- table(result$selection_type)
  testthat::expect_true(
    (sample_count[["exceeds_threshold"]] < sample_count[["below_threshold"]]) < sample_count[["not_selected"]]
  )
  
  result <- subsample(
    dates = date_vec, 
    values = vals, 
    thresh_ref = tref, 
    freq = "year"
  )
  
  testthat::expect_type(result, "list")
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(dim(result), c(length(date_vec), 9))
  testthat::expect_contains(
    unique(result$selection_type), 
    c("not_selected", "exceeds_threshold", "below_threshold")
  )
  testthat::expect_contains(
    unique(result$season), c(1, 2, 3, 4)
  )
  testthat::expect_contains(
    unique(result$ys_rank), c(1, 2, 3, 4)
  )
  testthat::expect_true(
    table(result$selection_type)[["below_threshold"]] == 10
  )
  sample_count <- table(result$selection_type)
  testthat::expect_true(
    (sample_count[["exceeds_threshold"]] < sample_count[["below_threshold"]]) < sample_count[["not_selected"]]
  )
  
  # All settings adjusted
  result <- subsample(
    dates = date_vec, 
    values = vals, 
    thresh_ref = tref, 
    season_start = 1, n_seasons = 6, 
    half_win = 1, threshold = 0.9, 
    n_samples = 2, freq = "quarter", 
    n_et_samples = 12, season_weights = c(3, 3, 2, 1, 1, 1), 
    target = "peaks", 
    look_behind = 14, look_ahead = 14, look_units = "days", 
    seed = 123
  )
  
  testthat::expect_type(result, "list")
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(dim(result), c(length(date_vec), 10))
  testthat::expect_contains(
    unique(result$selection_type), 
    c("not_selected", "exceeds_threshold", "below_threshold")
  )
  testthat::expect_contains(
    unique(result$season), c(1, 2, 3, 4, 5, 6)
  )
  testthat::expect_contains(
    unique(result$ys_rank), c(1, 2, 3, 4, 5, 6)
  )
  testthat::expect_true(
    table(result$selection_type)[["below_threshold"]] == 80
  )
  sample_count <- table(result$selection_type)
  testthat::expect_true(
    (sample_count[["below_threshold"]] < sample_count[["exceeds_threshold"]]) < sample_count[["not_selected"]]
  )
  
})

testthat::test_that("subsample gives errors", {
  
  date_vec <- seq.Date(as.Date("2015-01-01"), as.Date("2024-12-31"), by = "day")
  vals <- sample(20:500, length(date_vec), replace = TRUE)
  tref <- sample(20:5000, length(date_vec), replace = TRUE)
  
  testthat::expect_error(
      subsample(
      dates = date_vec, 
      values = rep(NA, length(date_vec)), 
      thresh_ref = tref
    ), 
    "No non-NA 'values'"
  )
  testthat::expect_error(
    subsample(
      dates = date_vec, 
      values = vector("numeric", 0), 
      thresh_ref = tref
    ), 
    "No non-NA 'values'"
  )
  testthat::expect_error(
    subsample(
      dates = rep(NA, length(date_vec)), 
      values = vals, 
      thresh_ref = tref
    ), 
    "No non-NA 'dates'"
  )
  testthat::expect_error(
    subsample(
      dates = vector("numeric", 0), 
      values = vals, 
      thresh_ref = tref
    ), 
    "No non-NA 'dates'"
  )
  testthat::expect_error(
    subsample(
      dates = as.POSIXlt(date_vec), 
      values = vals, 
      thresh_ref = tref
    ), 
    "Input 'dates' must be 'Date' class"
  )
  testthat::expect_error(
    subsample(
      dates = date_vec, 
      values = as.character(vals), 
      thresh_ref = tref
    ), 
    "Input 'values' must be 'numeric' or 'integer' class"
  )
  testthat::expect_error(
    subsample(
      dates = date_vec, 
      values = vals, 
      thresh_ref = rep(NA, length(date_vec))
    ), 
    "No non-NA 'thresh_ref'"
  )
  testthat::expect_error(
    subsample(
      dates = date_vec, 
      values = vals, 
      thresh_ref = vector("numeric", 0)
    ), 
    "No non-NA 'thresh_ref'"
  )
  testthat::expect_error(
    subsample(
      dates = date_vec, 
      values = vals, 
      thresh_ref = as.character(vals)
    ), 
    "Input 'thresh_ref' must be 'numeric' or 'integer' class"
  )
  testthat::expect_error(
    subsample(
      dates = date_vec[-1], 
      values = vals, 
      thresh_ref = tref
    ), 
    "Input 'dates', 'values', and 'thresh_ref' must be the same length"
  )
  testthat::expect_error(
    subsample(
      dates = date_vec, 
      values = vals, 
      thresh_ref = tref, 
      season_start = "a"
    ), 
    "Input 'season_start' must be an integer between 1 and 12"
  )
  testthat::expect_error(
    subsample(
      dates = date_vec, 
      values = vals, 
      thresh_ref = tref, 
      season_start = 0
    ), 
    "Input 'season_start' must be an integer between 1 and 12"
  )
  testthat::expect_error(
    subsample(
      dates = date_vec, 
      values = vals, 
      thresh_ref = tref, 
      season_start = 13
    ), 
    "Input 'season_start' must be an integer between 1 and 12"
  )
  testthat::expect_error(
    subsample(
      dates = date_vec, 
      values = vals, 
      thresh_ref = tref, 
      n_seasons = "a"
    ), 
    "Input 'n_seasons' must be an integer between 1 and 12"
  )
  testthat::expect_error(
    subsample(
      dates = date_vec, 
      values = vals, 
      thresh_ref = tref, 
      n_seasons = 0
    ), 
    "Input 'n_seasons' must be an integer between 1 and 12"
  )
  testthat::expect_error(
    subsample(
      dates = date_vec, 
      values = vals, 
      thresh_ref = tref, 
      n_seasons = 5
    ), 
    "Input 'n_seasons' must be an integer between 1 and 12"
  )
  testthat::expect_error(
    subsample(
      dates = date_vec, 
      values = vals, 
      thresh_ref = tref, 
      n_seasons = 13
    ), 
    "Input 'n_seasons' must be an integer between 1 and 12"
  )
  testthat::expect_error(
    subsample(
      dates = date_vec, 
      values = vals, 
      thresh_ref = tref, 
      half_win = "a"
    ), 
    "Input 'half_win' must be a positive integer"
  )
  testthat::expect_error(
    subsample(
      dates = date_vec, 
      values = vals, 
      thresh_ref = tref, 
      half_win = -2
    ), 
    "Input 'half_win' must be a positive integer"
  )
  testthat::expect_error(
    subsample(
      dates = date_vec, 
      values = vals, 
      thresh_ref = tref, 
      threshold = 0
    ), 
    "Input 'threshold' must be a number between 0 and 1"
  )
  testthat::expect_error(
    subsample(
      dates = date_vec, 
      values = vals, 
      thresh_ref = tref, 
      threshold = 2
    ), 
    "Input 'threshold' must be a number between 0 and 1"
  )
  testthat::expect_error(
    subsample(
      dates = date_vec, 
      values = vals, 
      thresh_ref = tref, 
      n_samples = "a"
    ), 
    "Input 'n_samples' must be a positive integer"
  )
  testthat::expect_error(
    subsample(
      dates = date_vec, 
      values = vals, 
      thresh_ref = tref, 
      n_samples = 0
    ), 
    "Input 'n_samples' must be a positive integer"
  )
  testthat::expect_error(
    subsample(
      dates = date_vec, 
      values = vals, 
      thresh_ref = tref, 
      freq = 1
    ), 
    "Input 'freq' must be 'character' class"
  )
  testthat::expect_error(
    subsample(
      dates = date_vec, 
      values = vals, 
      thresh_ref = tref, 
      freq = "decades"
    ), 
    "Input 'freq' must be one of: week, month, quarter, year"
  )
  testthat::expect_error(
    subsample(
      dates = date_vec, 
      values = vals, 
      thresh_ref = tref, 
      n_et_samples = "a"
    ), 
    "Input 'n_et_samples' must be 'numeric' class"
  )
  testthat::expect_error(
    subsample(
      dates = date_vec, 
      values = vals, 
      thresh_ref = tref, 
      season_weights = "a"
    ), 
    "Input 'season_weights' must be 'numeric' class"
  )
  testthat::expect_error(
    subsample(
      dates = date_vec, 
      values = vals, 
      thresh_ref = tref, 
      target = 1
    ), 
    "Input 'target' must be 'character' class"
  )
  testthat::expect_error(
    subsample(
      dates = date_vec, 
      values = vals, 
      thresh_ref = tref, 
      target = "troughs"
    ), 
    "Input 'target' must be one of: none, peaks"
  )
  testthat::expect_error(
    subsample(
      dates = date_vec, 
      values = vals, 
      thresh_ref = tref, 
      look_behind = "a"
    ), 
    "Inputs 'look_behind' and 'look_ahead' must be class 'numeric'"
  )
  testthat::expect_error(
    subsample(
      dates = date_vec, 
      values = vals, 
      thresh_ref = tref, 
      look_ahead = "a"
    ), 
    "Inputs 'look_behind' and 'look_ahead' must be class 'numeric'"
  )
  testthat::expect_error(
    subsample(
      dates = date_vec, 
      values = vals, 
      thresh_ref = tref, 
      look_units = "decades"
    ), 
    "Input 'look_units' must be one of: days, weeks, months"
  )
  testthat::expect_error(
    subsample(
      dates = date_vec, 
      values = vals, 
      thresh_ref = tref, 
      seed = "a"
    ), 
    "Input 'seed' must be class 'numeric'"
  )
  
})