#' Check inputs to thresholds()
#' 
#' @description
#' Validates input values to thresholds() and returns a message for errors
#' 
#' @inheritParams thresholds
#'
#' @return A TRUE/FALSE value with a "msg" attribute
#' @keywords internal
#' @noRd

check_thresholds_inputs <- function(dates, values, season_start, n_seasons, half_win, threshold) {
  
  err_check <- FALSE
  attr(err_check, "err_msg") <- ""
  
  if (all(is.na(dates)) || length(dates) == 0) {
    
    err_check <- TRUE
    err_msg <- "No non-NA 'dates'"
    
  } else if (!inherits(dates, "Date")) {
    
    err_check <- TRUE
    err_msg <- "Input 'dates' must be 'Date' class"
    
  } else if (all(is.na(values)) || length(values) == 0) {
    
    err_check <- TRUE
    err_msg <- "No non-NA 'values'"
    
  } else if (!inherits(values, "numeric") & !inherits(values, "integer")) {
    
    err_check <- TRUE
    err_msg <- "Input 'values' must be 'numeric' or 'integer' class"
    
  } else if (length(dates) != length(values)) {
    
    err_check <- TRUE
    err_msg <- "Input 'dates' and 'values' must be the same length"
    
  } else if (!inherits(season_start, "numeric") || season_start < 1 || season_start > 12) {
    
    err_check <- TRUE
    err_msg <- "Input 'season_start' must be an integer between 1 and 12"
    
  } else if (!inherits(n_seasons, "numeric") || n_seasons <= 0 || 12 %% n_seasons != 0) {
    
    err_check <- TRUE
    err_msg <- "Input 'n_seasons' must be an integer between 1 and 12"
    
  } else if (!inherits(half_win, "numeric") || half_win < 1) {
    
    err_check <- TRUE
    err_msg <- "Input 'half_win' must be a positive integer"
    
  } else if (!inherits(threshold, "numeric") || threshold <= 0 || threshold > 1) {
    
    err_check <- TRUE
    err_msg <- "Input 'threshold' must be a number between 0 and 1"
    
  }
  
  if (err_check) attr(err_check, "err_msg") <- err_msg
  
  return(err_check)
  
}

#' Check inputs to qw_stats()
#' 
#' @description
#' Validates input values to qw_stats() and returns a message for errors
#' 
#' @inheritParams qw_stats
#'
#' @return A TRUE/FALSE value with a "msg" attribute
#' @keywords internal

check_qw_inputs <- function(dates, values, rec_start, rec_end, by) {
  
  err_check <- FALSE
  attr(err_check, "err_msg") <- ""
  valid_intervals <- c("day", "week", "month", "quarter", "year")
  
  if (all(is.na(values)) || length(values) == 0) {
    
    err_check <- TRUE
    err_msg <- "No non-NA 'values'"
    
  } else if (all(is.na(dates)) || length(dates) == 0) {
    
    err_check <- TRUE
    err_msg <- "No non-NA 'dates'"
    
  } else if (!inherits(dates, "Date")) {
    
    err_check <- TRUE
    err_msg <- "Input 'dates' must be 'Date' class"
    
  } else if (!inherits(rec_start, "Date") || !inherits(rec_end, "Date")) {
    
    err_check <- TRUE
    err_msg <- "Input 'rec_start' and 'rec_end' must be 'Date' class"
    
  } else if (!inherits(values, "numeric") & !inherits(values, "integer")) {
    
    err_check <- TRUE
    err_msg <- "Input 'values' must be 'numeric' or 'integer' class"
    
  } else if (length(dates) != length(values)) {
    
    err_check <- TRUE
    err_msg <- "Input 'dates' and 'values' must be the same length"
    
  } else if (length(by) != 1) {
    
    err_check <- TRUE
    err_msg <- "Input 'by' must be of length 1"
    
  } else if (!inherits(by, "character")) {
    
    err_check <- TRUE
    err_msg <- "Input 'by' must be 'character' class"
    
  } else if (!by %in% valid_intervals) {
    
    err_check <- TRUE
    err_msg <- sprintf("Input 'by' must be one of: %s", paste(valid_intervals, collapse = ", "))
    
  }
  
  if (err_check) attr(err_check, "err_msg") <- err_msg
  
  return(err_check)
}

#' Check inputs to subsample()
#'
#' @inheritParams subsample
#'
#' @return A TRUE/FALSE value with a "msg" attribute
#' @keywords internal

check_subsample_inputs <- function(dates, values, thresh_ref, season_start, n_seasons, 
                                   half_win, threshold, n_samples, freq, n_et_samples, 
                                   season_weights, target, look_behind, look_ahead, look_units, 
                                   seed) {
  
  err_check <- FALSE
  attr(err_check, "err_msg") <- ""
  valid_intervals <- c("week", "month", "quarter", "year")
  valid_targets <- c("none", "peaks")
  valid_look_units <- c("days", "weeks", "months")
  
  d_l <- length(dates)
  v_l <- length(values)
  tr_l <- length(thresh_ref)
  
  if (all(is.na(values)) || length(values) == 0) {
    
    err_check <- TRUE
    err_msg <- "No non-NA 'values'"
    
  } else if (all(is.na(dates)) || length(dates) == 0) {
    
    err_check <- TRUE
    err_msg <- "No non-NA 'dates'"
    
  } else if (!inherits(dates, "Date")) {
    
    err_check <- TRUE
    err_msg <- "Input 'dates' must be 'Date' class"
    
  } else if (!inherits(values, "numeric") & !inherits(values, "integer")) {
    
    err_check <- TRUE
    err_msg <- "Input 'values' must be 'numeric' or 'integer' class"
    
  } else if (all(is.na(thresh_ref)) || length(thresh_ref) == 0) {
    
    err_check <- TRUE
    err_msg <- "No non-NA 'thresh_ref'"
    
  } else if (!inherits(thresh_ref, "numeric") & !inherits(thresh_ref, "integer")) {
    
    err_check <- TRUE
    err_msg <- "Input 'thresh_ref' must be 'numeric' or 'integer' class"
    
  } else if ((d_l != v_l) || (v_l != tr_l) || (d_l != tr_l)) {
    
    err_check <- TRUE
    err_msg <- "Input 'dates', 'values', and 'thresh_ref' must be the same length"
    
  } else if (!inherits(season_start, "numeric") || season_start < 1 || season_start > 12) {
    
    err_check <- TRUE
    err_msg <- "Input 'season_start' must be an integer between 1 and 12"
    
  } else if (!inherits(n_seasons, "numeric") || n_seasons < 1 || 12 %% n_seasons != 0) {
    
    err_check <- TRUE
    err_msg <- "Input 'n_seasons' must be an integer between 1 and 12 and a factor of 12"
    
  } else if (!inherits(half_win, "numeric") || half_win < 1) {
    
    err_check <- TRUE
    err_msg <- "Input 'half_win' must be a positive integer"
    
  } else if (!inherits(threshold, "numeric") || threshold <= 0 || threshold > 1) {
    
    err_check <- TRUE
    err_msg <- "Input 'threshold' must be a number between 0 and 1"
    
  } else if (!inherits(n_samples, "numeric") || n_samples < 1) {
    
    err_check <- TRUE
    err_msg <- "Input 'n_samples' must be a positive integer"
    
  } else if (!inherits(freq, "character")) {
    
    err_check <- TRUE
    err_msg <- "Input 'freq' must be 'character' class"
    
  } else if (!freq %in% valid_intervals) {
    
    err_check <- TRUE
    err_msg <- sprintf(
      "Input 'freq' must be one of: %s", paste(valid_intervals, collapse = ", ")
    )
    
  } else if (!inherits(n_et_samples, "numeric")) {
    
    err_check <- TRUE
    err_msg <- "Input 'n_et_samples' must be 'numeric' class"
    
  } else if (!inherits(season_weights, "numeric")) {
    
    err_check <- TRUE
    err_msg <- "Input 'season_weights' must be 'numeric' class"
    
  } else if (!inherits(target, "character")) {
    
    err_check <- TRUE
    err_msg <- "Input 'target' must be 'character' class"
    
  } else if (!target %in% valid_targets) {
    
    err_check <- TRUE
    err_msg <- sprintf(
      "Input 'target' must be one of: %s", paste(valid_targets, collapse = ", ")
    )
    
  } else if (!inherits(look_behind, "numeric") || !inherits(look_ahead, "numeric")) {
    
    err_check <- TRUE
    err_msg <- "Inputs 'look_behind' and 'look_ahead' must be class 'numeric'"
    
  } else if (!look_units %in% valid_look_units) {
    
    err_check <- TRUE
    err_msg <- sprintf(
      "Input 'look_units' must be one of: %s", paste(valid_look_units, collapse = ", ")
    )
    
  } else if (!inherits(seed, "numeric")) {
    
    err_check <- TRUE
    err_msg <- "Input 'seed' must be class 'numeric'"
    
  }
  
  if (err_check) attr(err_check, "err_msg") <- err_msg
  
  return(err_check)
}

#' Check inputs to subsample_routine()
#'
#' @inheritParams subsample_routine
#'
#' @return A TRUE/FALSE value with a "msg" attribute
#' @keywords internal

check_subsample_routine_inputs <- function(dates, values, day, freq) {
  
  err_check <- FALSE
  attr(err_check, "err_msg") <- ""
  valid_intervals <- c("day", "week", "month")
  
  if (all(is.na(values)) || length(values) == 0) {
    
    err_check <- TRUE
    err_msg <- "No non-NA 'values'"
    
  } else if (all(is.na(dates)) || length(dates) == 0) {
    
    err_check <- TRUE
    err_msg <- "No non-NA 'dates'"
    
  } else if (!inherits(dates, "Date")) {
    
    err_check <- TRUE
    err_msg <- "Input 'dates' must be 'Date' class"
    
  } else if (!inherits(values, "numeric") & !inherits(values, "integer")) {
    
    err_check <- TRUE
    err_msg <- "Input 'values' must be 'numeric' or 'integer' class"
    
  } else if (length(dates) != length(values)) {
    
    err_check <- TRUE
    err_msg <- "Input 'dates', 'values', and 'thresh_ref' must be the same length"
    
  } else if (!inherits(freq, "character")) {
    
    err_check <- TRUE
    err_msg <- "Input 'freq' must be 'character' class"
    
  } else if (!freq %in% valid_intervals) {
    
    err_check <- TRUE
    err_msg <- sprintf(
      "Input 'freq' must be one of: %s", paste(valid_intervals, collapse = ", ")
    )
    
  } 
  
  if (err_check) attr(err_check, "err_msg") <- err_msg
  
  return(err_check)
}


#' Check inputs to summarize_seasons()
#'
#' @inheritParams summarize_seasons
#'
#' @return A TRUE/FALSE value with a "msg" attribute
#' @keywords internal

check_summarize_seasons_inputs <- function(dates, values, season_start, n_seasons) {
  
  err_check <- FALSE
  attr(err_check, "err_msg") <- ""
  
  if (all(is.na(values)) || length(values) == 0) {
    
    err_check <- TRUE
    err_msg <- "No non-NA 'values'"
    
  } else if (all(is.na(dates)) || length(dates) == 0) {
    
    err_check <- TRUE
    err_msg <- "No non-NA 'dates'"
    
  } else if (!inherits(dates, "Date")) {
    
    err_check <- TRUE
    err_msg <- "Input 'dates' must be 'Date' class"
    
  } else if (!inherits(values, "numeric") & !inherits(values, "integer")) {
    
    err_check <- TRUE
    err_msg <- "Input 'values' must be 'numeric' or 'integer' class"
    
  } else if (
    (!inherits(season_start, "numeric") & !inherits(values, "integer")) || 
    season_start < 1 || season_start > 12 || season_start %% 1 != 0
  ) {
    
    err_check <- TRUE
    err_msg <- "Input 'season_start' must be an integer between 1 and 12"
    
  } else if (
    (!inherits(n_seasons, "numeric") & !inherits(n_seasons, "integer")) || 
    n_seasons < 1 || 12 %% n_seasons != 0
  ) {
    
    err_check <- TRUE
    err_msg <- "Input 'n_seasons' must be an integer between 1 and 12"
    
  }
  
  if (err_check) attr(err_check, "err_msg") <- err_msg
  
  return(err_check)
}

#' Check inputs to eval_dates()
#'
#' @inheritParams eval_dates
#'
#' @return A TRUE/FALSE value with a "msg" attribute
#' @keywords internal

check_eval_dates_inputs <- function(dates, rec_start, rec_end, by) {
  
  err_check <- FALSE
  attr(err_check, "err_msg") <- ""
  valid_intervals <- c("day", "week", "month", "quarter", "year")
  
  if (all(is.na(dates)) || length(dates) == 0) {
    
    err_check <- TRUE
    err_msg <- "No non-NA 'dates'"
    
  } else if (!inherits(dates, "Date")) {
    
    err_check <- TRUE
    err_msg <- "Input 'dates' must be 'Date' class"
    
  } else if (!inherits(rec_start, "Date") || !inherits(rec_end, "Date")) {
    
    err_check <- TRUE
    err_msg <- "Input 'rec_start' and 'rec_end' must be 'Date' class"
    
  } else if (length(by) != 1) {
    
    err_check <- TRUE
    err_msg <- "Input 'by' must be of length 1"
    
  } else if (!inherits(by, "character")) {
    
    err_check <- TRUE
    err_msg <- "Input 'by' must be 'character' class"
    
  } else if (!by %in% valid_intervals) {
    
    err_check <- TRUE
    err_msg <- sprintf("Input 'by' must be one of: %s", paste(valid_intervals, collapse = ", "))
    
  }
  
  if (err_check) attr(err_check, "err_msg") <- err_msg
  
  return(err_check)
}

#' Check inputs to local_max_index() and local_min_index()
#'
#' @inheritParams local_max_index
#'
#' @return A TRUE/FALSE value with a "msg" attribute
#' @keywords internal

check_local_index_inputs <- function(dates, values, look_behind, look_ahead, look_units) {
  
  err_check <- FALSE
  attr(err_check, "err_msg") <- ""
  valid_look_units <- c("days", "weeks", "months")
  
  if (all(is.na(values)) || length(values) == 0) {
    
    err_check <- TRUE
    err_msg <- "No non-NA 'values'"
    
  } else if (any(is.na(dates)) || length(dates) == 0) {
    
    err_check <- TRUE
    err_msg <- "Input 'dates' cannot have NA values or length 0"
    
  } else if (!inherits(dates, "Date")) {
    
    err_check <- TRUE
    err_msg <- "Input 'dates' must be 'Date' class"
    
  } else if (is.unsorted(dates, na.rm = TRUE)) {
    
    err_check <- TRUE
    err_msg <- "Input 'dates' must be in ascending order"
    
  } else if (!inherits(values, "numeric") & !inherits(values, "integer")) {
    
    err_check <- TRUE
    err_msg <- "Input 'values' must be 'numeric' or 'integer' class"
    
  } else if (!inherits(look_behind, "numeric") || !inherits(look_ahead, "numeric")) {
    
    err_check <- TRUE
    err_msg <- "Inputs 'look_behind' and 'look_ahead' must be class 'numeric'"
    
  } else if (!look_units %in% valid_look_units) {
    
    err_check <- TRUE
    err_msg <- sprintf(
      "Input 'look_units' must be one of: %s", paste(valid_look_units, collapse = ", ")
    )
    
  }
  
  if (err_check) attr(err_check, "err_msg") <- err_msg
  
  return(err_check)
}

#' Check inputs to rollmean_date()
#'
#' @inheritParams rollmean_date
#'
#' @return A TRUE/FALSE value with a "msg" attribute
#' @keywords internal

check_rollmean_date_inputs <- function(dates, values, look_behind, look_ahead, look_units) {
  
  err_check <- FALSE
  attr(err_check, "err_msg") <- ""
  valid_look_units <- c("days", "weeks", "months")
  
  if (all(is.na(values)) || length(values) == 0) {
    
    err_check <- TRUE
    err_msg <- "No non-NA 'values'"
    
  } else if (any(is.na(dates)) || length(dates) == 0) {
    
    err_check <- TRUE
    err_msg <- "Input 'dates' cannot have NA values or length 0"
    
  } else if (!inherits(dates, "Date")) {
    
    err_check <- TRUE
    err_msg <- "Input 'dates' must be 'Date' class"
    
  } else if (is.unsorted(dates, na.rm = TRUE)) {
    
    err_check <- TRUE
    err_msg <- "Input 'dates' must be in ascending order"
    
  } else if (!inherits(values, "numeric") & !inherits(values, "integer")) {
    
    err_check <- TRUE
    err_msg <- "Input 'values' must be 'numeric' or 'integer' class"
    
  } else if (!inherits(look_behind, "numeric") || !inherits(look_ahead, "numeric")) {
    
    err_check <- TRUE
    err_msg <- "Inputs 'look_behind' and 'look_ahead' must be class 'numeric'"
    
  } else if (!look_units %in% valid_look_units) {
    
    err_check <- TRUE
    err_msg <- sprintf(
      "Input 'look_units' must be one of: %s", paste(valid_look_units, collapse = ", ")
    )
    
  }
  
  if (err_check) attr(err_check, "err_msg") <- err_msg
  
  return(err_check)
}
