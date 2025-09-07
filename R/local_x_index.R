#' Find indexed local minima/maxima
#'
#' @description
#' Identify local minima or maxima of a vector across a sliding date window. Intended to be used on 
#' a (near) daily water quality record.
#' 
#' @param dates A vector of dates of 'Date' class. There are 2 restrictions: 
#' * The vector must be in ascending order; duplicates are allowed
#' * The vector cannot have missing values (i.e., no NA)
#' @param values A vector of numeric values. The values must be in correspondence with `dates`, 
#' meaning the *i*th element in `values` must correspond to the *i*th date in `dates`.
#' @param look_behind,look_ahead The number of `look_units` before and after the center date to 
#' include in the sliding window to determine local maxima or minima.
#' @param look_units One of "days", "weeks", or "months". The units to give `look_ahead` and 
#' `look_behind`.
#' 
#' @details
#' The size of the moving window is adjusted to be shorter at the ends of a record. For example, at 
#' the start of a record only the first element of the record plus the values included in the 
#' `look_ahead` are evaluated.
#' 
#' Any object that can be added or subtracted from the `dates` with `+` and `-` can be used for 
#' `look_behind` and `look_ahead`. By default, these are both `2` days. This creates a 5-day sliding 
#' window where the 3rd day is the element evaluated. 
#' 
#' If an element in `values` is `NA`, then `FALSE` is returned. If a group of elements within a 
#' window is all `NA`, then `FALSE` is returned.
#' 
#' @return 
#' A logical vector with the same length as `dates` and `values`.
#'
#' @examples
#' # Works as expected
#' date_vec <- seq.Date(
#'   from = as.Date("2020-01-01"), 
#'   to = as.Date("2020-01-06"), 
#'   by = "day"
#' )
#' num_vec <- c(10, 11, 50, 9, 8, 100)
#' 
#' results <- local_max_index(
#'   dates = date_vec, 
#'   values = num_vec
#' )
#' print(data.frame(
#'   "date" = date_vec, 
#'   "value" = num_vec, 
#'   "local_max" = results
#' ))
#' 
#' # Different look_ahead/behind
#' date_vec <- seq(
#'   from = as.Date("2020-01-01"), 
#'   to = as.Date("2020-02-01"), 
#'   by = "day"
#' )
#' set.seed(123)
#' num_vec <- sample(30:300, length(date_vec), replace = TRUE)
#' 
#' results <- local_max_index(
#'   dates = date_vec, 
#'   values = num_vec, 
#'   look_behind = 1, 
#'   look_ahead = , 
#'   look_units = "days"
#' )
#' print(data.frame(
#'   "date" = date_vec, 
#'   "value" = num_vec, 
#'   "local_max" = results
#' ))
#' @name local_x_index
NULL


#' @rdname local_x_index
#' @export

local_max_index <- function(dates, values, look_behind = 2, look_ahead = 2, look_units = "days") {
  
  err_check <- check_local_index_inputs(dates, values, look_behind, look_ahead, look_units)
  if (err_check) stop(attr(err_check, "err_msg"))
  
  look_ahead <- as.difftime(look_ahead, units = look_units)
  look_behind <- as.difftime(look_behind, units = look_units)
  
  df <- data.frame("date" = dates, "value" = values)
  df <- df[order(df[["date"]]), ]
  
  date_groups <- slider::slide_index(
    .x = df, 
    .i = df[["date"]], 
    .f = ~.x, 
    .before = look_behind,
    .after = look_ahead
  )
  
  is_local_max <- vector(mode = "logical", length = nrow(df))
  for (i in seq_along(is_local_max)) {
    
    current_value <- df[[i, "value"]]
    index <- which.max(date_groups[[i]][["value"]])
    if (length(index) == 1 && !is.na(current_value)) {
      max_value <- date_groups[[i]][index, "value"]
      is_local_max[i] <- current_value == max_value
    } else {
      is_local_max[i] <- FALSE
    }
    
  }
  
  return(is_local_max)
  
}


#' @rdname local_x_index
#' @export

local_min_index <- function(dates, values, look_behind = 2, look_ahead = 2, look_units = "days") {
  
  err_check <- check_local_index_inputs(dates, values, look_behind, look_ahead, look_units)
  if (err_check) stop(attr(err_check, "err_msg"))
  
  look_ahead <- as.difftime(look_ahead, units = look_units)
  look_behind <- as.difftime(look_behind, units = look_units)
  
  df <- data.frame("date" = dates, "value" = values)
  df <- df[order(df[["date"]]), ]
  
  date_groups <- slider::slide_index(
    .x = df, 
    .i = df[["date"]], 
    .f = ~.x, 
    .before = look_behind,
    .after = look_ahead
  )
  
  is_local_min <- vector(mode = "logical", length = nrow(df))
  for (i in seq_along(is_local_min)) {
    
    current_value <- df[[i, "value"]]
    index <- which.min(date_groups[[i]][["value"]])
    if (length(index) == 1 && !is.na(current_value)) {
      min_value <- date_groups[[i]][index, "value"]
      is_local_min[i] <- current_value == min_value
    } else {
      is_local_min[i] <- FALSE
    }
    
  }
  
  return(is_local_min)
  
}