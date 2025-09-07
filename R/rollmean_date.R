#' Rolling mean of values indexed by date
#'
#' @description 
#' Calculate the rolling mean of a set of numbers indexed by date.
#' 
#' @param dates A vector of dates of 'Date' class.
#' @param values Numeric values. The values must be in correspondence with `dates`, meaning the 
#' *i*th element in `values` must correspond to the *i*th date in `dates`.
#' @param look_behind,look_ahead The number of `look_units` before and after the center date to 
#' include in the sliding window.
#' @param look_units One of "days", "weeks", or "months". The units to give `look_ahead` and 
#' `look_behind`.
#' 
#' @details
#' The amount of time to include in the rolling mean is defined by `look_behind` and `look_ahead`. 
#' All values within the look-behind, the index, and the look-ahead will be used to calculate the 
#' average. If `look_behind` is 5 days and `look_ahead` is 0 days, then all values within the 5 days 
#' before the current index (i.e., the 6th day) will be included - it is a 6-day moving average. 
#' If `look_behind` is 5 days and `look_ahead` is 5 days, then it is a 11-day moving average centered 
#' on the 6th day.
#'  
#' The mean is calculated without `NA` values. If one value in the window is `NA`, then the rolling 
#' mean is that of all values within the window which are not `NA`.
#' 
#' @return A numeric vector with length equal to `dates` and `values`.
#'
#' @examples
#' date_vec <- seq.Date(
#'   from = as.Date("2020-01-01"), 
#'   to = as.Date("2020-01-05"), 
#'   by = "day"
#' )
#' num_vec <- c(2, 3, 4, 20, 10)
#' results <- rollmean_date(
#'   dates = date_vec, 
#'   values = num_vec, 
#'   look_behind = 2
#'  )
#' data.frame(
#'   "date" = date_vec, 
#'   "values" = num_vec, 
#'   "roll_mean" = results
#' )
#' 
#' # Missing data and 10-day moving window
#' date_vec <- seq.Date(
#'   from = as.Date("2020-01-01"), 
#'   to = as.Date("2020-03-31"), 
#'   by = "day"
#' )
#' set.seed(123)
#' num_vec <- sample(1:30, length(date_vec), replace = TRUE)
#' set.seed(123)
#' r <- sample(1:length(date_vec), 20)
#' results <- rollmean_date(
#'   dates = date_vec[-r], 
#'   values = num_vec[-r], 
#'   look_behind = 5, 
#'   look_ahead = 4
#'  )
#' head(
#'   data.frame(
#'     "date" = date_vec[-r], 
#'     "values" = num_vec[-r], 
#'     "roll_mean" = results
#'   ), 
#'   10
#' )
#' 
#' @export

rollmean_date <- function(dates, values, look_behind = 2, 
                          look_ahead = 0, look_units = "days") {
  
  err_check <- check_rollmean_date_inputs(dates, values, look_behind, look_ahead, look_units)
  if (err_check) stop(attr(err_check, "err_msg"))
  
  look_ahead <- as.difftime(look_ahead, units = look_units)
  look_behind <- as.difftime(look_behind, units = look_units)
  
  df <- data.frame("date" = dates, "value" = values)
  df <- df[order(df$date), ]
  
  rollmean_dat <- slider::slide_index(
    .x = df[["value"]], 
    .i = df[["date"]], 
    .f = mean, na.rm = TRUE, 
    .before = look_behind, 
    .after = look_ahead
  )
  rollmean_dat <- as.vector(do.call(rbind, rollmean_dat))
  
  return(rollmean_dat)
  
}