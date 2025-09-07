#' Adjust number of samples
#'
#' @param dates A vector of dates.
#' @param freq A character vector. May be "week", "month", "quarter", or "year".
#' @param n_size Integer of the number to be adjusted.
#' 
#' @return An integer
#' @keywords internal

adjust_n <- function(dates, freq, n_size = 8) {
  
  if (n_size == 1) {
    return(n_size)
  }
  
  group_days <- switch(
    freq, 
    week = 7, 
    month = {
      last_day_of_month <- seq.Date(
        from = dates[1], by = "month", length.out = 2
        )[2] - as.difftime(1, units = "days")
      num_days <- as.numeric(
        last_day_of_month - as.Date(format(dates[1], "%Y-%m-01"))
      ) + as.difftime(1, units = "days")
      as.numeric(num_days)
    }, 
    quarter = {
      year <- as.numeric(unique(format(dates, "%Y")))
      current_quarter <- unique(quarters(dates))
      is_leap_year <- if ((year %% 4 == 0 && year %% 100 != 0) || year %% 400 == 0) TRUE else FALSE
      if (current_quarter == "Q1") {
        if (is_leap_year) 91 else 90
      } else if (current_quarter == "Q2") {
        91
      } else if (current_quarter == "Q3") {
        92
      } else if (current_quarter == "Q4") {
        92
      }
    }, 
    year = {
      year <- as.numeric(unique(format(dates, "%Y")))
      if ((year %% 4 == 0 && year %% 100 != 0) || year %% 400 == 0) 366 else 365
    }
  )
  
  if (n_size >= group_days) {
    n_size <- group_days
  }
  
  date_groups <- slider::slide_index(
    .x = dates, 
    .i = dates, 
    .f = ~.x, 
    .before = as.difftime(0, units = "days"),
    .after = as.difftime(0, units = "days")
  )
  n_dates <- length(date_groups)
  
  new_n <- round(
    n_dates / floor(group_days / n_size), 
    digits = 0
  )
  
  if (new_n < 1) {
    new_n <- 1
  }
  
  if (new_n > n_size) {
    new_n <- n_size
  }
  
  return(new_n)
  
}