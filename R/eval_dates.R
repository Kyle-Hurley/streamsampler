#' Evaluate completeness of dates
#'
#' @description 
#' Evaluate the completeness of a sequence of dates compared to a hypothetically complete record of 
#' dates. `eval_dates()` will aggregate the dates by different time periods such as day, week, 
#' month, quarter, or year. Intended to be used on a water quality record.
#'
#' @param dates A vector of dates to be evaluated. Must be of class 'Date'.
#' @param rec_start The start date of the recording period. Must be of class 'Date'.
#' @param rec_end The end date of the recording period. Must be of class 'Date'.
#' @param by A character string specifying the time period for aggregation. One of "day", "week", 
#' "month", "quarter", or "year". Default is "day".
#' 
#' @return A data.frame with one row and two columns:
#' \tabular{lll}{
#' **Name** \tab **Type** \tab **Description** \cr
#' pct_complete \tab numeric \tab Percent coverage of the `dates` \cr
#' n_miss \tab integer \tab Number of missing time periods \cr
#' }
#' 
#' @examples
#' dates <- seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day")
#' rec_start <- as.Date("2020-01-01")
#' rec_end <- as.Date("2020-12-31")
#' 
#' # Evaluate by day
#' eval_dates(dates, rec_start, rec_end, by = "day")
#' 
#' # Evaluate by week
#' eval_dates(dates, rec_start, rec_end, by = "week")
#' 
#' # Evaluate by month
#' eval_dates(dates, rec_start, rec_end, by = "month")
#' 
#' # Evaluate by quarter
#' eval_dates(dates, rec_start, rec_end, by = "quarter")
#' 
#' # Evaluate by year
#' eval_dates(dates, rec_start, rec_end, by = "year")
#' 
#' # Example with missing dates
#' dates_with_na <- dates
#' dates_with_na[c(10, 20, 30)] <- NA
#' eval_dates(dates_with_na, rec_start, rec_end, by = "day")
#' @export

eval_dates <- function(dates, rec_start, rec_end, by = "day") {
  
  err_check <- check_eval_dates_inputs(dates, rec_start, rec_end, by)
  if (err_check) stop(attr(err_check, "err_msg"))
  
  dates <- dates[!is.na(dates)]
  dates <- dates[dates >= rec_start & dates <= rec_end]
  
  full_dates <- seq.Date(
    from = rec_start, 
    to = rec_end, 
    by = "day"
  )
  
  switch(
    EXPR = by, 
    day = {
      formatted_dates <- dates
    }, 
    week = {
      formatted_dates <- unique(format(dates, "%Y%W"))
      full_dates <- unique(format(full_dates, "%Y%W"))
    }, 
    month = {
      formatted_dates <- unique(format(dates, "%Y-%m"))
      full_dates <- unique(format(full_dates, "%Y-%m"))
    }, 
    quarter = {
      formatted_dates <- unique(
        paste0(
          format(dates, "%Y"), 
          quarters(dates)
        )
      )
      full_dates <- unique(
        paste0(
          format(full_dates, "%Y"), 
          quarters(as.Date(full_dates))
        )
      )
    }, 
    year = {
      formatted_dates <- unique(format(dates, "%Y"))
      full_dates <- unique(format(full_dates, "%Y"))
    }
  )
  
  pct <- (length(formatted_dates) / length(full_dates)) * 100
  n_miss <- length(full_dates) - length(formatted_dates)
  
  return(
    data.frame(
      "pct_complete" = pct, 
      "n_miss" = n_miss
    )
  )
  
}