#' Calculate summary statistics
#' 
#' @description 
#' Evaluate the completeness of a record compared to a hypothetical "ideal" record and simple 
#' statistics. Intended to be used on a (near) daily water quality record.
#' 
#' @param dates A vector of dates to be evaluated. Must be of class `Date`.
#' @param values A numeric vector to be evaluated. Must be of class `numeric`.
#' @param rec_start The start date of the recording period. Must be of class `Date`.
#' @param rec_end The end date of the recording period. Must be of class `Date`.
#' @param by The time interval to use for the evaluation. One of "day", "week", "month", "quarter", 
#' or "year". Default is "day".
#'
#' @return A data frame with 1 row and 22 columns: 
#' \tabular{lll}{
#' **Name** \tab **Type** \tab **Description** \cr
#' rec_startDate \tab Date \tab Start date of ideal record \cr
#' rec_endDate \tab Date \tab End date of ideal record \cr
#' n_miss_dates \tab integer \tab Number of missing time periods (as specified by `by`) within the 
#' recording period. \cr
#' pct_complete_dates \tab numeric \tab Percentage of the time period (as specified by `by`) 
#' covered by the `dates` \cr
#' value_startDate \tab Date \tab Date of the first `values` in the record \cr
#' value_endDate \tab Date \tab Date of the last `values` in the record \cr
#' n_value \tab integer \tab Number of non-NA `values` \cr
#' n_pos \tab integer \tab Number of non-negative `values` in the data \cr
#' pos_pct \tab numeric \tab Percentage of non-negative `values` in the data \cr
#' n_neg \tab integer \tab Number of negative `values` in the data \cr
#' neg_pct \tab numeric \tab Percentage of negative `values` in the data \cr
#' n_na \tab integer \tab Number of NA `values` in the data \cr
#' na_pct \tab numeric \tab Percentage of NA `values` in the data \cr
#' tot_pct \tab numeric \tab sum of `pos_pct`, `neg_pct`, and `na_pct` \cr
#' Min. \tab numeric \tab Minimum of `values` \cr
#' Q1 \tab numeric \tab 25th percentile of `values` \cr
#' Median \tab numeric \tab Median (50th percentile) of `values` \cr
#' Mean \tab numeric \tab Mean of `values` \cr
#' Q3 \tab numeric \tab 75th percentile of `values` \cr
#' Max. \tab numeric \tab Maximum of `values` \cr
#' std \tab numeric \tab Standard deviation of `values` \cr
#' vari \tab numeric \tab Variance of `values` \cr
#' }
#' 
#' @examples
#' data <- data.frame(
#'   date_col = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04", "2020-01-05")),
#'   var_col = c(1.2, 2.3, 3.1, NA, 4.5)
#' )
#' rec_start <- as.Date("2020-01-01")
#' rec_end <- as.Date("2020-01-05")
#' qw_stats(data$date_col, data$var_col, rec_start, rec_end, by = "day")
#'
#' data_no_missing <- data.frame(
#'   date_col = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04", "2020-01-05")),
#'   var_col = c(1.2, 2.3, 3.1, 4.5, 5.6)
#' )
#' qw_stats(data_no_missing$date_col, data_no_missing$var_col, rec_start, rec_end, by = "day")
#'
#' data_weekly <- data.frame(
#'   date_col = as.Date(c("2020-01-01", "2020-01-08", "2020-01-15", "2020-01-22", "2020-01-29")),
#'   var_col = c(1.2, 2.3, 3.1, 4.5, 5.6)
#' )
#' qw_stats(data_weekly$date_col, data_weekly$var_col, rec_start, rec_end, by = "week")
#' 
#' @importFrom stats sd var
#' @export

qw_stats <- function(dates, values, rec_start, rec_end, by = "day") {
  
  err_check <- check_qw_inputs(dates, values, rec_start, rec_end, by)
  if (err_check) stop(attr(err_check, "err_msg"))
  
  qw_df <- data.frame("date" = dates, "value" = values)
  qw_df <- qw_df[order(qw_df$date), ]
  
  rec_completeness <- eval_dates(
    dates = as.Date(qw_df$date), 
    rec_start = rec_start, 
    rec_end = rec_end, 
    by = by
  )
  
  qw_df <- qw_df[qw_df$date >= rec_start & qw_df$date <= rec_end, ]
  
  value_start <- qw_df[min(which(!is.na(qw_df$value))), "date"]
  value_end <- qw_df[max(which(!is.na(qw_df$value))), "date"]
  value_sign_stats <- eval_sign(values = qw_df$value)
  n_value <- sum(!is.na(qw_df$value))
  
  value_summary <- summary(qw_df$value)
  stdv <- stats::sd(qw_df$value, na.rm = TRUE)
  vari <- stats::var(qw_df$value, na.rm = TRUE)
  
  qwstats <- data.frame(
    "rec_startDate" = rec_start, 
    "rec_endDate" = rec_end, 
    "n_miss_dates" = rec_completeness$n_miss, 
    "pct_complete_dates" = rec_completeness$pct_complete, 
    "value_startDate" = value_start, 
    "value_endDate" = value_end, 
    "n_value" = n_value, 
    value_sign_stats,
    "Min." = value_summary[[1]],
    "Q1" = value_summary[[2]],
    "Median" = value_summary[[3]],
    "Mean" = value_summary[[4]],
    "Q3" = value_summary[[5]],
    "Max." = value_summary[[6]], 
    "std" = stdv, 
    "vari" = vari
  )
  
  return(qwstats)
  
}
