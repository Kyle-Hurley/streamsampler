#' Convert a daily record to periodic discrete
#'
#' @description#' 
#' Subsample a daily or near-daily data set to one containing infrequent but regularly recurring 
#' records based on a specified frequency. Intended to be used on a (near) daily water quality 
#' record.
#' 
#' @param dates A vector of dates of 'Date' class.
#' @param values Numeric values. The values must be in correspondence with `dates`, meaning the 
#' *i*th element in `values` must correspond to the *i*th date in `dates`.
#' @param day An integer specifying the day of the specified `freq`uency. See 'Details'.
#' @param freq A character string indicating the frequency of selection. Must be one of "day", 
#' "week", or "month". See 'Details'.
#'
#' @return A data.frame with the following columns:
#' \tabular{lll}{
#' **Name** \tab **Type** \tab **Description** \cr
#' date \tab Date \tab Date \cr
#' value \tab numeric \tab Input `values` \cr
#' selection_type \tab character \tab Type of randomly selected value "not_selected" (an 
#' observation not selected), "routine" (selected record) \cr
#' }
#'
#' @examples
#' create_plot <- function(df, log_xfrm = "x", xlab, ylab, subtitle) {
#'   plot(
#'     df$q[df$selection_type == "not_selected"],
#'     df$value[df$selection_type == "not_selected"],
#'     pch = 21, col = "gray",
#'     xlab = xlab, ylab = ylab,
#'     main = paste0("Subsampled Daily Data\n", subtitle),
#'     log = log_xfrm
#'   )
#'   points(
#'     df$q[df$selection_type != "not_selected"],
#'     df$value[df$selection_type != "not_selected"],
#'     pch = 16, cex = 1.5,
#'     col = c(
#'       "routine" = "#42047E"
#'     )[df$selection_type[df$selection_type != "not_selected"]]
#'   )
#'   legend(
#'     "topright",
#'     legend = c("Not Selected", "Routine"),
#'     col = c("gray", "#42047E"),
#'     pch = c(21, 16),
#'     bty = "n"
#'   )
#' }
#' 
#' # 15th of each month
#' sroutine <- subsample_routine(
#'   dates = streamdat$date, values = streamdat$sc, 
#'   day = 15, freq = "month"
#' )
#' df <- merge(streamdat[, -3], sroutine)
#' create_plot(
#'   df, "x", "Discharge (CFS)", 
#'   "Specific Conductivity (uS/cm)", "Subsampled on 15th of each month"
#' )
#' 
#' # Every Wednesday
#' sroutine <- subsample_routine(
#'   dates = streamdat$date, values = streamdat$sc, 
#'   day = 4, freq = "week"
#' )
#' df <- merge(streamdat[, -3], sroutine)
#' create_plot(
#'   df, "x", "Discharge (CFS)", 
#'   "Specific Conductivity (uS/cm)", "Subsampled on every Wednesday"
#' )
#' 
#' # Every 60th day
#' sroutine <- subsample_routine(
#'   dates = streamdat$date, values = streamdat$sc, 
#'   day = 60, freq = "day"
#' )
#' df <- merge(streamdat[, -3], sroutine)
#' create_plot(
#'   df, "x", "Discharge (CFS)", 
#'   "Specific Conductivity (uS/cm)", "Subsampled every 60th day"
#' )
#' 
#' @export

subsample_routine <- function(dates, values, day = 15, freq = "month") {
  
  err_check <- check_subsample_routine_inputs(
    dates, values, day, freq
  )
  if (err_check) stop(attr(err_check, "err_msg"))
  
  if (freq == "day") {
    
    if (!inherits(day, "numeric") || day < 1) {
      stop("Input 'day' must be an integer equal to or greater than 1")
    }
    
  } else if (freq == "week") {
    
    if (!inherits(day, "numeric") || day < 1 || day > 7) {
      stop("Input 'day' must be an integer between 1 and 7")
    }
    
  } else if (freq == "month") {
    
    if (!inherits(day, "numeric") || day < 1 || day > 31) {
      stop("Input 'day' must be an integer between 1 and 31")
    }
    
  }
  
  df <- data.frame("date" = dates, "value" = values)
  
  all_dates <- seq(
    min(df$date, na.rm = TRUE), 
    max(df$date, na.rm = TRUE), 
    by = "day"
  )
  
  selected_dates <- switch(
    freq, 
    day = {
      seq.Date(
        all_dates[1], 
        all_dates[length(all_dates)], 
        by = paste(day, "days")
      )
    }, 
    week = {
      dow_match <- c("Sun" = 1, "Mon" = 2, "Tue" = 3, "Wed" = 4, "Thu" = 5, "Fri" = 6, "Sat" = 7)
      dow <- weekdays(all_dates[1:7], abbreviate = TRUE)
      start_day <- which(names(dow_match)[day] == dow)
      seq.Date(
        all_dates[start_day], 
        all_dates[length(all_dates)], 
        by = "week"
      )
    }, 
    month = {
      if (day > 28) {
        warning(
          "There are fewer than 29 days in each month. Invalid days of the month are counted forward into the next month.")
      }
      seq.Date(
        as.Date(paste(format(all_dates[length(all_dates)], "%Y-%m"), day, sep = "-")), 
        all_dates[1], 
        by = paste(-1, "month")
      ) |> rev()
    }
  )
  
  df <- data.frame(
    "date" = dates, 
    "value" = values
  )
  
  df_ss <- data.frame(
    "date" = selected_dates, 
    "selection_type" = "routine"
  )
  
  selected <- merge(df, df_ss, by = c("date"), all = TRUE)
  selected[is.na(selected$selection_type), "selection_type"] <- "not_selected"
  
  
  return(selected)
  
}
