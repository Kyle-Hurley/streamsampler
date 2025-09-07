#' Identify gaps in a sequence of dates
#'
#' @description 
#' Identify gaps in a sequence of dates and return a data frame with the number of missing days, 
#' start and end dates, and the starting location of the gap in the vector. Intended to be used on 
#' a (near) daily water quality record.
#'
#' @param dates A vector of dates to be evaluated. Must be 'Date' class.
#' 
#' @return A data frame with the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' n_days \tab integer \tab The number of days in the gap \cr
#' start \tab date \tab The start date of the gap \cr
#' end \tab date \tab The end date of the gap \cr
#' location \tab integer \tab The location index of the gap in the original dates vector
#' }
#' 
#' @examples
#' dates <- as.Date(c("2020-01-01", "2020-01-03", "2020-01-04", "2020-01-10", "2020-01-15"))
#' find_gaps(dates)
#'
#' dates_no_gaps <- seq.Date(as.Date("2020-01-01"), as.Date("2020-01-05"), by = "day")
#' find_gaps(dates_no_gaps)
#'
#' dates_with_na <- as.Date(c("2020-01-01", "2020-01-03", NA, "2020-01-10"))
#' find_gaps(dates_with_na)
#' @export

find_gaps <- function(dates) {
  
  if (!inherits(dates, "Date")) {
    stop("Input 'dates' must be 'Date' class")
  } else if (length(dates) <= 1) {
    warning("Input 'dates' has length 1 or fewer. Returning NA.")
    return(
      data.frame(
        "n_days" = NA, 
        "start" = NA, 
        "end" = NA, 
        "location" = NA
      )
    )
  }
  
  dates <- dates[!is.na(dates)]
  dates <- dates[order(dates)]
  
  date_diff <- diff.Date(dates, lag = 1, differences = 1)
  date_diff <- c(NA, date_diff - 1)
  
  gaps <- data.frame(
    "n_days" = date_diff, 
    "start" = dates - date_diff, 
    "end" = dates - 1
  )
  
  gaps[["location"]] <- row.names(gaps)
  gaps <- gaps[2:nrow(gaps), ]
  gaps <- gaps[gaps[, "n_days"] > 0, ]
  gaps <- gaps[order(-gaps[, "n_days"]), ]
  if (nrow(gaps) != 0) {
    row.names(gaps) <- 1:nrow(gaps)
  }
  
  return(gaps)
  
}

