#' Find thresholds for each season
#' 
#' @description
#' Calculate the threshold for each unique season in a window of years. Values exceeding these 
#' thresholds would be considered "high" based on the specified quantile. Intended to be used on a 
#' (near) daily water quality record.
#' 
#' @inheritParams subsample
#'
#' @details
#' Thresholds are calculated based on groupings of seasonally adjusted years. If `half_win` is `2`, 
#' the default, then a total of 5 years is used to calculate the threshold. Years are adjusted 
#' such that the year starts on the first of the month of the first season, as determined by 
#' `season_start`, and end one year later, and is designated by the calendar year in which it ends.
#' For example, when `half_win` is 2, the threshold for season 1 of 2021 is the quantile defined by 
#' `threshold` of all season 1 values in 2019, 2020, 2021, 2022, and 2023.
#'
#' @return A data frame with three columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' season \tab integer \tab Season of the threshold \cr
#' threshold \tab numeric \tab Seasonal threshold \cr
#' center_year \tab integer \tab Middle year of the window \cr
#' }
#' 
#' @examples
#' date_vec <- seq.Date(
#'   from = as.Date("2020-05-03"), 
#'   to = as.Date("2023-10-17"), 
#'   by = "day"
#' )
#' set.seed(123)
#' q_vec <- stats::runif(length(date_vec), min = -50, max = 150)
#' df <- data.frame("date" = date_vec, "q" = q_vec)
#' 
#' results <- thresholds(
#'   dates = date_vec, 
#'   values = q_vec
#' )
#' print(head(results))
#' 
#' # Define seasons differently
#' results <- thresholds(
#'   dates = date_vec, 
#'   values = q_vec, 
#'   season_start = 1, 
#'   n_seasons = 3, 
#'   half_win = 2, 
#'   threshold = 0.9
#' )
#' print(head(results))
#' @export

thresholds <- function(dates, values, season_start = 10, n_seasons = 4, half_win = 2, 
                              threshold = 0.8) {
  
  err_check <- check_thresholds_inputs(dates, values, season_start, n_seasons, half_win, threshold)
  if (err_check) stop(attr(err_check, "err_msg"))
  
  df <- data.frame("date" = dates, "value" = values)
  
  df[["adj_year"]] <- ifelse(
    as.numeric(format(df[["date"]], "%m")) >= season_start & season_start > 1,
    as.numeric(format(df[["date"]], "%Y")) + 1,
    as.numeric(format(df[["date"]], "%Y"))
  )
  
  df <- df[order(df[["date"]]), ]
  df[["season"]] <- calc_seasons(
    dates = df[["date"]], 
    n_seasons = n_seasons, 
    season_start = season_start
  )
  
  start_year <- min(df[["adj_year"]], na.rm = TRUE)
  end_year <- max(df[["adj_year"]], na.rm = TRUE)
  
  center_years <- center_year(
    years = start_year:end_year, 
    start_year = start_year, 
    end_year = end_year, 
    half_win = half_win
  )
  
  center_years <- center_years[!duplicated(center_years)]
  thresholds <- lapply(
    center_years,
    function(year) {
      calc_thresh(
        values = df[["value"]],
        seasons = df[["season"]],
        years = df[["adj_year"]],
        center_year = year,
        half_win = half_win,
        threshold = threshold
      )
    }
  )
  thresholds <- do.call(rbind.data.frame, thresholds)
  
  return(thresholds)
  
}

#' Calculate seasonal thresholds
#'
#' `calc_thresh()` calculates thresholds for data within a specified window around a 
#' given year. The thresholds are calculated for each season based on a specified quantile.
#'
#' @param values A numeric vector.
#' @param seasons A vector indicating the season for each observation in `values`.
#' @param years A numeric vector indicating the year for each observation in `values`.
#' @param center_year The center year around which the window is defined.
#' @param half_win The half width of the window of years to group `values` by.
#' @param threshold The quantile of `values`.
#'
#' @return A data frame with the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' season \tab integer \tab Season of the threshold \cr
#' threshold \tab numeric \tab Seasonal threshold \cr
#' center_year \tab integer \tab Middle year of the window \cr
#' }
#' @keywords internal

calc_thresh <- function(values, seasons, years, center_year, half_win = 2, threshold = 0.8) {
  
  min_year <- min(years)
  max_year <- max(years)
  min_window <- center_year - half_win
  max_window <- center_year + half_win
  if (min_window < min_year || max_window > max_year) {
    warning("Window exceeds range of years. Results are for all 'values'.")
    df <- data.frame(
      "var" = values, 
      "season" = seasons
    )
    
    formula <- stats::as.formula("var ~ season")
    thresh_df <- stats::aggregate(
      x = formula, 
      data = df, 
      FUN = stats::quantile, 
      probs = threshold
    )
    thresh_df[["center_year"]] <- center_year
    
    names(thresh_df)[2] <- "threshold"
    
    return(thresh_df)
  }
  
  year_range <- min_window:max_window
  
  index <- which(years %in% year_range)
  season_data_sub <- seasons[index]
  value_data_sub <- values[index]
  
  df <- data.frame(
    "var" = value_data_sub, 
    "season" = season_data_sub
  )
  
  formula <- stats::as.formula("var ~ season")
  thresh_df <- stats::aggregate(
    x = formula, 
    data = df, 
    FUN = stats::quantile, 
    probs = threshold
  )
  thresh_df[["center_year"]] <- center_year
  
  names(thresh_df)[2] <- "threshold"
  
  return(thresh_df)
  
}