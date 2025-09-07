#' Seasonal and monthly value ranking
#' 
#' @description
#' Summarize data by calculating average values for each month and season, and the year-season 
#' rank. Allows for flexible season definitions by specifying a season start month and the number 
#' of seasons. Intended to be used on a (near) daily water quality record.
#'
#' @inheritParams subsample
#'
#' @details
#' The start of a season, `season_start`, may be any integer from 1 to 12, indicating the month 
#' which is the start of the first season. For example `season_start = 1` makes the first season 
#' start in January while `season_start = 10` makes the season start in October.
#' 
#' The seasonal average accounts for seasons split across years. For example, if `n_seasons = 4` 
#' and `season_start = 12`, then season 1 includes December of e.g. 2020, January 2021, and 
#' February 2021. The year is considered to begin in December and is designated by the year in 
#' which it ends (i.e., the seasonally adjusted year). Thus, the seasonal average for season 1 of 
#' 2021 would be calculated from December 2020 to February 2021. 
#'
#' @return A list with two data frames:
#' \describe{
#'   \item{monthly}{\tabular{lll}{
#'   **Name** \tab **Type** \tab **Description** \cr
#'   year \tab numeric \tab Year \cr
#'   month \tab numeric \tab Month \cr
#'   avg_value \tab numeric \tab Mean of the `values` for that year and month \cr
#'   }}
#'   \item{seasonal}{\tabular{lll}{
#'   **Name** \tab **Type** \tab **Description** \cr
#'   adj_year \tab numeric \tab Adjusted year based on `season_start` \cr
#'   season \tab integer \tab Season number; 1 being the first season of the year \cr
#'   avg_value \tab numeric \tab Mean of the `values` for that 'adj_year' and 'season' \cr
#'   ys_rank \tab numeric \tab Rank of the 'avg_value' for that 'adj_year' and 'season'; 1 being 
#'   the highest 'avg_value' \cr
#'   }}
#' }
#'
#' @examples
#' # Four seasons starting in October
#' date_vec <- seq.Date(
#'   from = as.Date("2020-05-03"), 
#'   to = as.Date("2023-10-17"), 
#'   by = "day"
#' )
#' set.seed(123)
#' num_vec <- sample(30:3000, length(date_vec), replace = TRUE)
#' # Four seasons starting in October
#' results <- summarize_seasons(
#'   dates = date_vec, 
#'   values = num_vec,  
#'   season_start = 10, 
#'   n_seasons = 4
#' )
#' print(head(results$monthly))
#' print(head(results$seasonal))
#' # Three seasons starting in January
#' results <- summarize_seasons(
#'   dates = date_vec, 
#'   values = num_vec, 
#'   season_start = 1, 
#'   n_seasons = 3
#' )
#' print(head(results$monthly))
#' print(head(results$seasonal))
#' 
#' @export

summarize_seasons <- function(dates, values, season_start = 10, n_seasons = 4) {
  
  err_check <- check_summarize_seasons_inputs(
    dates, values, season_start, n_seasons
  )
  if (err_check) stop(attr(err_check, "err_msg"))
  
  df <- data.frame("date" = dates, "value" = values)
  df <- df[order(df[["date"]]), ]
  
  df[["season"]] <- calc_seasons(
    dates = df[["date"]], 
    n_seasons = n_seasons, 
    season_start = season_start
  )
  
  df[["year"]] <- as.numeric(format(df[["date"]], "%Y"))
  df[["month"]] <- as.numeric(format(df[["date"]], "%m"))
  
  formula <- stats::as.formula("value ~ year + month")
  month_data <- stats::aggregate(x = formula, data = df, FUN = mean, na.rm = TRUE)
  names(month_data)[3] <- "avg_value"
  
  df[["adj_year"]] <- ifelse(
    df[["month"]] >= season_start & season_start > 1, 
    df[["year"]] + 1, 
    df[["year"]]
  )
  
  formula <- stats::as.formula("value ~ adj_year + season")
  season_data <- stats::aggregate(x = formula, data = df, FUN = mean, na.rm = TRUE)
  names(season_data)[3] <- "avg_value"
  season_data[["ys_rank"]] <- stats::ave(
    season_data[["avg_value"]], 
    season_data[["adj_year"]], 
    FUN = \(x) rank(-x, ties.method = "first"))
  season_data <- season_data[order(season_data[["adj_year"]], season_data[["season"]]), ]
  row.names(season_data) <- 1:nrow(season_data)
  
  return(
    list(
      "monthly" = month_data, 
      "seasonal" = season_data
    )
  )
  
}
