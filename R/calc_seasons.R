#' Calculate seasons from dates
#'
#' @description 
#' Assign seasons to a vector of dates based on the specified number of seasons and the starting 
#' month of the first season.
#'
#' @inheritParams subsample
#' 
#' @return A vector of named integers representing the season for each date in `dates`.
#' 
#' @examples
#' date_vec <- as.Date(c("2020-01-01", "2020-03-01", "2020-06-01", "2020-09-01", "2020-12-01"))
#' streamsampler:::calc_seasons(date_vec)
#'
#' date_vec <- as.Date(c("2020-01-01", "2020-06-01", "2020-07-01", "2020-12-01"))
#' streamsampler:::calc_seasons(date_vec, n_seasons = 2)
#'
#' date_vec <- as.Date(c("2020-01-01", "2020-03-01", "2020-07-01", "2020-09-01", "2020-12-01"))
#' streamsampler:::calc_seasons(date_vec, season_start = 7)
#'
#' date_vec <- seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "month")
#' streamsampler:::calc_seasons(date_vec)
#'
#' date_vec <- as.Date(c("2020-12-31", "2021-01-01"))
#' streamsampler:::calc_seasons(date_vec)
#'
#' date_vec <- as.Date(c("2020-01-01", "2020-04-01", "2020-08-01", "2020-12-01"))
#' streamsampler:::calc_seasons(date_vec, n_seasons = 3, season_start = 4)
#' @keywords internal

calc_seasons <- function(dates, n_seasons = 4, season_start = 10) {
  
  season_len <- 12 / n_seasons
  season_vec <- rep(1:n_seasons, each = season_len)
  if (season_start != 1) {
    names(season_vec) <- sprintf("%02d", c(season_start:12, 1:(season_start - 1)))
  } else {
    names(season_vec) <- sprintf("%02d", 1:12)
  }

  seasons <- season_vec[format(dates, "%m")]
  
  return(seasons)
  
}