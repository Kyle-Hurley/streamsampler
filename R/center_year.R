#' Find middle year in a sequence of years
#'
#' @param years Vector of a sequence of years
#' @param start_year The first year of the record
#' @param end_year The last year of the record
#' @param half_win The half-window size, specifying the range of years to be maintained, 
#' particularly from the start and end years.
#'
#' @return Vector of the center years
#' 
#' @examples
#' years <- c(2005, 2010, 2015)
#' start_year <- 2000
#' end_year <- 2020
#' half_win <- 5
#' streamsampler:::center_year(years, start_year, end_year, half_win)
#'
#' years <- c(1995, 2000, 2002)
#' start_year <- 2000
#' end_year <- 2020
#' half_win <- 5
#' streamsampler:::center_year(years, start_year, end_year, half_win)
#'
#' years <- c(2018, 2021, 2025)
#' start_year <- 2000
#' end_year <- 2020
#' half_win <- 5
#' streamsampler:::center_year(years, start_year, end_year, half_win)
#'
#' years <- c(2000, 2020)
#' start_year <- 2000
#' end_year <- 2020
#' half_win <- 5
#' streamsampler:::center_year(years, start_year, end_year, half_win)
#'
#' years <- 2010
#' start_year <- 2000
#' end_year <- 2020
#' half_win <- 5
#' streamsampler:::center_year(years, start_year, end_year, half_win)
#' @keywords internal

center_year <- function(years, start_year, end_year, half_win = 2) {
  
  if (length(years) == 1) {
    return(years)
  }
  
  max_lim <- (start_year + half_win)
  year_max <- max(years, na.rm = TRUE)
  min_lim <- (end_year - half_win) 
  year_min <- min(years, na.rm = TRUE)
  if (max_lim > year_max || min_lim < year_min) {
    warning("Half window is larger than year range. Try a smaller half_win. Returning year vector.")
    return(years)
  }
  
  if (start_year < year_min || end_year > year_max) {
    warning("Start and end year outside year range. Unexpected behavior may occur.")
  }
  
  center_years <- pmin(pmax(years, start_year + half_win), end_year - half_win)
  
  return(center_years)
  
}