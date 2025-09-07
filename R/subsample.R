#' Convert a daily record to discrete
#' 
#' @description
#' Sample a daily or near-daily data set to one containing infrequent but periodic records based on 
#' a random sampling protocol. Intended to be used on a (near) daily water quality record.
#' 
#' @param dates A vector of dates of 'Date' class.
#' @param values Numeric values. The values must be in correspondence with `dates`, meaning the 
#' *i*th element in `values` must correspond to the *i*th date in `dates`.
#' @param thresh_ref Numeric values to calculate a threshold. See 'Details'. The values must be in 
#' correspondence with `dates`, meaning the `i`th element in `thresh_ref` must correspond to the 
#' `i`th date in `dates`.
#' @param season_start The starting month of the first season, specified as an integer from 1 to 
#' 12. Default is 10 (October).
#' @param n_seasons The number of seasons in a year. Must be a factor of 12. Default is 4.
#' @param half_win The half width of the window of years to group `thresh_ref` by. See 'Details'.
#' @param threshold The quantile of `thresh_ref` above which `values` is sampled `n_et_samples` 
#' times per year.
#' @param n_samples Integer of the number of below-threshold samples to be selected from `values` 
#' at a frequency defined by `freq`.
#' @param freq Character of the frequency at which below-threshold samples are selected. May be 
#' "week", "month", "quarter", or "year". See 'Details'.
#' @param n_et_samples Integer of the number of yearly exceeds-threshold samples to be selected 
#' from `values`. See 'Details'.
#' @param season_weights A vector of integers of the weights to assign to seasons for random sampling of exceeds-threshold 
#' `values`. Based on the rank of the seasonal average `thresh_ref` (from highest to lowest). Must 
#' have length equal to the number of seasons (`n_seasons`). See 'Details'.
#' @param target One of "none" or "peaks". See 'Details'.
#' @param look_behind,look_ahead When `target` is "peaks", the number of `look_units` before and 
#' after the center date to include in the sliding window to determine local maxima.
#' @param look_units One of "days", "weeks", or "months". The units to give `look_ahead` and 
#' `look_behind`.
#' @param seed An integer which determines the state for the random number generator. Ensures 
#' random sampling is reproducible.
#' 
#' @details
#' `values` are randomly selected based on a provided sampling protocol using `dates` as an index 
#' and `thresh_ref` as a classifier. Elements in `values` equal to or less than their seasonal 
#' threshold are randomly sampled according the protocol set by `n_samples` and `freq`. 
#' `n_et_samples` elements in `values` greater than the threshold are randomly sampled for each 
#' year in `values`. This results in `n_samples` of below-threshold values for each unique `freq` 
#' and `n_et_samples` of exceeds-threshold values for each unique year.
#' 
#' Elements in `values` and `thresh_ref` must correspond with their respective values in `dates`.
#' 
#' `subsample()` is psuedo-random across time in that values are selected randomly in rolling 
#' chunks of time determined by `freq`. If, for example, `freq` is "week" and `n_samples` is 1, 
#' then the result will be 1 randomly selected below-threshold, non-NA value for each week. 
#' However, the selected values could be very close in time (e.g., Saturday and Sunday). 
#' 
#' Thresholds are calculated based on groupings of seasonally adjusted years, accounting for 
#' seasons split across years. For example, if `n_seasons = 4` and `season_start = 12`, then 
#' season 1 includes December of e.g. 2020, January 2021, and February 2021. The year is considered 
#' to begin in December and is designated by the year in which it ends (i.e., the seasonally 
#' adjusted year); 2021 in this example. If `half_win` is `2`, the default, then a total of 5 years 
#' is used to calculate the threshold. For example, when `half_win` is 2, the threshold for season 
#' 1 of 2021 is the quantile defined by `threshold` of all season 1 values in 2019, 2020, 2021, 
#' 2022, and 2023.
#' 
#' The selection of exceeds-threshold values is always across an entire year with no 
#' guarantee of timing between selected values. Setting `threshold` to values near 1 would result 
#' in a smaller sample pool since there would conceivably be fewer values above 0.9 than 0.8 - thus 
#' increasing the likelihood of selected exceeds-threshold values being "far" apart in time.
#' 
#' Both `n_samples` and `n_et_samples` are adjusted lower when the number of unique `dates` in 
#' the defined `freq` is less than the number of unique dates in a complete `freq`. This adjustment 
#' is calculated by multiplying the number of unique dates in the given `freq` and the number of 
#' `*_samples`, dividing the number of dates in the complete `freq`, and then rounding to the 
#' nearest whole number. For example, when `n_samples` is 2 and `freq` is “week” but only 1 unique 
#' sample date exists for a given week, then `n_samples` is adjusted to 1 ((1 * 2) / 7) --> 1).
#' 
#' `season_weights` influences the random sampling of exceeds-threshold values by weighting the 
#' values according to the rank of the seasonal average of `thresh_ref` for the respective adjusted 
#' year. For example, if `n_seasons` is 2 and `season_weights` is c(2, 1) then each season with the 
#' highest seasonal average of `thresh_ref` values is given a weight of 2 and each season with the 
#' lowest is given a weight of 1 - making the exceeds-threshold `values` occurring in the highest 
#' ranking seasons more likely to be selected than if the weight was 1. See the details for the 
#' `prob` argument in [sample()] for more information.
#' 
#' When `target` is "none", the random selection of exceeds-threshold values is influenced only by 
#' `season_weights`. When "peaks", the weights are doubled for values corresponding to local 
#' maxima, exceeds-threshold `thresh_ref` values.
#' 
#' @return A data.frame with the following columns:
#' \tabular{lll}{
#' **Name** \tab **Type** \tab **Description** \cr
#' date \tab Date \tab Date \cr
#' adj_year \tab integer \tab Adjusted year \cr
#' season \tab integer \tab Season number '1':n_seasons \cr
#' value \tab numeric \tab Input `values` \cr
#' thresh_ref \tab numeric \tab Input `thresh_ref` values \cr
#' threshold \tab numeric \tab Seasonal `threshold` quantile of `thresh_ref` \cr
#' is_peak \tab logical \tab TRUE when `thresh_ref` value is local maximum. Only when `target` is 
#' "peaks". TRUE/FALSE \cr
#' selection_type \tab character \tab Type of randomly selected value "not_selected" (a record not 
#' sampled), "below_threshold" (sampled record with value at or below threshold), or 
#' "exceeds_threshold" (sampled record with value above threshold) \cr
#' weight \tab integer \tab Weight assigned to the value  \cr
#' ys_rank \tab integer \tab Unique year-season rank of the seasonal average `thresh_ref` \cr
#' }
#'
#' @examples
#' # Randomly sample using defaults
#' df <- subsample(
#'   dates = streamdat$date, values = streamdat$sc, thresh_ref = streamdat$q, 
#' )
#' # Plotting function
#' create_plot <- function(df, log_xfrm = "x", xlab, ylab) {
#'   
#'   plot(
#'     df$thresh_ref[df$selection_type == "not_selected"], 
#'     df$value[df$selection_type == "not_selected"], 
#'     pch = 21, col = "gray", 
#'     xlab = xlab, ylab = ylab, 
#'     main = "Subsampled Daily Data", 
#'     log = log_xfrm
#'   )
#'   points(
#'     df$thresh_ref[df$selection_type != "not_selected"], 
#'     df$value[df$selection_type != "not_selected"], 
#'     pch = 16, cex = 1.5, 
#'     col = c(
#'       "below_threshold" = "#42047E", 
#'       "exceeds_threshold" = "#07A49E"
#'     )[df$selection_type[df$selection_type != "not_selected"]]
#'   )
#'   legend(
#'     "topright", 
#'     legend = c("Not Sampled", "Below Threshold", "Exceeds Threshold"), 
#'     col = c("gray", "#42047E", "#07A49E"), 
#'     pch = c(21, 16, 16), 
#'     bty = "n"
#'   )
#' }
#' # Plot
#' create_plot(df, "x", "Discharge (CFS)", "Specific Conductivity (uS/cm)")
#' 
#' 
#' df <- subsample( 
#'   dates = streamdat$date, values = streamdat$sc, thresh_ref = streamdat$q, 
#'   target = "peaks", 
#'   season_weights = c(2, 1, 1, 1) # default is four seasons
#' )
#' create_plot(df, "x", "Discharge (CFS)", "Specific Conductivity (uS/cm)")
#' 
#' df <- subsample( 
#'   dates = streamdat$date, values = streamdat$sc, thresh_ref = streamdat$sc, 
#'   target = "peaks", 
#'   n_samples = 1, freq = "week", 
#'   n_et_samples = 12, 
#'   half_win = 3
#' )
#' df <- merge(streamdat, df, by.x = c("date", "sc"), by.y = c("date", "value"))
#' df <- df[, !colnames(df) %in% c("thresh_ref")]
#' colnames(df)[c(2, 3)] <- c("value", "thresh_ref")
#' create_plot(df, "x", "Discharge (CFS)", "Specific Conductivity (uS/cm)")
#' @export

subsample <- function(dates, values, thresh_ref, season_start = 10, n_seasons = 4, half_win = 2, 
                      threshold = 0.8, n_samples = 1, freq = "month", n_et_samples = 8, 
                      season_weights = rep(1, n_seasons), target = "none", look_behind = 2, 
                      look_ahead = 2, look_units = "days", seed = 123) {
  
  err_check <- check_subsample_inputs(
    dates, values, thresh_ref, season_start, 
    n_seasons, half_win, threshold, n_samples, 
    freq, n_et_samples, season_weights, target, 
    look_behind, look_ahead, look_units, seed
  )
  if (err_check) stop(attr(err_check, "err_msg"))
  
  look_ahead <- as.difftime(look_ahead, units = look_units)
  look_behind <- as.difftime(look_behind, units = look_units)
  
  df <- data.frame("date" = dates, "value" = values, "thresh_ref" = thresh_ref)
  df <- df[order(df[["date"]]), ]
  
  thresh <- thresholds(
    dates = dates, 
    values = thresh_ref, 
    season_start = season_start, 
    n_seasons = n_seasons, 
    half_win = half_win, 
    threshold = threshold
  )
  
  df[["adj_year"]] <- ifelse(
    as.numeric(format(df[["date"]], "%m")) >= season_start & season_start > 1, 
    as.numeric(format(df[["date"]], "%Y")) + 1, 
    as.numeric(format(df[["date"]], "%Y"))
  )
  
  df[["season"]] <- calc_seasons(
    dates = df[["date"]], 
    n_seasons = n_seasons, 
    season_start = season_start
  )
  
  df[["center_year"]] <- center_year(
    years = df[["adj_year"]], 
    start_year = min(df[["adj_year"]], na.rm = TRUE), 
    end_year = max(df[["adj_year"]], na.rm = TRUE), 
    half_win = half_win
  )
  
  df <- merge(df, thresh, by = c("season", "center_year"))
  df <- df[order(df[["date"]]), ]
  row.names(df) <- 1:nrow(df)
  
  df[["excd_thresh"]] <- df[["thresh_ref"]] > df[["threshold"]]
  df[is.na(df[["excd_thresh"]]), "excd_thresh"] <- FALSE

  dates_below_threshold <- df[!df[["excd_thresh"]] & !is.na(df[["value"]]), "date"]
  
  date_groups <- switch(
    freq, 
    week = {
      groups <- format(dates_below_threshold, "%Y-%U")
      split(dates_below_threshold, groups)
    }, 
    month = {
      groups <- format(dates_below_threshold, "%Y-%m")
      split(dates_below_threshold, groups)
    }, 
    quarter = {
      groups <- paste0(
        format(dates_below_threshold, "%Y"), 
        quarters(dates_below_threshold)
      )
      split(dates_below_threshold, groups)
    }, 
    year = {
      groups <- format(dates_below_threshold, "%Y")
      split(dates_below_threshold, groups)
    }
  )
  
  below_thresh_sample_dates <- lapply(
    date_groups, 
    \(group_dates, freq, n_size, seed) {
      n_size <- adjust_n(dates = group_dates, freq = freq, n_size = n_size)
      if (length(group_dates) >= n_size) {
        set.seed(seed)
        sample(group_dates, n_size)
      } else {
        group_dates
      }
    }, 
    freq = freq, 
    n_size = n_samples, 
    seed = seed
  )
  below_thresh_sample_dates <- as.Date(unlist(below_thresh_sample_dates, use.names = FALSE))
  
  df[["selection_type"]] <- ifelse(
    df[["date"]] %in% below_thresh_sample_dates, 
    "below_threshold", 
    "not_selected"
  )
  
  season_ranks <- summarize_seasons(
    dates = dates, values = thresh_ref, season_start = season_start, n_seasons = n_seasons
  )$seasonal
  df <- merge(df, season_ranks, by = c("adj_year", "season"))
  names(season_weights) <- 1:n_seasons
  df[["weight"]] <- season_weights[df[["ys_rank"]]]
  if (target == "peaks") {
    
    df[["is_peak"]] <- local_max_index(
      dates = df[["date"]], 
      values = df[["thresh_ref"]], 
      look_behind = as.numeric(look_behind),
      look_ahead = as.numeric(look_ahead), 
      look_units = look_units
    )
    
    df[["weight"]] <- ifelse(
      df[["is_peak"]] & df[["excd_thresh"]], 
      df[["weight"]] * 2, 
      df[["weight"]]
    )
    
  }
  
  df$year <- format(df[["date"]], "%Y")
  split_data <- split(df, df[["year"]])
  sampled_dates <- lapply(
    X = split_data, 
    FUN = subsample_et, 
    n_et_samples = n_et_samples, 
    seed = seed
  )
  sampled_dates <- as.Date(unlist(sampled_dates, use.names = FALSE))
  
  df[["selection_type"]] <- ifelse(
    df[["date"]] %in% sampled_dates, 
    "exceeds_threshold", 
    df[["selection_type"]]
  )
  
  if (target == "peaks") {
    cols_keep <- c(
      "date", "adj_year", "season", "value", "thresh_ref", 
      "threshold", "is_peak", "selection_type", "weight", "ys_rank"
    )
  } else {
    cols_keep <- c(
      "date", "adj_year", "season", "value", "thresh_ref", 
      "threshold", "selection_type", "weight", "ys_rank"
    )
  }
  df <- df[, cols_keep]
  
  return(df)
  
}

#' Subsample exceeds-threshold data
#' 
#' @description
#' Samples values that exceed the threshold
#' 
#' @inheritParams subsample
#' @param group Data frame to be subsampled. Must include columns named "date" ('Date'), "value" 
#' ('numeric'), "excd_thresh" (logical), and "selection_type" (character).
#' @keywords internal
  
subsample_et <- function(group, n_et_samples = 8, seed = 123) {
  
  non_na_dates <- group[!is.na(group[["value"]]), "date"]
  
  if (length(non_na_dates) == 0) {
    return(NULL)
  }
  
  # group$date_adj <- as.Date(paste0(group[["adj_year"]], "-", format(group[["date"]], "%m-%d")))
  # group <- group[order(group[["date_adj"]]), ]
  group <- group[order(group[["date"]]), ]
  
  n_et_samples <- adjust_n(dates = group[["date"]], freq = "year", n_size = n_et_samples)
  
  above_thresh <- group[["excd_thresh"]]
  not_na <- !is.na(group[["value"]])
  not_blw_thresh <- group[["selection_type"]] != "below_threshold"
  
  et_data <- group[above_thresh & not_blw_thresh & not_na, ]
  
  if (nrow(et_data) >= n_et_samples) {
    
    set.seed(seed)
    selected_dates <- sample(
      x = et_data[["date"]], 
      size = n_et_samples, 
      replace = FALSE, 
      prob = et_data[["weight"]]
    )
    return(selected_dates)
    
  } else if (nrow(et_data) > 0) {
    
    selected_dates <- et_data[["date"]]
    return(selected_dates)
    
  } else {
    
    return(NULL)
    
  }
  
}