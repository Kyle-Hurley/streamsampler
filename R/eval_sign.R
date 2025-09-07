#' Evaluate the sign of data
#'
#' @description 
#' Evaluate a numeric vector for the proportions of positive (> 0), negative (<= 0), and NA values. 
#' Intended to be used on a water quality record.
#'
#' @param values A numeric vector to be evaluated.
#' 
#' @return A data frame with one row and seven columns:
#' \tabular{lll}{
#' **Name** \tab **Type** \tab **Description** \cr
#' n_pos \tab integer \tab number of non-negative values in the data \cr
#' pos_pct \tab numeric \tab percentage of non-negative values in the data \cr
#' n_neg \tab integer \tab number of negative values in the data \cr
#' neg_pct \tab numeric \tab percentage of negative values in the data \cr
#' n_na \tab integer \tab number of NA values in the data \cr
#' na_pct \tab numeric \tab percentage of NA values in the data \cr
#' tot_pct \tab numeric \tab sum of `pos_pct`, `neg_pct`, and `na_pct` \cr
#' }
#' 
#' @examples
#' data <- c(-1, -2, 0, 1, 2, 3, -3, -4, 4)
#' eval_sign(data)
#'
#' data_all_positive <- c(1, 2, 3, 4, 5)
#' eval_sign(data_all_positive)
#'
#' data_all_negative <- c(-1, -2, -3, -4, -5)
#' eval_sign(data_all_negative)
#'
#' data_mixed <- c(-1, 0, 1, 2, -2, -3, NA)
#' eval_sign(data_mixed)
#' @export

eval_sign <- function(values) {
  
  if (!inherits(values, "numeric")) {
    stop("Input 'values' must be 'numeric' class")
  }
  
  n <- length(values)
  na_values <- values[is.na(values)]
  n_na <- sum(is.na(na_values))
  if (is.na(n_na)) n_na <- 0
  na_pct <- (n_na / n) * 100
  
  n_pos <- sum(values[!is.na(values)] > 0)
  if (is.na(n_pos)) n_pos <- 0
  pos_pct <- (n_pos / n) * 100
  
  n_neg <- sum(values[!is.na(values)] <= 0)
  if (is.na(n_neg)) n_neg <- 0
  neg_pct <- (n_neg / n) * 100
  
  return(
    data.frame(
      "n_pos" = n_pos, 
      "pos_pct" = pos_pct, 
      "n_neg" = n_neg, 
      "neg_pct" = neg_pct, 
      "n_na" = n_na, 
      "na_pct" = na_pct, 
      "tot_pct" = sum(pos_pct, neg_pct, na_pct, na.rm = TRUE)
    )
  )
  
}