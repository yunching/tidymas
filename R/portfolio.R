#' Calculate Z-score
#'
#' @param my_ts Time series to calculate z-score
#'
#' @return Z-scores
#' @export
#'
#' @examples
#' z_score(1:10)
z_score <- function(my_ts){
  my_mean <- mean(my_ts)
  my_sd <- stats::sd(my_ts)

  return((my_ts - my_mean)/my_sd)
}
