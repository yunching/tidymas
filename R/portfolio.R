#' Calculate Z-score
#'
#' @param my_ts Time series to calculate z-score
#'
#' @return Z-scores
#' @export
#'
#' @examples
#' z_score(1:10)
#' #[1] -1.4863011 -1.1560120 -0.8257228 -0.4954337 -0.1651446  0.1651446  0.4954337  0.8257228  1.1560120  1.4863011
z_score <- function(my_ts){
  my_mean <- mean(my_ts)
  my_sd <- sd(my_ts)

  return((my_ts - my_mean)/my_sd)
}
