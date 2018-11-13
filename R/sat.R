#' Market_capping
#'
#' @param mv Vector of market values
#' @param capped_wts Vector of weights to cap each market values
#'
#' @return A datafrane of results containing capped market values, capped weights, scaling factors and rescaled scaling factors (see Note)
#'
#' @note When a country has zero market value, its scaling factor will be assigned 0, as opposed to 1 per the scaling factor spreadsheet convention, as it would give a more "correct" rescaled scaling factor.
#' @note Rescaled scaling factors ensures that the largest factor is 1 by rescaling scaling factors.
#' @export
#'
#' @examples data(mvs)
#' @examples data(caps)
#' @examples market_capping(mvs, caps)
market_capping <- function(mv, capped_wts){
  #check mv_cap_wts are valid
  if (sum(capped_wts < 0 )){
    stop("There should not be any negative weights in capped_wts.")
  }

  if (sum(capped_wts > 1 )){
    stop("There should not be any weights > 100% in capped_wts.")
  }

  #mv_cap_wts should be same length as mv
  if (length(mv) != length(capped_wts)){
    stop("mv and capped_mv_wts should be of same length.")
  }

  #calculate uncapped weights
  total_mv <- sum(mv)
  uncapped_wts <- mv / total_mv

  #check if any weight caps are violated
  mv_caps <- total_mv * capped_wts
  allocated_mv <- total_mv * uncapped_wts
  reallocation_wts <- rep(1, length(mv))

  while (sum(allocated_mv > mv_caps) > 0 ){
    #calculate overallocated mvs
    overallocated_mv <- allocated_mv - mv_caps
    overallocated_mv[overallocated_mv <0] <- 0

    #calculate wts for re-allocation
    violated_rows <- which(allocated_mv > mv_caps)
    unviolated_rows <- which(allocated_mv <= mv_caps)
    reallocation_wts[violated_rows] <- rep(0, length(violated_rows))
    reallocation_wts[unviolated_rows] <- uncapped_wts[unviolated_rows] / sum(uncapped_wts[unviolated_rows])

    #subtract overallocation
    allocated_mv <- allocated_mv - overallocated_mv

    #apply overallocation to remaining countries
    mv_to_reallocate <- sum(overallocated_mv) * reallocation_wts
    allocated_mv <- allocated_mv + mv_to_reallocate
  }

  capped_mv <- allocated_mv
  capped_mv_wts <- capped_mv / sum(capped_mv)
  scaling_factors <- capped_mv / mv
  #if mv is zero (e.g. when country is not in benchmark), we will get a divide by 0 hence NA. Here we replace with 1s per
  #spreadsheet's convention.
  scaling_factors[is.na(scaling_factors)] <- 0

  rescaled_sf <- scaling_factors / max(scaling_factors)

  #Some additional checks on output
  stopifnot(
    sum(uncapped_wts) == 1,
    abs(sum(capped_mv) - total_mv) < 1e-3,
    sum(capped_mv_wts) - 1 < 1e-10,
    max(rescaled_sf) == 1
  )

  # output <- cbind(mv, capped_wts, uncapped_wts, capped_mv, capped_mv_wts, scaling_factors, rescaled_sf)
  # colnames(output) <- c("mv", "mv_cap_wts", "uncapped_wts", "capped_mv", "capped_mv_wts", "scaling_factors", "rescaled_sf")
  output <- data.frame(mv, capped_wts, uncapped_wts, capped_mv, capped_mv_wts, scaling_factors, rescaled_sf)
  return(output)
}

#' mvs
#' @name mvs
#' @docType data
#' @details Sample market value
NULL

#' caps
#' @name caps
#' @docType data
#' @details Sample market value caps (as weights)
NULL
