#' Market_capping
#'
#' @param mv Vector of market values
#' @param capped_wts Vector of weights to cap each market values
#'
#' @return A vector of results containing capped market values, capped weights, scaling factors and rescaled scaling factors (see Note)
#' @export
#'
#' @examples data(mvs)
#' @examples data(caps)
#' @examples market_capping(mvs, caps)
market_capping <- function(mv, capped_wts){
  #check mv_cap_wts are valid
  if (sum(capped_wts < 0 )){
    stop("There should not be any negative weights in capped_wts")
  }

  if (sum(capped_wts > 1 )){
    stop("There should not be any weights > 100% in capped_wts")
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
  rescaled_sf <- scaling_factors / max(scaling_factors)
  output <- cbind(mv, capped_wts, uncapped_wts, capped_mv, capped_mv_wts, scaling_factors, rescaled_sf)
  colnames(output) <- c("mv", "mv_cap_wts", "uncapped_wts", "capped_mv", "capped_mv_wts", "scaling_factors", "rescaled_sf")
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
