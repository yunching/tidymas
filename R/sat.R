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

#'mkt_capping_hist_mv
#'@name mkt_capping_hist_mv
#'@docType data
#'@details Historical mvs to use for regression testing
NULL

#'mkt_capping_hist_wts
#'@name mkt_capping_hist_wts
#'@docType data
#'@details Historical capped mvs to use for regression testing
NULL

#'mkt_capping_hist
#'@name mkt_capping_hist_cap
#'@docType data
#'@details Historical caps to use for regression testing
NULL

clean_rating <- function(rating){
  if (stringr::str_detect(rating, stringr::regex("^(AAA|Aaa)"))){
    rating <- "AAA"
  }
  else if (stringr::str_detect(rating, stringr::regex("^(AA\\+|Aa1)"))){
    rating <- "AA+"
  }
  else if (stringr::str_detect(rating, stringr::regex("^(AA-|Aa3)"))){
    rating <- "AA-"
  }
  else if (stringr::str_detect(rating, stringr::regex("^(AA|Aa2)"))){
    rating <- "AA"
  }
  else if (stringr::str_detect(rating, stringr::regex("^(A\\+|A1)"))){
    rating <- "A+"
  }
  else if (stringr::str_detect(rating, stringr::regex("^(A-|A3)"))){
    rating <- "A-"
  }
  else if (stringr::str_detect(rating, stringr::regex("^(A|A2)"))){
    rating <- "A"
  }
  else if (stringr::str_detect(rating, stringr::regex("^(BBB\\+|Baa1)"))){
    rating <- "BBB+"
  }
  else if (stringr::str_detect(rating, stringr::regex("^(BBB-|Baa3)"))){
    rating <- "BBB-"
  }
  else if (stringr::str_detect(rating, stringr::regex("^(BBB|Baa2)"))){
    rating <- "BBB"
  }
  else {
    rating <- NA
  }

  return(rating)
}

rating_to_num <- function(rating){
  if (rating == "AAA")
    return(1)
  else if (rating == "AA+")
    return(2)
  else if (rating == "AA")
    return(3)
  else if (rating == "AA-")
    return(4)
  else if (rating == "A+")
    return(5)
  else if (rating == "A")
    return(6)
  else if (rating == "A-")
    return(7)
  else if (rating == "BBB+")
    return(8)
  else if (rating == "BBB")
    return(9)
  else
    return(NA)
}

num_to_rating <- function(rating_num){
  if (rating_num == 1)
    return("AAA")
  else if (rating_num == 2)
    return("AA+")
  else if (rating_num == 3)
    return("AA")
  else if (rating_num == 4)
    return("AA-")
  else if (rating_num == 5)
    return("A+")
  else if (rating_num == 6)
    return("A")
  else if (rating_num == 7)
    return("A-")
  else if (rating_num == 8)
    return("BBB+")
  else if (rating_num == 9)
    return("BBB")
  else
    return(NA)
}

pad_2zeros <- function(x){
  formatC(x, width = 2, format = "d", flag = "0")
}

#' Get MAS rating
#'
#' @param my_sec A list of Bloomberg tickers to calculate ratings
#' @param date Date to obtain rating
#'
#' @return Clean ratings and MAS rating
#' @export
#'
#' @examples
#' ## library(Rblpapi)
#' ## library(lubridate)
#' ## blpConnect()
#' ## get_mas_rating("GTGBP10Y Govt", ymd("20180101"))
#'
#' @importFrom magrittr "%>%"
get_mas_rating <- function(my_sec, date){

  # message("Year: ", lubridate::year(date))
  # message("Month: ", lubridate::month(date))
  # message("Day: ", lubridate::day(date))

  date_override <-
    paste0(
      lubridate::year(date),
      pad_2zeros(lubridate::month(date)),
      pad_2zeros(lubridate::day(date))
    )
  # message("Date override: ", date_override)
  moodys <-
    Rblpapi::bdp(
      my_sec,
      "RTG_SP_LT_LC_ISSUER_CREDIT",
      overrides = c("Rating_as_of_date_override" = date_override)
    )
  snp <-
    Rblpapi::bdp(
      my_sec,
      "RTG_MDY_LT_LC_DEBT_RATING",
      overrides = c("Rating_as_of_date_override" = date_override)
    )
  fitch <-
    Rblpapi::bdp(
      my_sec,
      "RTG_FITCH_LT_LC_DEBT",
      overrides = c("Rating_as_of_date_override" = date_override)
    )

  moodys.clean <- clean_rating(moodys)
  snp.clean <- clean_rating(snp)
  fitch.clean <- clean_rating(fitch)
  average_rating <-
    mean(
      rating_to_num(moodys.clean),
      rating_to_num(snp.clean),
      rating_to_num(fitch.clean)
    ) %>%
    round(0) %>%
    num_to_rating()

  output <- cbind(date, moodys, snp, fitch, moodys.clean, snp.clean, fitch.clean, average_rating)
  colnames(output) <- c("Date","Moodys", "S&P", "Fitch","Moodys.clean", "S&P.clean", "Fitch.clean", "MAS Avg")

  return(output)
}

