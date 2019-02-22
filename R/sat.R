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
#' @examples
#' data(mvs)
#' data(caps)
#' market_capping(mvs, caps)
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
  if (is.na(rating)) {
    rating <- NA
  }
  else if (stringr::str_detect(rating, stringr::regex("^(AAA|Aaa)"))){
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
  if (is.na(rating))
    return(NA)
  else if (rating == "AAA")
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
  else if (rating == "BBB-")
    return(10)
  else
    return(NA)
}

num_to_rating <- function(rating_num){
  if (is.na(rating_num))
    return(NA)
  else if (rating_num == 1)
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
  else if (rating_num == 10)
    return("BBB-")
  else
    return(NA)
}

#' Pad numbers with two zeros
#'
#' @param Number to be padded
#'
#' @return Number padded with two zeros
#' @export
#'
#' @details Helper function for using dates with Rblpapi
pad_2zeros <- function(Number){
  formatC(Number, width = 2, format = "d", flag = "0")
}

#' Download and calculate median credit rating over time
#' @importFrom magrittr "%>%"
#' @importFrom tidyr crossing
#' @importFrom stats median
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @importFrom scales date_format
#' @import ggplot2
#' @param end_date Last date to show credit ratings.
#' @param per No. of historical monthends to retrieve
#'
#' @return Returns a data frame to be used with plot_credit_ratings()
#' @details
#' This function rounds the end_date to the most recent monthend (in the past). Credit rating agencies
#' might re-rate countries anytime within a month, and in making this function we are standarding
#' the credit ratings of countries as at each monthend.
#' @export
#'
#' @examples
#' \donttest{
#' library(Rblpapi)
#' blpConnect()
#'
#' get_bm_ratings(end_date="2018-10-01", per=2)
#' }
get_bm_ratings <- function(end_date= Sys.Date(), per=6){
  #check end_date is a date
  if (!lubridate::is.Date(end_date)){
    stop("end_date requires Date datatype.")
  }

  dates <-
    #calculate monthends, standardising when ratings are shown monthly
    sort(seq(
      lubridate::floor_date(end_date, "1 month") - months(per-1),
      lubridate::floor_date(end_date, "1 month"),
      by = "1 month"
    ) - 1)

    sec_list <- c(
    "GTAUD10Y Govt",
    "GTBEF10Y Govt",
    "GTCAD10Y Govt",
    "GTFRF10Y Govt",
    "GTDEM10Y Govt",
    "GTITL10Y Govt",
    "GTJPY10Y Govt",
    "GTNLG10Y Govt",
    "GTKRW10Y Govt",
    "GTESP10Y Govt",
    "GTGBP10Y Govt",
    "GT10 Govt",
    "GTDKK10Y Govt",
    "GTATS10Y Govt",
    "GTPTE10Y Govt",
    "GTIEP10Y Govt",
    "GTMXN10Y Govt",
    "GTSEK10Y Govt",
    "GTNOK10Y Govt",
    "GTTHB10Y Govt",
    "GTPLN10Y Govt",
    "GTFIM10Y Govt",
    "GTNZD10Y Govt",
    "GTCHF10Y Govt",
    "GTHKD10Y Govt"
  )
  credit_rating_levels <- c("BBB-", "BBB", "BBB+",  "A-",  "A",  "A+",  "AA-",  "AA", "AA+", "AAA")
  generics_country_map <- tibble(
    sec = sec_list,
    country = c(
      "Australia",
      "Belgium",
      "Canada",
      "France",
      "Germany",
      "Italy",
      "Japan",
      "Netherlands",
      "S.Korea",
      "Spain",
      "United Kingdom",
      "United States",
      "Denmark",
      "Austria",
      "Portugal",
      "Ireland",
      "Mexico",
      "Sweden",
      "Norway",
      "Thailand",
      "Poland",
      "Finland",
      "New Zealand",
      "Switzerland",
      "Hong Kong"
    )
  )
  message(paste("Downloading data from", min(dates), "to", max(dates), ": this might take a while..."))

  #Prepare dates in format required by Bloomberg calls
  credit_rating_raw <- tibble::as_tibble(dates) %>%
    dplyr::rename(date = .data$value) %>%
    dplyr::mutate(
        padded_date = paste0(
        lubridate::year(.data$date),
        tidymas::pad_2zeros(lubridate::month(.data$date)),
        tidymas::pad_2zeros(lubridate::day(.data$date))
      )
    )

  #Pull data from Bloomberg
  credit_rating_raw <- purrr::map(credit_rating_raw$padded_date, function(x){
    Rblpapi::bdp(
      sec_list,
      c("RTG_MDY_LT_LC_DEBT_RATING", "RTG_SP_LT_LC_ISSUER_CREDIT", "RTG_FITCH_LT_LC_DEBT"),
      overrides = c("Rating_as_of_date_override" = x)
    )
  }) %>%
    #Tidy data collected
    purrr::map(~dplyr::mutate(.x, Ticker = sec_list)) %>%
    purrr::set_names(credit_rating_raw$date) %>%
    dplyr::bind_rows(.id = "date") %>%
    dplyr::transmute(date = as.Date(.data$date),
                  sec = .data$Ticker,
                  moody = .data$RTG_MDY_LT_LC_DEBT_RATING,
                  snp = .data$RTG_SP_LT_LC_ISSUER_CREDIT,
                  fitch = .data$RTG_FITCH_LT_LC_DEBT) %>%
    tibble::as_tibble()

  #Clean and compute internal credit ratings
  credit_rating <- credit_rating_raw %>%
    dplyr::group_by(.data$date, .data$sec) %>%
    dplyr::mutate(
      moody.clean = clean_rating(.data$moody),
      snp.clean = clean_rating(.data$snp),
      fitch.clean = clean_rating(.data$fitch),
      moody.num = rating_to_num(.data$moody.clean),
      snp.num = rating_to_num(.data$snp.clean),
      fitch.num = rating_to_num(.data$fitch.clean),
      mas_median_num = stats::median(c(.data$moody.num, .data$snp.num, .data$fitch.num)),
      mas_rating = num_to_rating(.data$mas_median_num) %>% factor(credit_rating_levels),
      country = generics_country_map$country[match(.data$sec, generics_country_map$sec)]
    )

  return(credit_rating)
}

#' Plot credit ratings evolution over time and facet by country
#'
#' @param my_data The data framme generated by get_bm_ratings()
#'
#' @return Returns a plot
#' @export
#'
#' @examples
#' \donttest{get_bm_ratings(start_date="2018-10-01", end_date = "2018-11-01") %>% plot_credit_ratings()}
plot_credit_ratings <- function(my_data){
  credit_rating_subtitle <- paste("From", min(my_data$date), "to", max(my_data$date))

  # wt_caps <- factor(c(
  #   "0%" = "dashed",
  #   "5%" = "dotdash",
  #   "10%" = "dotted"
  # ))

  credit_rating_levels <- c("BBB-", "BBB", "BBB+",  "A-",  "A",  "A+",  "AA-",  "AA", "AA+", "AAA")

  recent_ratings <- my_data %>%
    dplyr::ungroup() %>%
    dplyr::filter(date == max(.data$date)) %>%
    dplyr::select(.data$date, .data$country, .data$moody.clean, .data$snp.clean, .data$fitch.clean) %>%
    dplyr::rename(moody = .data$moody.clean, snp = .data$snp.clean, fitch = .data$fitch.clean)  %>%
    tidyr::gather("rater", "rating", -.data$date, -.data$country) %>%
    dplyr::mutate(rating = .data$rating %>% factor(credit_rating_levels) %>% forcats::fct_rev()) %>%
    dplyr::arrange(.data$country)

  my_data %>%
    ggplot(aes(x = .data$date, y = .data$mas_rating, col = .data$country, group=.data$country)) +
    geom_line() + geom_point() +geom_point(data=recent_ratings, aes(x=.data$date, y=.data$rating, shape=.data$rater), alpha=0.5, col="black") +
    labs(x = "Date",
         y = "Credit Rating",
         title = "Median credit rating over time for benchmark countries",
         col = "Country",
         subtitle = credit_rating_subtitle) +
    geom_hline(aes(yintercept=1, linetype=factor("0%", levels = c("0%", "5%", "10%"))), col="red") +
    geom_hline(aes(yintercept=3, linetype=factor("5%", levels = c("0%", "5%", "10%"))), col="red") +
    geom_hline(aes(yintercept=6, linetype=factor("10%", levels = c("0%", "5%", "10%"))), col="red") +
    facet_wrap(.data$country ~ .) +
    scale_x_date(labels = date_format("%m%y")) +
    scale_y_discrete(limits=forcats::fct_rev(credit_rating_levels)) +
    scale_linetype_manual(name="Weight caps", breaks=c("0%", "5%", "10%"), values=c(2, 3, 4)) +
    scale_color_discrete(guide=FALSE) +
    scale_shape_discrete(name = "Rating agency", breaks=c("fitch", "moody", "snp"), labels=c("Fitch", "Moody", "S&P"))
}
