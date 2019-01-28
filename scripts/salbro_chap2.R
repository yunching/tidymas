library(tidyverse)

approx_f_m_n <- function(spot, m, n){
  spot <- enquo(spot)
  ans <- (n * !!spot[n] - m * !!spot[m]) / (n - m)
  return(ans)
}

tibble(period=1:10,
       spot = c(6, 7, 7.75, 8.31, 8.73, 9.05, 9.29, 9.47, 9.60, 9.7) / 100
       ) %>%
  mutate(cml_ret = (1 + spot) ^ period,
         one_yr_fd = if_else(
           #assign first one year forward rate to one year spot rate
           is.na(cml_ret / lag(cml_ret, 1) - 1),
           spot[1],
           cml_ret / lag(cml_ret, 1) - 1
         ),
         impl_spot_1_yr_fwd = lead((cml_ret / cml_ret[1]) ^ (1/ (period - 1)) - 1, 1),
         imp_1y_spt_chg = impl_spot_1_yr_fwd - spot
         # f_2_3 = approx_f_m_n(.data$spot, 2, 3)
        )
