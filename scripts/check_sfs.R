library(purrr)

##Checks historical scaling factors
#load historical mvs
hist_mv <- read.csv("./data2/sf_check.csv", nrows=12)
rownames(hist_mv) <- hist_mv[,1]
hist_mv <- hist_mv[,2:length(hist_mv)]
hist_mv

#load historical caps
hist_cap <- read.csv("./data2/sf_check.csv", nrows=12, skip=30)
rownames(hist_cap) <- hist_cap[,1]
hist_cap <- hist_cap[,2:length(hist_cap)]
hist_cap

#load historical wts
hist_wts <- read.csv("./data2/sf_check.csv", nrows=12, skip=16)
rownames(hist_wts) <- hist_wts[,1]
hist_wts <- hist_wts[,2:length(hist_wts)]
hist_wts

#calc sfs from historical mvs
results <- purrr::map2(hist_mv, data.frame(hist_cap), tidymas::market_capping)
calculated_wts <- purrr::map(results, "capped_mv_wts") #%>% purrr::map(function(x) round(x, 2))
countries <- c(
  "Australia",
  "Belgium",
  "Canada",
  "France",
  "Germany",
  "Italy",
  "Japan",
  "Netherlands",
  "S. Korea",
  "Spain",
  "United Kingdom",
  "United States"
)
calculated_wts <- data.frame(calculated_wts, row.names=countries)
diff <- round(calculated_wts - hist_wts, 3)
diff

#compare sfs with historical sfs

