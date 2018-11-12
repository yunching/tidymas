library(purrr)

##Checks historical scaling factors
#load historical mvs
hist_mv <- read.csv("./data2/sf_check.csv", nrows=12)
rownames(hist_mv) <- hist_mv[,1]
hist_mv <- hist_mv[,2:length(hist_mv)]
hist_mv

#load historical sfs
hist_cap <- read.csv("./data2/sf_check.csv", nrows=12, skip=14)
rownames(hist_cap) <- hist_cap[,1]
hist_cap <- hist_cap[,2:length(hist_cap)]
hist_cap <- hist_cap / 100 #rescale to percentage
hist_cap

#calc sfs from historical mvs
#here I've just used no capping constraints
purrr::map2(hist_mv, data.frame(matrix(rep(1, 240), nrow = 12)), tidymas::market_capping)

#compare sfs with historical sfs
#TBC
