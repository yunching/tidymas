library(Rblpapi)
library(lubridate)
library(tidyverse)
library(scales)
blpConnect()

dates <- sort(seq(as.Date("2018-10-31"), length=120, by="-1 month") - 1)
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
  "GT10 Govt"
)
credit_rating_levels <- c("AAA", "AA+", "AA", "AA-", "A+", "A", "A-", "BBB+", "BBB", "BBB-")
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
    "South Korea",
    "Spain",
    "United Kingdom",
    "US"
  )
)

credit_rating_raw <- crossing(date=dates, sec=sec_list) %>%
  mutate(
    padded_date = paste0(
      lubridate::year(date),
      tidymas::pad_2zeros(lubridate::month(date)),
      tidymas::pad_2zeros(lubridate::day(date))
    )
  ) %>%
  group_by(date, sec) %>%
  #Pull LT credit rating data from Bloomberg
  mutate(
    moody = Rblpapi::bdp(
      sec,
      "RTG_MDY_LT_LC_DEBT_RATING",
      overrides = c("Rating_as_of_date_override" = padded_date)
    )[1,1],
    snp = Rblpapi::bdp(
      sec,
      "RTG_SP_LT_LC_ISSUER_CREDIT",
      overrides = c("Rating_as_of_date_override" = padded_date)
    )[1,1],
    fitch = Rblpapi::bdp(
      sec,
      "RTG_FITCH_LT_LC_DEBT",
      overrides = c("Rating_as_of_date_override" = padded_date)
    )[1,1]
  )
#split BBG data retrival from rest of data tasks as bdp call element-wise is very slow
credit_rating <- credit_rating_raw %>%
  mutate(
    moody.clean = clean_rating(moody),
    snp.clean = clean_rating(snp),
    fitch.clean = clean_rating(fitch),
    moody.num = rating_to_num(moody.clean),
    snp.num = rating_to_num(snp.clean),
    fitch.num = rating_to_num(fitch.clean),
    mas_median_num = median(c(moody.num, snp.num, fitch.num)),
    mas_rating = num_to_rating(mas_median_num) %>% factor(credit_rating_levels),
    country = generics_country_map$country[match(sec, generics_country_map$sec)]
  )

wt_caps <- c(
  "5%" = "dotdash",
  "0%" = "dashed",
  "15%" = "dotted"
)

credit_rating_subtitle <- paste("From", min(credit_rating$date), "to", max(credit_rating$date))

credit_rating %>%
  mutate(mas_rating = fct_rev(mas_rating)) %>%
  ggplot(aes(x = date, y = mas_rating, col = country, group=country)) + geom_line() + geom_point() +
  labs(x = "Date",
       y = "Credit Rating",
       title = "Median credit rating over time for benchmark countries",
       col = "Country",
       subtitle = credit_rating_subtitle) +
  geom_hline(aes(yintercept=1, linetype="0%")) +
  geom_hline(aes(yintercept=3, linetype="5%")) +
  geom_hline(aes(yintercept=6, linetype="15%")) +
  facet_wrap(country ~ .) +
  scale_x_date(labels = date_format("%Y")) +
  scale_linetype_manual(name="Weight caps", values=wt_caps)


