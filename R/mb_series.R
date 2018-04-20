library(MacrobondAPI)

#Update time series automatically from a live source
AppendTimeSeriesAuto <- function(private_name, live_name){
  #Get existing data
  my_old_data <- FetchOneTimeSeries(private_name)
  if (getIsError(my_old_data))
    stop(getErrorMessage(my_old_data))

  #Get new data
  my_new_data <- FetchOneTimeSeries(live_name)
  if (getIsError(my_new_data))
    stop(getErrorMessage(my_new_data))

  #Union and take
  my_old_data.ts <- xts(getValues(my_old_data), order.by = getDatesAtStartOfPeriod(my_old_data))
  my_new_data.ts <- xts(getValues(my_new_data), order.by = getDatesAtStartOfPeriod(my_new_data))
  merge(my_old_data.ts, my_new_data.ts)

  #Backup old data to separate series with time stamps?

}

#Update time series manually by providing dates and values
AppendTimeSeriesManual <- function (name, description, region, category, frequency, live_source){
  my_data <- FetchOneTimeSeries(live_source)
  if (getIsError(my_data))
    stop(getErrorMessage(my_data))
}

#Download old series



#Append to
seriesNew <- CreateTimeSeriesObject(name = "ih:mb:com:s1", #Use "ih:mb:com:xx" for company account, "ih:mb:priv:xx" for private account. Update xx to series code.
                                    description = "My forecast", #Title of series
                                    region = "us", #See help for list of region codes
                                    category = "Forecasts",
                                    frequency = "Monthly",
                                    startDateOrDates = as.Date("1990-01-01"),
                                    values = c(12.2, 12.7, 12.8, 13.0)
                                    )
UploadOneOrMoreTimeSeries(seriesNew)

tmp <- list(name = "ih:mb:com:s1", #Use "ih:mb:com:xx" for company account, "ih:mb:priv:xx" for private account. Update xx to series code.
      description = "My forecast", #Title of series
      region = "us", #See help for list of region codes
      category = "Forecasts",
      frequency = "Monthly")
cat(tmp$name)

