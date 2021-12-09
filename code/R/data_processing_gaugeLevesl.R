library(dplyr)
library(tidyverse)
library(lubridate)
library(xts)


gauge_level.data <- read.csv(file = '/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/weather_data/gauge_levels_Rhein_CGN.csv', header = FALSE, sep=' ')

gauge_level.data <- gauge_level.data %>% dplyr::slice(-c(1:5))
head(gauge_level.data)

names(gauge_level.data) <- c('Date', 'RhineWaterLevel')
gauge_level.data[] <- lapply(gauge_level.data, gsub, pattern=';', replacement='')
gauge_level.data$RhineWaterLevel <- as.numeric(gauge_level.data$RhineWaterLevel)
gauge_level.data$Date <- as.POSIXct(gauge_level.data$Date, format = "%Y%m%d%H%M%S")
class(gauge_level.data$Date)

head(gauge_level.data)

# Remove wrong entries
gauge_level.data <- gauge_level.data[gauge_level.data$RhineWaterLevel > 0,]
print(xx)

# Group By Hour
hourly_data <-gauge_level.data %>%
  dplyr::group_by(Date=floor_date(Date, '1 hour')) %>%
  dplyr::summarize(RhineWaterLevel=mean(RhineWaterLevel))



# Create xts object
hourly_data <- na.omit(hourly_data)
gauge_level.data.as_xts_t <- xts::as.xts(hourly_data, order.by = lubridate::as_datetime(x = hourly_data$Date,tz = "Europe/Amsterdam"))

# Cut the data into rigth timeframe
start_date <- '2020-08-25 00:00:00'
end_date <- '2021-09-24 07:28:35'

gauge_level.data.as_xts_t <- gauge_level.data.as_xts_t['2020-08-25 00:00:00/2021-09-24 07:28:35']

gauge_level.data.as_xts_t <- data.frame(gauge_level.data.as_xts_t)

gauge_level.data.as_xts_t$Date <- lubridate::as_datetime(x = gauge_level.data.as_xts_t$Date,tz = "Europe/Amsterdam")
gauge_level.data.as_xts_t$RhineWaterLevel <- as.numeric(gauge_level.data.as_xts_t$RhineWaterLevel)

tail(gauge_level.data.as_xts_t)
write.csv(gauge_level.data.as_xts_t, "/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/datasets/cleaned_datasets/cleaned_water_level_dataset.csv")




plot(df)
hist(df$Value)

ggplot(df, aes(x = df$Date)) + 
  geom_line(aes(y =  df$Value), color = "darkred") 
  
