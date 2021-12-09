library(tidyverse)
library(dplyr)
data <- read.csv('/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/mineWaterLevels/Mineshafts\ water\ levels.csv', sep=";")
# Mine Water Stations
station.WILLEM <-  data[data$Location == 'WILLEM',]
station.WILLEM $stationTyp <- 'Mine Well'
station.WILLEM$mine_water_level <- as.numeric(gsub(",", ".", gsub("\\.", "", station.WILLEM$Water.level..m.surface.level.)))
station.WILLEM$Water.level..m.surface.level. <- NULL
station.WILLEM$Location <- NULL
station.WILLEM$stationTyp <- NULL


# Ground Water Stations
station.B62B0921 <- data[data$Location == 'B62B0921',]
station.B62B0921$stationTyp <- 'Groundwater Well'
station.B62B0921$ground_water_level <- as.numeric(gsub(",", ".", gsub("\\.", "", station.B62B0921$Water.level..m.surface.level.)))
station.B62B0921$Water.level..m.surface.level. <- NULL
station.B62B0921$Location <- NULL
station.B62B0921$stationTyp <- NULL

# Transform Date column to be in right format

transform_me <- function(dataset){
  for (i in 1:nrow(dataset)){
    first_part<- substr( dataset[i,]$Date, 1, 6)    
    middle_part <- "20"
    last_part <- substr( dataset[i,]$Date, 7, 8)    
    dataset[i,]$Date <- paste0(first_part, middle_part, last_part)
  }
  return(dataset)
}

station.B62B0921 <- transform_me(station.B62B0921)
station.WILLEM <- transform_me(station.WILLEM)

station.B62B0921$Date <- as.Date(station.B62B0921$Date, format = "%d.%m.%Y")
station.WILLEM$Date <- as.Date(station.WILLEM$Date, format = "%d.%m.%Y")

# B62B0921 has a lot of duplicated rows,  have to remove them
# The falsely dataset starts at index 969
station.B62B0921 <- station.B62B0921[1:968,]

# Missing values in the WILLEM dataset
View(station.WILLEM)
ggplot(data=station.WILLEM, aes(x=Date, y=mine_water_level, group=1)) +
  geom_line()+
  geom_point()

# From 17.04.2020 until 06.06.2020
# Using the average from both dates to be the value

# Get the dates for missing values and add it to the df
missing_dates = seq(as.Date("2020-04-18"), as.Date("2020-06-07"), by="days")
start_date <- station.WILLEM[station.WILLEM$Date == '2020-04-17',]
end_date <- station.WILLEM[station.WILLEM$Date == '2020-06-06',]
avg <- start_date$mine_water_level + ((end_date$mine_water_level - start_date$mine_water_level) / length(missing_dates))


add_missing_dates = function(dates, dataset){
  for(i in 1:length(dates)){
    df <- data.frame(dates[i], avg)
    colnames(df) <- c('Date', "mine_water_level")
    dataset<- rbind(dataset, df)
  }
  return(dataset[order(dataset$Date),])
}



total.dataset <- merge(station.WILLEM, station.B62B0921)


# Select the correct timeframe
total.dataset.xts <- xts::as.xts(total.dataset, order.by = lubridate::as_datetime(x = total.dataset$Date,tz = "Europe/Amsterdam"))
total.dataset.xts <- total.dataset.xts['2020-08-25 00:00:00/2021-09-24']


write.csv(station.WILLEM, '/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/mineWaterLevels/station_WILLEM.csv')
write.csv(station.B62B0921, '/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/mineWaterLevels/station_B62B0921.csv')
write.csv(total.dataset.xts,'/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/datasets/cleaned_datasets/cleaned_minewater_and_groudnwater_data.csv' )

