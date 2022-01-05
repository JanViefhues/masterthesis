library(tidyverse)
library(psych)
library(dplyr)
library(lubridate)
library(xts)
library(ggplot2)
library(TSstudio)
library(Rpdb)


precipitation_data <- read.csv("/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/weather_data/produkt_rr_stunde_20200425_20211026_15000.txt", sep=";")

# Transform date column into roght format. Since it is bad format, it was quiet complicated
precipitation_data <- transform(precipitation_data, MESS_DATUM = as.character(MESS_DATUM))
for (i in 1:length(precipitation_data$MESS_DATUM)){
  precipitation_data$MESS_DATUM[i] = paste(substr(precipitation_data$MESS_DATUM[i], 1, 4),"-",substr(precipitation_data$MESS_DATUM[i], 5, 6),'-',substr(precipitation_data$MESS_DATUM[i], 7, 8), ' ', substr(precipitation_data$MESS_DATUM[i], 9, 10),':00:00',  sep = "" )
}
precipitation_data<- data.frame(precipitation_data)
precipitation_data$MESS_DATUM <- as.POSIXct(precipitation_data$MESS_DATUM, format="%Y-%m-%d %H:%M:%S")
precipitation_data <- precipitation_data[, !names(precipitation_data) %in% c('STATIONS_ID', 'QN_8', 'QN_8', 'RS_IND', 'WRTR', 'eor')]
colnames(precipitation_data) <- c('Timestamp','precipitation')

# !!! I saw NAs in the dataset !!!
precipitation_data <- na.omit(precipitation_data) 
tail(precipitation_data)
write.csv(precipitation_data, "/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/weather_data/precipitation_data_cleaned.csv")

data_as_xts <- as.xts(precipitation_data["precipitation"], order.by = precipitation_data$Timestamp)
precipitation_data_during_flooding_cleaned <- data_as_xts["2021-06-01/2021-08-01"]

setwd('/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/weather_data/')
getwd()
write.csv(precipitation_data_during_flooding_cleaned, "precipitation_data_during_flooding_cleaned.csv")
head(precipitation_data_during_flooding_cleaned)

## Descriptives of the precipitation data

# Summary statistics, excluding the Date column
precipitation_data_during_flooding_cleaned.summary <- describe(precipitation_data_during_flooding_cleaned)
## Select only the values we want to see
precipitation_data_during_flooding_cleaned.summary <- precipitation_data_during_flooding_cleaned.summary[c('n', 'mean', 'sd','median', 'min', 'max')]
print(precipitation_data_during_flooding_cleaned.summary)
ts_plot(precipitation_data_during_flooding_cleaned$precipitation)


hist(precipitation_data_during_flooding_cleaned$precipitation)
