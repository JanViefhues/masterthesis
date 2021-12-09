library(readxl)
library(dplyr)
library(xts)
library(psych)


discharge_data <- read_excel("/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/weather_data/Limburg_water_data/dischargeRimburgWorm.xlsx")
discharge_data <- discharge_data %>% dplyr::slice(-c(1:4))
colnames(discharge_data) <- c("Date", "Discharge")
discharge_data.xts <- xts::as.xts(discharge_data, order.by = lubridate::as_datetime(x = discharge_data$Date,tz = "Europe/Amsterdam"))
discharge_data.xts <- discharge_data.xts['2020-08-25 00:00:00/2021-09-24 07:28:35']



# mean 2.63
# sd 2.36
# Outlier  7,35 (mean + 2 SD)
outlier_treshold <- 7.35
as.numeric(discharge_data.xts$Discharge)
outliers <- discharge_data.xts[as.numeric(discharge_data.xts$Discharge) >= outlier_treshold ]


write.csv(discharge_data.xts, "/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/datasets/cleaned_datasets/cleaned_water_discharge_dataset.csv")


#-------------------------------------------------------------------------------
#-------------------------- STREAM WATER LEVEL ---------------------------------
#-------------------------------------------------------------------------------

water_data <- read_excel("/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/weather_data/Limburg_water_data/waterlevel.xlsx")
water_data <- water_data %>% dplyr::slice(-c(1:4))
water_data <- water_data[1:2]
# Measured in m+NAP
colnames(water_data) <- c("Date", "Stream_water_level")
psych::describe(as.numeric(water_data$Stream_water_level))

# MEAN 94.69 SD: 0.08
# OUTLIER  7,35 (mean + 2 SD)
outlier_treshold.stream <- 94.85

water_data.xts <- xts::as.xts(water_data, order.by = lubridate::as_datetime(x = water_data$Date,tz = "Europe/Amsterdam"))
water_data.xts <- water_data.xts['2020-08-25 00:00:00/2021-09-24 07:28:35']
outliers.stream <- water_data.xts[as.numeric(water_data.xts$Stream_water_level) >= outlier_treshold.stream]

write.csv(water_data.xts, "/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/datasets/cleaned_datasets/cleaned_stream_water_level_dataset.csv")







