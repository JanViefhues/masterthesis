library(psych)
library(xts)
library(lubridate)
library(dplyr)
library(data.table)
library(Hmisc)
library(corrplot)
library(tidyverse)

before.dataset <- read.csv(file = '/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/datasets/cleaned_datasets/total_Dataset_with_adjusted_ABC.csv')
during.dataset <- read.csv(file = '/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/datasets/cleaned_datasets/cleaned_dataset_new.csv')


# Check for PM Outliers in the dataset
Outliers.before.1 <- before.dataset[before.dataset$PM25  >100,]
Outliers.before.2 <- before.dataset[before.dataset$PM10 > 100,]
Outliers.during.zeros <- during.dataset[during.dataset$PM25 == 0,]
Outliers.during.1 <- during.dataset[during.dataset$PM25 > 100,]
Outliers.during.zeros <- during.dataset[during.dataset$PM25 == 0,]

# Remove PM Outliers from the dataset
before.dataset <-  before.dataset[before.dataset$PM25 < 100,]
before.dataset <-  before.dataset[before.dataset$PM10 < 100,]
before.dataset <-  before.dataset[before.dataset$PM25 > 0,]
before.dataset <-  before.dataset[before.dataset$PM10 > 0,]

# CO2 outlier

before.dataset <-  before.dataset[before.dataset$CO2 < 5000,]

# Remove unnecessary cols
before.dataset$Sensor <- NULL
before.dataset$CO2_400 <- NULL


during.dataset<- during.dataset[, !names(during.dataset) %in% c('Dewpoint')]
before.dataset$Date = lubridate::as_datetime(x = before.dataset$Date,tz = "Europe/Amsterdam")

colnames(during.dataset) <- c('x','Date', 'CO2', 'PM25', 'PM10', 'Temperature', 'Humidity', 'Pressure')
colnames(before.dataset) <- c('x','Date', 'CO2', 'PM25', 'PM10', 'Temperature', 'Humidity', 'Pressure')
during.dataset$x <- NULL
before.dataset$x <- NULL

before.dataset <-  na.omit(before.dataset)
during.dataset <-  na.omit(during.dataset)
# ----------------------------------------------------------
# ----- AVERAGE DUPLICATE ROWS FROM BEFORE DATASET ---------
# ----------------------------------------------------------
# Before dataset has two records for the same date/time for some entries. This
# is due to the join performed in the cleaning process
# Hence, I have to create averages for those records
before.dataset$Date <- lubridate::as_datetime(x = before.dataset$Date,tz = "Europe/Amsterdam")
before.dataset <-before.dataset %>%
  dplyr::group_by(Date=Date) %>%
  dplyr::summarise(CO2=mean(CO2), PM25=mean(PM25), PM10=mean(PM10), Temperature=mean(Temperature),Humidity=mean(Humidity),Pressure=mean(Pressure))


# --- STEPS FOR BOTH DATA SETS ----

# ----------------------------------------------------------
# ----------------------------------------------------------
# ------------CREATE ONE LARGE TOTAL DATASET ---------------
# ----------------------------------------------------------
# ----------------------------------------------------------
head(before.dataset)
head(during.dataset)

total.dataset <- rbind(before.dataset,during.dataset)




# Check for duplicated row (due to overlapping timeframe)
length(total.dataset$Date)
length(unique(total.dataset$Date))

# Combine duplicated rows
total.dataset <-total.dataset %>%
  dplyr::group_by(Date=Date) %>%
  dplyr::summarise(CO2=mean(CO2), PM25=mean(PM25), PM10=mean(PM10), Temperature=mean(Temperature),Humidity=mean(Humidity),Pressure=mean(Pressure))


length(total.dataset$Date)
length(unique(total.dataset$Date))

total.dataset$Date <- lubridate::as_datetime(x = total.dataset$Date,tz = "Europe/Amsterdam")



# ----------------------------------------------------------
# ------------ Add weather data ----------------------------
# ----------------------------------------------------------

percipitation.data <- read.csv(file = '/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/weather_data/precipitation_data_cleaned.csv')


# Trim the dataset to be in the same timeframe
as_xts.percipitation.data <- xts::as.xts(percipitation.data, order.by = lubridate::as_datetime(x = percipitation.data$Timestamp,tz = "Europe/Amsterdam"))
as_xts.percipitation.data<- as_xts.percipitation.data['2020-08-25 00:00:00/2021-09-24 07:28:35']
percipitation.data <- as.data.frame(data.table::as.data.table(as_xts.percipitation.data))
percipitation.data <- percipitation.data[c('Timestamp','precipitation')]
colnames(percipitation.data) <- c('Date', 'Precipitation')


percipitation.data$Precipitation = as.numeric(percipitation.data$Precipitation)
percipitation.data$Date = lubridate::as_datetime(x = percipitation.data$Date,tz = "Europe/Amsterdam")



# bind the precipitation data to the data set 
total.dataset <- merge(total.dataset, percipitation.data, by = "Date")

# Remove possible nas
total.dataset <- total.dataset[complete.cases(total.dataset),]



# ----------------------------------------------------------
# ------------ CREATE DUMMY FOR FLOODING -------------------
# ----------------------------------------------------------

# > 20 L / QM in 6 hours (20 mm in 6 h)
flooding.threshold <- 10
print(flooding.threshold)

total.dataset <- mutate(total.dataset, HeavyRainIndicator = case_when(
  Precipitation >= flooding.threshold  ~ 1,
  TRUE ~ 0
))



# Check which days have potential flooding 
days_with_heavy_percipitation <- total.dataset[which(total.dataset$HeavyRainIndicator == 1),]
print(days_with_heavy_percipitation)


flood_start <- '2021-06-29'
flood_end <- '2021-07-17'

# Validate time frame with news theory

# Create timeframe for flooding

key1 <- 'periode_from'
value1 <- flood_start
key2 <- 'periode_to'
value2 <- flood_end

flooding.periode <- list()
flooding.periode[[ key1 ]] <- value1
flooding.periode[[ key2 ]] <- value2

# Add binaries for flooding periode
total.dataset <- mutate(total.dataset, IsFloodingPeriode = case_when(
  (Date >= flooding.periode$periode_from & Date <= flooding.periode$periode_to) ~ 1,
  TRUE ~ 0
))


# ----------------------------------------------------------
# --------- CREATE CATEGORICALS FOR PERIODES ---------------
# ----------------------------------------------------------
total.dataset <- mutate(total.dataset, periode = case_when(
  (Date < flooding.periode$periode_from) ~ "before",
    (Date > flooding.periode$periode_to) ~ "after",
    TRUE ~ "during"
))



# ----------------------------------------------------------
# --------- CREATE DUMMIES FOR DANGEROUS WATER LEVEL -------
# ----------------------------------------------------------
water_level.data <- read.csv("/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/datasets/cleaned_datasets/cleaned_water_level_dataset.csv")
water_level.data[1] <- NULL
water_level.data$Date <- lubridate::as_datetime(x = water_level.data$Date,tz = "Europe/Amsterdam")



# bind the water_level data to the data set 
total.dataset.copy <- merge(total.dataset, water_level.data, by = "Date")




# I have to do some research on this
high_water_level.threshold <- 500

# Adding column based on other column:
total.dataset <- mutate(total.dataset.copy, HighWaterLevel = case_when(
  RhineWaterLevel >= high_water_level.threshold ~ 1,
  RhineWaterLevel < high_water_level.threshold ~ 0,
))



# ----------------------------------------------------------
# --------- ADD GROUND - AND MINE WATER ---------- ---------
# ----------------------------------------------------------

ground_and_mine_water.data <- read.csv("/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/datasets/cleaned_datasets/cleaned_minewater_and_groudnwater_data.csv")
ground_and_mine_water.data$Date <- lubridate::as_datetime(x = ground_and_mine_water.data$Date,tz = "Europe/Amsterdam")
ground_and_mine_water.data$X <- NULL

# Convert to hourly data
ground_and_mine_water.data.to.be.coverted <- ground_and_mine_water.data
as_xts.ground_and_mine_water.data.to.be.coverted <- xts::as.xts(ground_and_mine_water.data.to.be.coverted, order.by = lubridate::as_datetime(x = ground_and_mine_water.data.to.be.coverted$Date,tz = "Europe/Amsterdam"))


for (date in 1:length(ground_and_mine_water.data.to.be.coverted$Date)){
  number_of_hours <- 23
  default_date <- as.character(ground_and_mine_water.data.to.be.coverted$Date[date])
  default_mine_water <- ground_and_mine_water.data.to.be.coverted$mine_water_level[date]
  default_ground_water <- ground_and_mine_water.data.to.be.coverted$ground_water_level[date]
  for (hour in 1:number_of_hours){
    date_and_time_stamp <- paste(default_date, paste0(as.character(hour),":00:00"))
    date_and_time_stamp <- lubridate::as_datetime(x = date_and_time_stamp,tz = "Europe/Amsterdam")
    ground_and_mine_water.data.to.be.coverted <- rbind(ground_and_mine_water.data.to.be.coverted, data.frame(Date=date_and_time_stamp, mine_water_level=default_mine_water, ground_water_level=default_ground_water))
  }
}
ground_and_mine_water.data.to.be.coverted <- ground_and_mine_water.data.to.be.coverted[complete.cases(ground_and_mine_water.data.to.be.coverted ), ] # Keep only the complete rows
ground_and_mine_water.data.to.be.coverted <- xts(ground_and_mine_water.data.to.be.coverted, order.by=ground_and_mine_water.data.to.be.coverted$Date)
ground_and_mine_water.data.to.be.coverted <- ground_and_mine_water.data.to.be.coverted['2020-08-25 00:00:00/2021-09-24 07:00:00 ']

converted.ground_and_mine_water.data <-  data.frame(Date=index(ground_and_mine_water.data.to.be.coverted), coredata(ground_and_mine_water.data.to.be.coverted))
converted.ground_and_mine_water.data$Date.1 <- NULL


# Merge to total dataset
total.dataset <- merge(total.dataset, converted.ground_and_mine_water.data, by = "Date")
total.dataset <- total.dataset[complete.cases(total.dataset ), ] # Keep only the complete rows
head(total.dataset)

# ----------------------------------------------------------
# --------- ADD DISCHARGE WATER DATA -----------------------
# ----------------------------------------------------------

discharge.data <- read.csv("/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/datasets/cleaned_datasets/cleaned_water_discharge_dataset.csv")
discharge.data <- discharge.data[complete.cases(discharge.data),]
discharge.data$Date <- lubridate::as_datetime(x = discharge.data$Date,tz = "Europe/Amsterdam")
discharge.data$X <- NULL
total.dataset <- merge(total.dataset, discharge.data, by = "Date")

# ----------------------------------------------------------
# --------- ADD STREAM WATER DATA --------------------------
# ----------------------------------------------------------

stream.data <- read.csv("/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/datasets/cleaned_datasets/cleaned_stream_water_level_dataset.csv")
stream.data <- stream.data[complete.cases(stream.data),]
stream.data$Date <- lubridate::as_datetime(x = stream.data$Date,tz = "Europe/Amsterdam")
stream.data$X <- NULL
total.dataset <- merge(total.dataset, stream.data, by = "Date")




# ----------------------------------------------------------
# --------- CREATE DUMMIES FOR DANGEROUS CO2 LEVEL ---------
# ----------------------------------------------------------

leakage_CO2.threshold <- 1000

# Adding column based on other column:
total.dataset <- mutate(total.dataset, abnormal_Co2_leakage = case_when(
  CO2 >= leakage_CO2.threshold ~ 1,
  CO2 < leakage_CO2.threshold ~ 0,
))


write.csv(total.dataset, "/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/datasets/cleaned_datasets/total_dataset_with_precipitation_and_dummies_hourly.csv")


# ----------------------------------------------------------
# ----------------------------------------------------------
# -----------CREATE SPLITTET DATASET -----------------------
# ----------------------------------------------------------
# ----------------------------------------------------------

# TASK: Split the dataset into 3 parts, before, during and after flooding

total.dataset.copy <- total.dataset
total.dataset.copy.a_xts <- xts::as.xts(total.dataset.copy, order.by = lubridate::as_datetime(x = total.dataset.copy$Date,tz = "Europe/Amsterdam"))
dataset.before.flooding.as_xts <- total.dataset.copy.a_xts[paste(start(total.dataset.copy.a_xts$Date),as.Date(flood_start)-1, sep ="/", collapse = NULL)]
dataset.during.flooding.as_xts <- total.dataset.copy.a_xts[paste(flood_start, flood_end, sep ="/", collapse = NULL)]
dataset.after.flooding.as_xts <-  total.dataset.copy.a_xts[paste(as.Date(flood_end)+1, end(total.dataset$Date),sep ="/", collapse = NULL)]

# Convert the data back to be numeric
dataset.before.flooding <- as.data.frame(data.table::as.data.table(dataset.before.flooding.as_xts))
dataset.during.flooding <- as.data.frame(data.table::as.data.table(dataset.during.flooding.as_xts))
dataset.after.flooding <- as.data.frame(data.table::as.data.table(dataset.after.flooding.as_xts))


dataset.before.flooding <- dataset.before.flooding[c('Date', 'CO2','PM25', 'PM10', 'Temperature', 'Humidity', 'Pressure', 'Precipitation', 'HeavyRainIndicator', 'flood','IsFloodingPeriode', 'abnormal_Co2_leakage','RhineWaterLevel','ground_water_level','mine_water_level', 'Stream_water_level', 'Discharge')]
dataset.during.flooding <- dataset.during.flooding[c('Date', 'CO2','PM25', 'PM10', 'Temperature', 'Humidity', 'Pressure', 'Precipitation', 'HeavyRainIndicator', 'flood','IsFloodingPeriode', 'abnormal_Co2_leakage','RhineWaterLevel','ground_water_level','mine_water_level', 'Stream_water_level','Discharge')]
dataset.after.flooding <- dataset.after.flooding[c('Date', 'CO2','PM25', 'PM10', 'Temperature', 'Humidity', 'Pressure', 'Precipitation', 'HeavyRainIndicator', 'flood', 'IsFloodingPeriode', 'abnormal_Co2_leakage','RhineWaterLevel','ground_water_level','mine_water_level','Stream_water_level','Discharge')]


print(length(dataset.during.flooding$Date))

#Export Datasets
tail(total.dataset)


write.csv(dataset.before.flooding, "/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/datasets/cleaned_datasets/hourly_dataset.before.flooding_with_precipitation_and_dummies.csv")
write.csv(dataset.during.flooding, "/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/datasets/cleaned_datasets/hourly_dataset.during.flooding_with_precipitation_and_dummies.csv")
# PROTO
write.csv(dataset.after.flooding, "/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/datasets/cleaned_datasets/hourly_dataset.after.flooding_with_precipitation_and_dummies.csv")



