library(rjson)
library(dplyr)
library(lubridate)
library(data.table)

# 
# Add 450 to the 400er values
# K3 (ABC) -> add Pattern from above
# Get 400er levels for K4 (no ABC) and Remove them
# Use the pattern from ABC dataset for the 400 values

json_data <- rjson::fromJSON(file = "/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/metadata.json")
data_ABC_ON <- read.csv('/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/kerkrade3tillJune1.csv')
data_no_ABC<- read.csv('/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/kerkrade4tillJune1.csv')

colnames(data_no_ABC)[grep("field",colnames(data_no_ABC))] = sapply(json_data$channel[grep("field",names(json_data$channel))],function(x){x[[1]]})
data_no_ABC$sensor =4
data_no_ABC$autocal = 0
data_no_ABC$created_at = lubridate::round_date(x = lubridate::as_datetime(x = data_no_ABC$created_at,tz = "Europe/Amsterdam"),unit = 'seconds')
data_no_ABC <- data_no_ABC[, !names(data_no_ABC) %in% c("latitude","longitude","elevation","status","dewpoint","entry_id","altitude")]

data_no_ABC <-data_no_ABC %>%
  dplyr::group_by(created_at=lubridate::floor_date(created_at, '1 hour')) %>%
  dplyr::summarize(CO2=mean(CO2), PM25=mean(PM25), PM10=mean(PM10), Temperature=mean(Temperature),Humidity=mean(Humidity),Pressure=mean(Pressure),sensor=mean(sensor), autocal=mean(autocal))


colnames(data_ABC_ON)[grep("field",colnames(data_ABC_ON))] = sapply(json_data$channel[grep("field",names(json_data$channel))],function(x){x[[1]]})
data_ABC_ON$sensor = 3
data_ABC_ON$autocal = 1
data_ABC_ON$created_at = lubridate::round_date(x = lubridate::as_datetime(x = data_ABC_ON$created_at,tz = "Europe/Amsterdam"),unit = 'seconds')
data_ABC_ON <- data_ABC_ON[, !names(data_ABC_ON) %in% c("latitude","longitude","elevation","status","dewpoint","entry_id","altitude")]

data_ABC_ON <-data_ABC_ON %>%
  dplyr::group_by(created_at=lubridate::floor_date(created_at, '1 hour')) %>%
  dplyr::summarize(CO2=mean(CO2), PM25=mean(PM25), PM10=mean(PM10), Temperature=mean(Temperature),Humidity=mean(Humidity),Pressure=mean(Pressure),sensor=mean(sensor), autocal=mean(autocal))

data_ABC_ON <-Filter(function(x)!all(is.na(x)), data_ABC_ON)
data_no_ABC<-Filter(function(x)!all(is.na(x)), data_no_ABC)

# Add 400 CO2 dummy to indicate flat levels
data_no_ABC <- mutate(data_no_ABC, CO2_400 = case_when(
  CO2 == 400  ~ 1,
  TRUE ~ 0
))
data_ABC_ON <- mutate(data_ABC_ON, CO2_400 = case_when(
  CO2 == 400  ~ 1,
  TRUE ~ 0
))


# Create time series objects. Its easier to work with.
xts <- xts::as.xts(data_ABC_ON, order.by = lubridate::as_datetime(x = data_ABC_ON$created_at,tz = "Europe/Amsterdam"))
xts.no.ABC <- xts::as.xts(data_no_ABC, order.by = lubridate::as_datetime(x = data_no_ABC$created_at,tz = "Europe/Amsterdam"))

# Calculate deltas for Bi and Bi+1
xts <-  na.omit(xts)
ABC.Data.with.deltas <- data.frame(diff(as.matrix(as.numeric(xts$CO2))))
ABC.Data.with.deltas <- data.frame(diff(as.matrix(as.numeric(xts$CO2))))
ABC.Data.with.deltas<-  na.omit(ABC.Data.with.deltas)

# Add number to the first element since there is no delta
x <- rep(0, ncol(ABC.Data.with.deltas))
ABC.Data.with.deltas <- rbind(x, ABC.Data.with.deltas)
xts$delta <- ABC.Data.with.deltas$diff.as.matrix.as.numeric.xts.CO2...
# RENAME DATASET BEFORE MERGE
names.ABC <- c("Date", "CO2_ON", "PM25_ON", "PM10_ON", "Temperature_ON", "Humidity_ON", "Pressure_ON", "Sensor", "autocal", "CO2_400_ON","Delta_ON")
colnames(xts) <- names.ABC

# We certainly loose the date column, so we add them back based on index of xts object
xts <- data.table::as.data.table(xts)
xts$Date <- xts$index
xts$index <- NULL

# Calculate deltas for Bj and Bj+1
xts.no.ABC <-  na.omit(xts.no.ABC)
NOABC.Data.with.deltas <- data.frame(diff(as.matrix(as.numeric(xts.no.ABC$CO2))))
NOABC.Data.with.deltas <-  na.omit(NOABC.Data.with.deltas)
y <- rep(0, ncol(xts.no.ABC))
NOABC.Data.with.deltas <- rbind(y, NOABC.Data.with.deltas)


xts.no.ABC$delta <- NOABC.Data.with.deltas$diff.as.matrix.as.numeric.xts.no.ABC.CO2...
names.NOABC <- c("Date","CO2_OFF", "PM25_OFF", "PM10_OFF", "Temperature_OFF", "Humidity_OFF", "Pressure_OFF", "Sensor", "autocal", "CO2_400_OFF","Delta_OFF")
colnames(xts.no.ABC) <- names.NOABC
xts.no.ABC <- data.table::as.data.table(xts.no.ABC)
xts.no.ABC$Date <- xts.no.ABC$index
xts.no.ABC$index<- NULL





# Create total dataset
total.dataset <- merge(x = xts.no.ABC, y = xts, by = "Date", all = TRUE)
View(total.dataset)
relevant.columns <- c("Date", "CO2_OFF","CO2_ON", "CO2_400_ON","CO2_400_OFF", "Delta_ON", "Delta_OFF")
total.dataset.relevant.columns <- subset(total.dataset, select=relevant.columns)
# How to handle the NA values for ABC sensor
total.dataset.relevant.columns <-  na.omit(total.dataset.relevant.columns)



# Add delta Bj (no ABC-Issue) to 400 CO2 periods of ABC data
for (i in 1:length(total.dataset.relevant.columns$Date)){
  if(total.dataset.relevant.columns[i,]$CO2_400_ON == 0) {
    total.dataset.relevant.columns[i,]$CO2_ON <- total.dataset.relevant.columns[i,]$CO2_ON + total.dataset.relevant.columns[i,]$Delta_OFF
  }
}
# Show that the alogrithm works
# -> Indicate ABC event in September
relevant = which(as.Date(substr(total.dataset.relevant.columns$Date,1,10),"%Y-%m-%d") %in% seq(as.Date("15-09-2020","%d-%m-%Y"),as.Date("18-09-2020","%d-%m-%Y"),by = 'days'))
plot(y = total.dataset.relevant.columns$CO2_OFF[relevant],
     x = total.dataset.relevant.columns$Date[relevant],
     pch= NA,ylab = 'CO2 Levels',xlab = "date")
lines(y = total.dataset.relevant.columns$CO2_ON,
      x = total.dataset.relevant.columns$Date, col = 'blue')
lines(y = total.dataset.relevant.columns$CO2_OFF,
      x = total.dataset.relevant.columns$Date,col = 'black')


#------------------------------------------------------------------------------
#------------------------COMBINE DATASETS -------------------------------------
#------------------------------------------------------------------------------
# OFF = K4
k4.columns <- c("Date","CO2_OFF", "PM25_OFF", "PM10_OFF", "Temperature_OFF", "Humidity_OFF", "Pressure_OFF","Sensor.x", "CO2_400_OFF")
dataset.k4 <- subset(total.dataset, select=k4.columns)
View(dataset.k4)
colnames(dataset.k4) <- c('Date', 'CO2', 'PM25', 'PM10', 'Temperature', 'Humidity', 'Pressure', 'Sensor', 'CO2_400')
# REMOVE CO2 400er values from noABC dataset since they are false
dataset.k4 <- dataset.k4[dataset.k4$CO2_400 == 0,]

k3.columns <- c("Date", "CO2_ON", "PM25_ON", "PM10_ON", "Temperature_ON", "Humidity_ON", "Pressure_ON","Sensor.y", "CO2_400_ON")
dataset.k3 <- subset(total.dataset, select=k3.columns)
colnames(dataset.k3) <- c('Date', 'CO2', 'PM25', 'PM10', 'Temperature', 'Humidity', 'Pressure', 'Sensor', 'CO2_400')


# Combine the datasets
total.dataset.stacked = bind_rows(dataset.k3,dataset.k4)
total.dataset.stacked <-  na.omit(total.dataset.stacked)
total.dataset.stacked = total.dataset.stacked[order(total.dataset.stacked$Date),]

# Get duplicated rows (based on date) of ABC dataset (K3)
K3.records.of.duplicated.rows <- total.dataset.stacked[duplicated(total.dataset.stacked$Date,fromLast=TRUE),]
K3.records.of.duplicated.rows = K3.records.of.duplicated.rows[order(K3.records.of.duplicated.rows$Date),]

# Remove the duplicated rows of K3 from the dataset 
data_frame_mod <- dplyr::anti_join(total.dataset.stacked,K3.records.of.duplicated.rows)

# Add 450 to every record to account for baseline issue
for(i in 1:length(data_frame_mod$CO2)){
  data_frame_mod[i,]$CO2 <-data_frame_mod[i,]$CO2+450 
}

#------------------------------------------------------------------------------
#------------------------EXPORT DATASETS --------------------------------------
#------------------------------------------------------------------------------

View(data_frame_mod)
write.csv(data_frame_mod, "/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/datasets/cleaned_datasets/total_Dataset_with_adjusted_ABC.csv")

# ------------------------------------------------------------------------------
# THE FOLLOWING CODE IS NOT RELEVANT FOR ABC ISSUE
# ------------------------------------------------------------------------------

# CLEAN NEW DATASET
# Read in new K4 (basement) data; We don't have any issues here with the data 
dataset_K4.new <- read.csv(file = '/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/2021-05-15_2021-09-29_K4_basement.csv')
tail(dataset_K4.new)
dataset_K4.new<- data.frame(dataset_K4.new)
dataset_K4.new$created_at <- as.POSIXct(dataset_K4.new$created_at, format = "%Y-%m-%d %H:%M:%S")
colnames(dataset_K4.new) <- c('Date','entry_id', 'Co2','PM25', 'PM10', 'Temperature', 'Humidity', 'Pressure','Altitude', 'Dewpoint')
dataset_K4.new <- dataset_K4.new[, !names(dataset_K4.new) %in% c('entry_id', 'latitude', 'longitude', 'elevation','status','Altitude')]
dataset_K4.new <-Filter(function(x)!all(is.na(x)), dataset_K4.new)
# Group the data by hour
hourly_data.dataset_K4.new <-dataset_K4.new %>%
  dplyr::group_by(Date=floor_date(Date, '1 hour')) %>%
  dplyr::summarize(Co2=mean(Co2), PM25=mean(PM25), PM10=mean(PM10), Temperature=mean(Temperature),Humidity=mean(Humidity),Pressure=mean(Pressure),Dewpoint=mean(Dewpoint))

write.csv(hourly_data.dataset_K4.new, "/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/datasets/cleaned_datasets/cleaned_dataset_new.csv")

