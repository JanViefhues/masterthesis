library(car)
install.packages("devtools")
library(devtools)
devtools::install_github("kassambara/easyGgplot2")
library(easyGgplot2)
devtools::install_github("matherion/userfriendlyscience", dependencies=TRUE)
library(userfriendlyscience)
library(MASS)


# Hourly base
before.dataset <- read.csv(file = '/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/datasets/cleaned_datasets/hourly_dataset.before.flooding_with_precipitation_and_dummies.csv')
during.dataset <- read.csv(file = '/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/datasets/cleaned_datasets/hourly_dataset.during.flooding_with_precipitation_and_dummies.csv')
after.dataset <- read.csv(file = '/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/datasets/cleaned_datasets/hourly_dataset.after.flooding_with_precipitation_and_dummies.csv')

before.dataset = before.dataset[complete.cases(before.dataset),]
during.dataset = during.dataset[complete.cases(during.dataset),]
after.dataset = after.dataset[complete.cases(after.dataset),]
total.dataset = read.csv(file="/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/datasets/cleaned_datasets/total_dataset_with_precipitation_and_dummies_hourly.csv")

# ----------------------------------------------------------
#Daily base

before.dataset <- read.csv(file = '/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/datasets/cleaned_datasets/dataset.before.flooding_with_precipitation_and_dummies_dailyBase.csv')
during.dataset <- read.csv(file = '/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/datasets/cleaned_datasets/dataset.during.flooding_with_precipitation_and_dummies_dailyBase.csv')
after.dataset <- read.csv(file = '/Users/jan-philippviefhues/Desktop/UNI/Maastricht/um/Thesis/data/datasets/cleaned_datasets/dataset.after.flooding_with_precipitation_and_dummies_dailyBase.csv')

View(after.dataset)
# ----------------------------------------------------------

# Check if the correct data is there
# THERE IS AN PROBLEM
before.dataset <- na.omit(before.dataset)

View(before.dataset)

# ----------------------------------------------------------
# --------- DESCRIPTIVE STATISTICS -------------------------
# ----------------------------------------------------------

# ----------------------------------------------------------
# -----------    Summary Statistics   ----------------------

summaryStatistics <- function(dataset){
  x <- psych::describe(dataset[, !names(dataset) %in% c("flood","Date","X","HighWaterLevel","RhineWaterLevel","dangerousCo2","IsFloodingPeriode","HeavyRainIndicator","periode")], na.rm = TRUE)
  x[, names(x) %in% c('vars', 'min', 'max', 'n', 'sd', 'median', 'mean', 'skew', 'kurtosis', 'range')]
  }


total.dataset.summary <- summaryStatistics(total.dataset)
before.dataset.summary <- summaryStatistics(before.dataset)
during.dataset.summary <- summaryStatistics(during.dataset)
after.dataset.summary <- summaryStatistics(after.dataset)
print(before.dataset.summary)
print(during.dataset.summary)
print(after.dataset.summary)

# ----------------------------------------------------------
# ---------------- CO2 -------------------------------------
# ----------------------------------------------------------
print(before.dataset.summary)
hist(before.dataset$CO2)
print(during.dataset.summary)
hist(during.dataset$CO2)
print(after.dataset.summary)
print(total.dataset.summary)
hist(after.dataset$CO2)


ggplot(total.dataset, aes(x=periode, y=CO2, fill=periode)) + geom_boxplot()


# Check statistic differenceof CO2 in the datasets 
welchs_anova_training_CO2 <- oneway.test(CO2 ~ periode, data = total.dataset, var.equal = FALSE)
welchs_anova_training_CO2
# Is significant so we can the recject the H0 meaning the means are not equal

# Games-Howell post-hoc test 
gamesHowell.CO2 <- oneway(total.dataset$periode, y = total.dataset$mine_water_level, posthoc = 'games-howell')
gamesHowell.CO2
View(total.dataset)
# Is significant. The differences make intuitive sense as judging from the boxplot 
# it can be seen that these groups distributed quite differently from each other.


# ------ Count the numbers of leakage events ----------------
before.dataset = before.dataset[complete.cases(before.dataset),]
dangerous_CO2_counter_before <- 0
for (val in before.dataset$dangerousCo2){
  if (val == 1){
    dangerous_CO2_counter_before <- dangerous_CO2_counter_before + 1
  }
}

during.dataset = during.dataset[complete.cases(during.dataset),]
dangerous_CO2_counter_during <- 0
for (val in during.dataset$dangerousCo2){
  if (val == 1){
    dangerous_CO2_counter_during <- dangerous_CO2_counter_during + 1
  }
}


after.dataset = after.dataset[complete.cases(after.dataset),]
dangerous_CO2_counter_after <- 0
for (val in after.dataset$dangerousCo2){
  if (val == 1){
    dangerous_CO2_counter_after <- dangerous_CO2_counter_after + 1
  }
}

total.dataset = total.dataset[complete.cases(total.dataset),]
dangerous_CO2_counter_total <- 0
for (val in total.dataset$dangerousCo2){
  if (val == 1){
    dangerous_CO2_counter_total <- dangerous_CO2_counter_total + 1
  }
}

print(dangerous_CO2_counter_before)
print(dangerous_CO2_counter_during)
print(dangerous_CO2_counter_after)
print(dangerous_CO2_counter_total)

# Test for random chance in leakage

# -------- Create rations from total / leakage --------------

len_before.dataset <- length(before.dataset$Date)
len_during.dataset <- length(during.dataset$Date)
len_after.dataset <- length(after.dataset$Date)
len_total.dataset <- length(total.dataset$Date)

print(len_before.dataset/dangerous_CO2_counter_before)
print(len_after.dataset)


leakage.ration.before <- dangerous_CO2_counter_before/ len_before.dataset *100
leakage.ration.during <- dangerous_CO2_counter_during/ len_during.dataset *100
leakage.ration.after <- dangerous_CO2_counter_during/ len_after.dataset *100
leakage.ration.total <- dangerous_CO2_counter_total/ len_total.dataset *100

leakage_per_day.before <- dangerous_CO2_counter_before/(len_before.dataset/24)
leakage_per_day.during <- dangerous_CO2_counter_during/(len_during.dataset/24)
leakage_per_day.after <- dangerous_CO2_counter_after/(len_after.dataset/24)

print(leakage_per_day.before)
print(leakage_per_day.during)
print(leakage_per_day.after)

print(leakage.ration.before)
print(leakage.ration.during)
print(leakage.ration.after)
print(leakage.ration.total)


# ----------------------------------------------------------
# ---------------- Humidity --------------------------------
# ----------------------------------------------------------

hist(before.dataset$Humidity)
hist(during.dataset$Humidity)
hist(after.dataset$Humidity)

ggplot(total.dataset, aes(x=periode, y=Humidity, fill=periode)) + geom_boxplot()

welchs_anova_training_Humidity <- oneway.test(Humidity ~ periode, data = total.dataset, var.equal = FALSE)
welchs_anova_training_Humidity

# !!! Games-Howell post-hoc test 
gamesHowell.Humidity <- oneway(total.dataset$periode, y = total.dataset$Humidity, posthoc = 'games-howell')
gamesHowell.Humidity
# The differences make intuitive sense as judging from the boxplot it can be seen that these groups distributed quite differently from each other.

# To indicate the skew in humidity between the groups, we reduce the skale
# and compare the data on a one week base
total.datset.xts <- xts::as.xts(total.dataset, order.by = lubridate::as_datetime(x = total.dataset$Date,tz = "Europe/Amsterdam"))
total.dataset.1.Week.before <- total.datset.xts["2021-06-11/2021-06-18"]
total.dataset.1.Week.during <- total.datset.xts["2021-06-19/2021-06-25"]
total.dataset.1.Week.after <- total.datset.xts["2021-07-16/2021-07-23"]
total.dataset.oneWeekEach <- rbind(total.dataset.1.Week.before, total.dataset.1.Week.during, total.dataset.1.Week.after)

total.dataset.oneWeekEach<- as.data.frame(total.dataset.oneWeekEach)
total.dataset.oneWeekEach$Humidity <- as.numeric(total.dataset.oneWeekEach$Humidity)
total.dataset.oneWeekEach$CO2 <- as.numeric(total.dataset.oneWeekEach$CO2)

# Multiple histograms on the same plot

# TOTAL DATASET
ggplot2.histogram(data=total.dataset, xName='Humidity',
                  groupName='periode', legendPosition="top")


ggplot2.histogram(data=total.dataset, xName='Humidity',
                  groupName='periode', legendPosition="top",
                  alpha=0.5 )
ggplot2.histogram(data=total.dataset, xName='Humidity',
                  groupName='periode', legendPosition="top",
                  alpha=0.5, 
                  meanLineColor="white", meanLineSize=1.5)


# TOTAL DATASET ONE WEEK
ggplot2.histogram(data=total.dataset.oneWeekEach, xName='Humidity',
                  groupName='periode', legendPosition="top")


ggplot2.histogram(data=total.dataset.oneWeekEach, xName='Humidity',
                  groupName='periode', legendPosition="top",
                  alpha=0.5 )
ggplot2.histogram(data=total.dataset.oneWeekEach, xName='Humidity',
                  groupName='periode', legendPosition="top",
                  alpha=0.5, 
                   meanLineColor="white", meanLineSize=1.5)



# Plot
# TODO: Add colors
total.dataset.oneWeekEach$Date <- lubridate::as_datetime(x = total.dataset.oneWeekEach$Date,tz = "Europe/Amsterdam")
ggplot(total.dataset.oneWeekEach, aes(x=Date, y=Humidity)) +
  geom_line()


# Result
# From the describes we can see, that the mean of CO2 is much higher in the
# periode during and after the flooding. Also, the humidity increase. This 
# makes perfect sense, since the heavy raining increase the overall outside humidity. 
# This effect gives confidence that the data set is accurate.

# -----------------------------------------------------------
# ---- DESCRIBE PM 10 & 25 ----------------------------------
# -----------------------------------------------------------

gamesHowell.pm10 <- oneway(total.dataset$periode, y = total.dataset$PM10, posthoc = 'games-howell')
gamesHowell.pm10
gamesHowell.pm25 <- oneway(total.dataset$periode, y = total.dataset$PM25, posthoc = 'games-howell')
gamesHowell.pm25

ggplot(total.dataset, aes(x=periode, y=PM25, fill=periode)) + geom_boxplot()
ggplot(total.dataset, aes(x=periode, y=PM10, fill=periode)) + geom_boxplot()


# -----------------------------------------------------------
# ---- DESCRIBE PRESSURE - ----------------------------------
# -----------------------------------------------------------
gamesHowell.pressure <- oneway(total.dataset$periode, y = total.dataset$Pressure, posthoc = 'games-howell')
gamesHowell.pressure
ggplot(total.dataset, aes(x=periode, y=Pressure, fill=periode)) + geom_boxplot()

hist(before.dataset$Pressure)
hist(during.dataset$Pressure)
hist(after.dataset$Pressure)


# TODO: Pressure vs CO2 Plot

# -----------------------------------------------------------
# ---- DESCRIBE Percipitation -------------------------------
# -----------------------------------------------------------

gamesHowell.precipitation <- oneway(total.dataset$periode, y = total.dataset$Precipitation, posthoc = 'games-howell')
gamesHowell.precipitation
ggplot(total.dataset, aes(x=periode, y=Precipitation, fill=periode)) + geom_boxplot()

# Draw percipitation vs CO2 in line chart tablau


# -----------------------------------------------------------
# ---- DESCRIBE Rhine Water Level ---------------------------
# -----------------------------------------------------------

# Use this command to reset the ggplot 2 due to early messup
dev.off()
ggplot(total.dataset, aes(x=periode, y=RhineWaterLevel, fill=periode)) + geom_boxplot()
gamesHowell.rhineWaterLevel <- oneway(total.dataset$periode, y = total.dataset$RhineWaterLevel, posthoc = 'games-howell')
gamesHowell.rhineWaterLevel



# ----------------------------------------------------------
# -----------    Correlation matrix   ----------------------

before.dataset.without_date <- before.dataset[, !names(before.dataset) %in% c("Date", "X")]
during.dataset.without_date <- during.dataset[, !names(during.dataset) %in% c("Date", "X")]
after.dataset.without_date <- after.dataset[, !names(after.dataset) %in% c("Date", "X")]
total.dataset.without_date <- total.dataset[, !names(total.dataset) %in% c("Date", "X")]

head(before.dataset.without_date)
# Check for normality assumption for Pearson matrix
# With RHINE WATER
fit <- lm(CO2 ~ Humidity + PM25 + PM10 + Temperature + Pressure + Precipitation +RhineWaterLevel ,before.dataset.without_date) #assign regression results to fit
# With MINE and GROUND WATER
fit <- lm(CO2 ~ Humidity + PM25 + PM10 + Temperature + Pressure + Precipitation +mine_water_level+ground_water_level,before.dataset.without_date) #assign regression results to fit
plot(fit, 1)
# There is no linear relationship between the independent variable and the other features


# Use Spearman rank-order correlation coefficient as the data is not normal
# With RHINE WATER
relevant.features <- c("CO2","PM25","PM10","Temperature", "Humidity", "Pressure","Precipitation","RhineWaterLevel")

# With MINE and GROUND WATER
relevant.features <- c("CO2","PM25","PM10","Temperature", "Humidity", "Pressure","Precipitation","ground_water_level","mine_water_level")

# Total Dataset
total.dataset.correlation_matrix <- cor(total.dataset.without_date[relevant.features], y = NULL,
    method = c("spearman"))
corrplot::corrplot(total.dataset.correlation_matrix, method = 'number') 

# Before Dataset
before.dataset.correlation_matrix <- cor(before.dataset.without_date[relevant.features], y = NULL,
                                        method = c("spearman"))
corrplot::corrplot(before.dataset.correlation_matrix, method = 'number')

# During Dataset
during.dataset.correlation_matrix <- cor(during.dataset.without_date[relevant.features], y = NULL,
                                         method = c("spearman"))
corrplot::corrplot(during.dataset.correlation_matrix, method = 'number') 

# After Dataset
after.dataset.correlation_matrix <- cor(after.dataset.without_date[relevant.features], y = NULL,
                                         method = c("spearman"))
corrplot::corrplot(after.dataset.correlation_matrix, method = 'number')                   

after.dataset.correlation_matrix <- cor(after.dataset.without_date[relevant.features], y = NULL,
                                        method = c("spearman"))
corrplot::corrplot(after.dataset.correlation_matrix, method = 'number')     

res2 <-cor.test(after.dataset$CO2, after.dataset$Temperature,  method = "spearman")
res2
# colorful number

# ----------------------------------------------------------
# -----------    Histograms for CO2   ----------------------

before.dataset.hist <- hist(before.dataset$CO2)
during.dataset.hist <- hist(during.dataset$CO2)
after.dataset.hist <- hist(after.dataset$CO2)
total.dataset.hist <- hist(total.dataset$CO2)

hist(before.dataset$dangerousCo2)

# Temperature, Pressure, Humididty are normal distributed

head(total.dataset)
                        

# ----------------------------------------------------------
# -----------   Check for differences in the dataset   -----

# As we can see from the histograms, the data for  CO2 is not normally distributed.
# And as CO2 is the independent variable in our case, we assign greatest importance
# to it likewise to the selection on an appr. test and models.
# This behaivior is quiet expected, as most of the CO2 is in the lower boundary 
# of < 400, and some outliers skew the dustribution to the right. 
# We don't want to lose this information, as we are especially interested in exactly
# these outliers.

# The data is right skewed and has kurtosis, which is caused by the leakage effects of CO2.
# So the cause is just normal and exactly what we want to see here.
# For our analysis that means, that we will work with none parametric tests


# Non-Paremetric testing
# MCNEMAR-TEST: Test, if there is a differece in different timeframes - before and 
# central limit theorem (https://www.investopedia.com/terms/c/central_limit_theorem.asp)

# ANOVA


# T-Test
# Compare just flooding and not flooding



# 1. Check if variances are euqal
s.before <- psych::describe(before.dataset$CO2)
s.during <- psych::describe(during.dataset$CO2)
var.before <- s.before$sd ** 2
var.during <- s.during$sd ** 2
print(sd.before)
print(var.during)

# 2. Perform T-Test \ Welch Test for before and during dataset with not equal variances
t.test.beforeAndDuring <- t.test(before.dataset$CO2,during.dataset$CO2, var.equal = FALSE, alternative = "two.sided")
print(t.test.beforeAndDuring)

# -------------------------------------------
# ----- ANOVA --------------------------
# -------------------------------------------

# ---- ANOVA ASSUMPTIONS ---

# Testing that the population is normally distributed

# We already know that the population is not normal dist.
# However, this is seen as not a big problem as the normality ass. of
# anova is not really robust. Furthermore, as the dataset is huge, normality can
# be assumed, as the ... says

# Testing for euqal variances (Levene’s test)

leveneTest(total.dataset$CO2, total.dataset$periode)
# We have to reject H0, meaning that the gorup dont have equal means. 
# This totally makes sense as from the summary statisics we can
# already guess, that the Co2 for the flooding periode is higher 

anova_training <- aov(total.dataset$CO2~total.dataset$periode)
summary(anova_training)
# P value is significant meaning we can reject H0 which means 
# that the group means are not the same => strong evidence
# against the H0
# Concluding, we should use a Welch's-Test ANOVA

welchs_anova_training <- oneway.test(CO2 ~ periode, data = total.dataset, var.equal = FALSE)
welchs_anova_training
# The overall p value is smaller than the significant niveau of 5%, meaning 
# that we can reject the H0 Hypothesis that the groups have equal Co2 values
boxplot(CO2 ~ periode,
        data = total.dataset,
        main = "CO2 for diffent periods",
        xlab = "periode",
        ylab = "CO2 value",
        col = "steelblue",
        border = "black")


# Check the normality assumption of residuals
plot(anova_training, 2)




