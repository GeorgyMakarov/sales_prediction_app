
# Prerequisite libraries --------------------------------------------------

# Libraries and directory setup

library(dplyr)
library(lubridate)
library(ggplot2)
library(pracma)
library(tseries)
library(forecast)

setwd("/home/georgy/Документы/Rstudio/baltika")

# Read in the data

mydata <- read.csv("mydata.csv", stringsAsFactors = FALSE)
mydata <- mydata %>% select(-X)

# Data tidying ----------------------------------------------------------------

# Filter 1 sku

sku_set <- 1239
by_weeks <- mydata %>% 
    filter(sku == sku_set) %>% group_by(wk) %>% summarise(sales = sum(sales),
                                                          volume = sum(volume),
                                                          price = mean(price))
rm(sku_set)

# Find min and max value of y-axis for further plot usage

y_low_lim <- round(min(by_weeks$volume), -2)
y_up_lim <- round(max(by_weeks$volume), -3)


# Compute maximal variance coefficient for chosen SKU
# If variance is greater than 30, then test for outliers and impute outliers
# Else go to modeling section

hm_test <- max(apply(by_weeks[, -1], 2, function(x) sd(x) / mean(x)))

if (hm_test > 0.30) {
    
    ts_volume <- ts(by_weeks$volume)
    volume_hampel <- hampel(ts_volume, 3, 2)
    volume_hampel <- as.vector(volume_hampel$y)
    by_weeks$volume <- volume_hampel
    by_weeks$sales <- by_weeks$volume * by_weeks$price
    rm(ts_volume, volume_hampel)
    
} else {by_weeks <- by_weeks}

# Plot results

ggplot(by_weeks) + 
    geom_point(aes(wk, volume, color = price, size = sales), alpha = 3/4) +
    scale_color_gradient(low = "green", high = "red") + xlab("") +
    geom_smooth(aes(wk, volume)) + scale_y_continuous(limits = c(0, y_up_lim))


# Modeling ----------------------------------------------------------------

# Split dataset into training and testing parts
# Split dataset into two time series: volume, price
# Test time series for stationarity
# Make prediction of price with auto.arima
# Decompose price to components and make arima model
# Make predictions on price for given data
# Make predictions of volume on ARIMAX model
# Find RMSE of the model with and without price-regressor
# Make forecast for next 52 weeks


# plot original time series
bywkts <- ts(by_weeks$volume, start = c(2015, 8), frequency = 12)
plot(bywkts, type = "l", col = "blue")

# make second differences to eliminate the trend
bywk_d2 <- diff(bywkts, differences = 2)
plot(bywk_d2, type = "l", col = "green")

# make first differences with lag = 52 to eliminate seasonality
bywk_d2d12 <- diff(bywk_d2, lag = 12)
plot(bywk_d2d12, ylab=expression(paste(nabla,"(",nabla^2,"Vol"[2],")")))



# Split 80% of the dataset to training

in_train <- round(nrow(by_weeks) * 0.8, 0)
training <- by_weeks %>% filter(wk < in_train)
testing <- by_weeks %>% filter(wk >= in_train)

# Make two univariate time-series for monthly data
# Monthly data to allow ARIMA model use seasonality

ts_volume <- ts(training$volume, start = c(2019, 1), frequency = 12)
ts_price <- ts(by_weeks$price, start = c(2016, 8), frequency = 12)

plot(ts_volume)
plot(decompose(ts_volume))



adf.test(ts_volume, alternative = "stationary", k = 12) # non-stationary series
findbest <- auto.arima(ts_volume, D = 1)
findbest
plot(forecast(findbest, h = 52))
arima_pred <- forecast(findbest, h = 52)
arima_pred <- arima_pred$mean
t <- 1:52
df <- data.frame(t, by_weeks$volume, arima_pred)

ggplot(df) + 
    geom_line(aes(x = t, y = by_weeks.volume)) +
    geom_line(aes(x = t, y = arima_pred), color = "blue")
?auto.arima

testing

# Copy what has been done in example

t <- 1:36
mu <- 10 + 0.05 * t
set.seed(1)
ses <- rnorm(12, mean = 0, sd = 1.5)
set.seed(1)
er <- rnorm(36, mean = 0, sd = 0.2)
ts_volume <- data.frame(mu, ses, er, vol = mu + ses + er)
ts_volume <- ts(ts_volume$vol, start = c(2015, 1), frequency = 12)
rm(mu, ses, er, t)

ts <- ts_volume
plot(ts)
plot(decompose(ts))
adf.test(ts, alternative = "stationary", k = 12) # non-stationary series
findbest <- auto.arima(ts)
findbest
plot(forecast(findbest, h = 12))









