# prerequisite libraries

library(dplyr)
library(lubridate)
library(splines)
library(ggplot2)
library(forecast)


# Data preprocessing ------------------------------------------------------

# reading data

setwd("/home/georgy/Документы/Rstudio")
mydata <- read.csv(file = "data.csv")

# transforming data

str(mydata) 
head(mydata) 
dim(mydata)

mydata$Date_Id <- ymd(mydata$Date_Id) # transform to date format
mydata$sales <- mydata$valuesales * 1000 # transform to currency
mydata <- mydata %>% filter(volumesales > 0)
mydata$price <- round(mydata$sales / mydata$volumesales, 4) # find price per litre
mydata$volume <- mydata$volumesales # name litres correctly for simplicity
mydata$SalesItem <- as.numeric(as.character(mydata$SalesItem))
mydata <- mydata %>% select(Date_Id, Shop_Id, SKU_Id, sales, price, volume)

# check and remove missing values and zero sales

na_val <- dim(mydata)[1] - sum(complete.cases(mydata))
mydata <- mydata %>% filter(!is.na(price))
mydata <- mydata %>% filter(sales > 0)
mydata <- mydata %>% filter(price > 0)

# identify sales outliers

ggplot(mydata) + geom_density(aes(sales))
ggplot(mydata) + 
    geom_histogram(aes(sales), bins = 20, fill = "steelblue") +
    coord_cartesian(ylim = c(0, 100))

# use univariate approach to remove sales outliers

qnt <- quantile(mydata$sales, probs = c(0.25, 0.75), na.rm = TRUE)
h <- 1.5 * (qnt[2] - qnt[1])
lower_limit <- qnt[1] - h
upper_limit <- qnt[2] + h
c(lower_limit, upper_limit)

mydata$sales[mydata$sales < lower_limit] <- NA
mydata$sales[mydata$sales > upper_limit] <- NA
round(sum(is.na(mydata$sales)) / nrow(mydata), 3)
mydata <- mydata %>% filter(!is.na(sales))

ggplot(mydata) + geom_histogram(aes(sales), bins = 15, fill = "steelblue")

# remove price outliers

qnt <- quantile(mydata$price, probs = c(0.25, 0.75), na.rm = TRUE)
h <- 1.5 * (qnt[2] - qnt[1])
lower_limit <- qnt[1] - h
upper_limit <- qnt[2] + h
mydata$price[mydata$price < lower_limit] <- NA
mydata$price[mydata$price > upper_limit] <- NA
mydata <- mydata %>% filter(!is.na(price))
ggplot(mydata) + geom_histogram(aes(price), bins = 15, fill = "steelblue")

# remove volume outliers

qnt <- quantile(mydata$volume, probs = c(0.25, 0.75), na.rm = TRUE)
h <- 1.5 * (qnt[2] - qnt[1])
lower_limit <- qnt[1] - h
upper_limit <- qnt[2] + h
mydata$volume[mydata$volume < lower_limit] <- NA
mydata$volume[mydata$volume > upper_limit] <- NA
mydata <- mydata %>% filter(!is.na(volume))
ggplot(mydata) + geom_histogram(aes(volume), bins = 15, fill = "steelblue")

# summarize dataset

summary(mydata)

# skewness tests



# filter data for top shops and skus

by_shop <- mydata %>% filter(Shop_Id %in% c(28, 2565, 3500, 6281, 7395, 7475,
                                            9290, 9862, 9882, 10036, 12017, 
                                            7338, 7382, 11064, 7389))
by_sku <- by_shop %>% filter(SKU_Id %in% c(1545, 1881, 1894, 1426, 1239))

# write the dataset for application to reduce the memory requirements

write.csv(by_sku, "mydata.csv")

# choose one sku for training

training <- by_sku %>% filter(SKU_Id == 1239)

training %>% group_by(Shop_Id) %>% summarise(ssales = sum(sales)) %>% 
    arrange(desc(ssales))

# plot on time series

ggplot(training) + 
    geom_point(mapping = aes(Date_Id, volume, color = price), alpha = 3/4) +
    geom_smooth(mapping = aes(Date_Id, volume)) + theme_grey()

# filter data for one shop

top_shop <- training #%>% filter(Shop_Id == 7338)

# plot on time series

ggplot(top_shop) +
    geom_point(aes(Date_Id, volume, color = price, size = sales), alpha = 3/4) +
    geom_smooth(aes(Date_Id, volume)) + theme_grey()

# group by weeks

top_shop$week <- week(top_shop$Date_Id)
by_weeks <- top_shop %>% 
    group_by(week) %>% summarise(Date_Id = mean(Date_Id),
                                 #Shop_Id = mean(Shop_Id),
                                 #SKU_Id = mean(SKU_Id),
                                 sales = sum(sales),
                                 volume = sum(volume),
                                 price = mean(price))

# Modeling ----------------------------------------------------------------

# convert sales data to time series

ts_sales <- ts(by_weeks$volume, 
               frequency = 365.25 / 7,
               start = decimal_date(ymd("2019-01-01")))
plot(ts_sales)

# make model with external regressor

ext_regressor <- by_weeks$price # use price as external regressor
model2 <- auto.arima(ts_sales, xreg = ext_regressor)
num_rows <- nrow(by_weeks)

# make forecast with corrected price

ext_forecast <- by_weeks$price * 1.0
pred.model2 <- forecast(model2, h = num_rows, xreg = ext_forecast)
pred.model2 <- as.numeric(pred.model2$mean)
pred.model2

fc_data <- data.frame(week = 1:num_rows, 
                      price = ext_forecast, 
                      volume = pred.model2,
                      sales = pred.model2 * ext_forecast)
fc_data$Date_Id <- seq(max(top_shop$Date_Id)+2, by = 'week', length.out = num_rows)
fc_data <- fc_data %>% select(Date_Id, sales, price, volume)

# replace negative forecast with mean

impute.mean <- function(x) replace(x, x < 0, mean(x, na.rm = TRUE))

fc_data <- fc_data %>% mutate(volume = impute.mean(volume))
ac_data <- by_weeks %>% select(Date_Id, sales, price, volume)

# combine two datasets for plotting

plot_dataset <- rbind(ac_data, fc_data)

# plot final results

png("baltika.png", width = 900, height = 640)
ggplot(plot_dataset) +
    geom_point(aes(Date_Id, volume, color = price, size = sales), alpha = 3/4) +
    geom_smooth(aes(Date_Id, volume)) + 
    geom_vline(xintercept = as.numeric(plot_dataset$Date_Id[52]),
                                                   color = "red", size = 0.8) +
    xlab("date") + ggtitle("Volume forecast for 2020") +
    scale_color_gradient(low = "green", high = "red")
dev.off()



