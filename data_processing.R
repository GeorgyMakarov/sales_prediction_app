# Preparing for sales prediction app

# Data wrangling ----------------------------------------------------------

## Prerequisite libraries and setup

# Show libraries to final users;
# Do not show setwd part;
# Briefly describe the dataset;

library(dplyr)
library(lubridate)
library(splines)
library(ggplot2)
library(forecast)
library(e1071)

setwd("/home/georgy/Документы/Rstudio/baltika")

## Data download and transformation

# Read the data from .csv file;
# Briefly explore the type of data;

mydata <- read.csv(file = "data.csv", stringsAsFactors = FALSE)

str(mydata) 
head(mydata) 
dim(mydata)
summary(mydata)

# Convert data to date format;
# Multiply sales by thousand to get to currency;
# Filter *NA* values from all columns;
# Filter rows with sales = 0;
# Filter rows with volume = 0;
# Compute price as a fraction of sales / volume;
# Reduce map - need: week, shop, sku, sales, price, volume;


mydata$week <- ymd(mydata$Date_Id)
mydata$sales <- mydata$valuesales * 1000
dim(mydata)[1] - sum(complete.cases(mydata))
mydata <- mydata %>% filter(volumesales > 0)
mydata <- mydata %>% filter(sales > 0)
mydata$price <- round(mydata$sales / mydata$volumesales, 4)
mydata <- mydata %>% select(week, shop = Shop_Id, sku = SKU_Id, 
                            sales, price, volume = volumesales)

# Remove outliers from the dataset ----------------------------------------

# Check skewness of the data;
# Skewness shows positive skew for sales, price, volume;
# Price is the most heavily skewed to the right;

t1 <- tribble(~feature, ~coef_skewness, ~ mean,
                "sales", skewness(mydata$sales), mean(mydata$sales),
                "price", skewness(mydata$price), mean(mydata$price),
                "volume", skewness(mydata$volume), mean(mydata$volume))

t2 <- tribble(~median, ~max, ~min,
              median(mydata$sales), max(mydata$sales), min(mydata$sales),
              median(mydata$price), max(mydata$price), min(mydata$price),
              median(mydata$volume), max(mydata$volume), min(mydata$volume))

cbind(t1, t2)
rm(t1, t2)

tribble(~feature, ~coeff.variation,
        "sales", sd(mydata$sales) / mean(mydata$sales) * 100,
        "price", sd(mydata$price) / mean(mydata$price) * 100,
        "volume", sd(mydata$volume) / mean(mydata$volume) * 100)

# Visualize outliers with histograms.
# Sales histogram shows that there are sales with values up to 4e+05. There are 
# so few observations with sales higher than 1e+04 that they do not show up on
# the histogram. The size of the sales axis marks that there are outliers;
# Histogram of volume confirms the presence of outliers;
# Strangely, but price histogram also shows wide spread of prices with 
# concentration around zero. It indicates that outliers in sales and volume
# do not match by observations;


par(mfrow = c(2, 2))
hist(mydata$sales, col = "steelblue", main = "sales", xlab = "sales")
hist(mydata$volume, col = "skyblue", main = "volume", xlab = "volume")
hist(mydata$price, col = "lightgreen", main = "price", xlab = "price")
par(mfrow = c(1,1))

# Despite multiple options to deal with outliers, the most sensible choice is
# to remove them. This choice makes sense since the number of outliers is small
# and it will not disturb the modeling. Removing outliers will benefit to
# increasing speed of future application as well;

# Cook's distance;
# Make a formula for sales ~ price + volume;
# Calculate Cook's distances;
# Setup cut-off distance value as 4 / n as given in the source:
# http://gradientdescending.com/introduction-to-outlier-detection/
# Mark outliers in the dataset;

form <- as.formula(log(sales) ~ price + volume)
mod <- lm(form, data = mydata)

cooksd <- cooks.distance(mod)
cut_off <- 4 / nrow(mydata) # setup cutoff value as 4 / n

cooks_data <- data.frame(cooksd, cut_off, test = cooksd >= cut_off)
mydata <- mydata %>% mutate(cooksd = cooks_data$test)
rm(mod, cooksd, cut_off, form, cooks_data)
summary(mydata$cooksd)

# Applying Cook's distance method to the dataset reduced the number of outliers
# by 42k rows. Volume range decreased by 5 times. The same for the price range.
# The same for the sales range;
# There are many outliers left in the dataset in all three features. Cook's
# distance method returned poor result, because there are *mistakes* in the
# dataset, that make the model fit to them;

par(mfrow = c(2, 2))
hist(mydata$sales[mydata$cooksd == F], main = "", col = "steelblue", xlab = "sales")
hist(mydata$volume[mydata$cooksd == F], main = "", col = "skyblue", xlab = "volume")
hist(mydata$price[mydata$cooksd == F], main = "", col = "lightgreen", xlab = "price")
par(mfrow = c(1,1))

# Interquantile range
# Univariate method of removing outliers with 1.5IQ;
# Calculate quantiles and interquantile interval for sales, price, volume
# Remove values that are out of the range of 1.5IQ;

out_sales <- boxplot.stats(mydata$sales)$out
out_price <- boxplot.stats(mydata$price)$out
out_volume <- boxplot.stats(mydata$volume)$out

mydata$iqr <- 0
mydata$iqr[mydata$sales %in% out_sales] <- 1
mydata$iqr[mydata$price %in% out_price] <- 1
mydata$iqr[mydata$volume %in% out_volume] <- 1

mydata$iqr[mydata$iqr == 0] <- "FALSE"
mydata$iqr[mydata$iqr == 1] <- "TRUE"
mydata$iqr <- as.logical(mydata$iqr)
rm(out_sales, out_price, out_volume)
summary(mydata$iqr)

# Cleaning out the dataset with one-by-one outliers extraction results in
# better identification of outliers than Cook's distance approach. However
# there are still to many outliers in the data. The algorithm worked well
# on sales, but the volume and the price required further exploration;

par(mfrow = c(2, 2))
hist(mydata$sales[mydata$iqr == F], main = "", col = "steelblue", xlab = "sales")
hist(mydata$volume[mydata$iqr == F], main = "", col = "skyblue", xlab = "volume")
hist(mydata$price[mydata$iqr == F], main = "", col = "lightgreen", xlab = "price")
par(mfrow = c(1,1))

# Mahalanobis distance;
# Calculate Mahalanobis distances;
# Setup cutoff value for confidence level .95 according to the source Model 2:
# https://www.r-bloggers.com/a-new-way-to-handle-multivariate-outliers/
# Test outliers using MD and cutoff;

mah_data <- mydata[, c("sales", "price", "volume")]
mah_data$sales <- log(mah_data$sales)

alpha <- 0.001
cutoff <- (qchisq(p = 1 - alpha, df = ncol(mah_data)))

mahal_r <- mahalanobis(mah_data, colMeans(mah_data), cov(mah_data))
mah_data <- mah_data %>% mutate(mhlbs = mahal_r, 
                                crit = cutoff, 
                                test = mhlbs >= crit)

mydata$mhlbs <- mah_data$test
rm(mah_data, mahal_r)
summary(mydata$mhlbs)

# MD method detected less outliers than Cook's distance and 2.5 less outliers
# than interquantile range method. The reason as in case with CD is the impact
# from large *misprint* values in sales and volume;

par(mfrow = c(2, 2))
hist(mydata$sales[mydata$mhlbs == F], main = "", col = "steelblue", xlab = "sales")
hist(mydata$volume[mydata$mhlbs == F], main = "", col = "skyblue", xlab = "volume")
hist(mydata$price[mydata$mhlbs == F], main = "", col = "lightgreen", xlab = "price")
par(mfrow = c(1,1))

# Temp data processing

mydata <- read.csv(file = "sales_b_database.csv", stringsAsFactors = FALSE)
mydata <- mydata %>% select(-X)
mydata$week <- ymd(mydata$week)
summary(mydata[, 4:6])

par(mfrow = c(2, 2))
hist(mydata$sales, main = "", col = "steelblue", xlab = "sales")
hist(mydata$volume, main = "", col = "skyblue", xlab = "volume")
hist(mydata$price, main = "", col = "lightgreen", xlab = "price")
par(mfrow = c(1,1))

# Start with outliers in prices - visual inspection
hist(mydata$price, main = "", col = "lightgreen", xlab = "price")
mydata <- mydata %>% filter(price < 200)

# Research first cluster of prices

par(mfrow = c(2, 2))
hist(mydata$price[mydata$price > 0 & mydata$price < 10], 
     main = "price in [0,10]", col = "lightgreen", xlab = "price")
hist(mydata$price[mydata$price > 0 & mydata$price < 2.5], 
     main = "price in [0, 2.5]", col = "lightgreen", xlab = "price")
hist(mydata$price[mydata$price > 0 & mydata$price < 1.5], 
     main = "price in [0, 1.5]", col = "lightgreen", xlab = "price")
hist(mydata$price[mydata$price > 0.4 & mydata$price < 1.5], 
     main = "price in [0.4, 1.5]", col = "lightgreen", xlab = "price")
par(mfrow = c(1,1))

# Research second cluster of prices

par(mfrow = c(2, 2))
hist(mydata$price[mydata$price > 30 & mydata$price < 200], 
     main = "price in [30, 200]", col = "lightgreen", xlab = "price")
hist(mydata$price[mydata$price > 40 & mydata$price < 150], 
     main = "price in [40, 150]", col = "lightgreen", xlab = "price")
hist(mydata$price[mydata$price > 40 & mydata$price < 130], 
     main = "price in [40, 130]", col = "lightgreen", xlab = "price")
hist(mydata$price[mydata$price > 40 & mydata$price < 120], 
     main = "price in [40, 120]", col = "lightgreen", xlab = "price")
par(mfrow = c(1,1))

# Filter items with price of 0.5 and see why this could happen

low_1894 <- mydata %>% 
        filter(sku == 1894) %>%
        filter(price >= 0.5 & price <= 0.6) %>% 
        head(3)

high_1894 <- mydata %>%
        filter(sku == 1894) %>% 
        filter(price >= 50 & price <= 60) %>% 
        head(3)

rbind(low_1894, high_1894)

rm(low_1894, high_1894)

# Cut off values with prices < 40 & > 120

mydata <- mydata %>% filter(price > 40 & price < 120)

par(mfrow = c(2, 2))
hist(mydata$sales, main = "", col = "steelblue", xlab = "sales")
hist(mydata$volume, main = "", col = "skyblue", xlab = "volume")
hist(mydata$price, main = "", col = "lightgreen", xlab = "price")
par(mfrow = c(1,1))

# Run interquartile range on the dataset

out_sales <- boxplot.stats(mydata$sales)$out
out_price <- boxplot.stats(mydata$price)$out
out_volume <- boxplot.stats(mydata$volume)$out

mydata$iqr <- 0
mydata$iqr[mydata$sales %in% out_sales] <- 1
mydata$iqr[mydata$price %in% out_price] <- 1
mydata$iqr[mydata$volume %in% out_volume] <- 1

mydata$iqr[mydata$iqr == 0] <- "FALSE"
mydata$iqr[mydata$iqr == 1] <- "TRUE"
mydata$iqr <- as.logical(mydata$iqr)
rm(out_sales, out_price, out_volume)
summary(mydata[, 4:7])
mydata <- mydata %>% filter(iqr == FALSE) %>% select(-iqr)

# Run Cook's distance on the dataset

form <- as.formula(log(sales) ~ price + volume)
mod <- lm(form, data = mydata)
cooksd <- cooks.distance(mod)
cut_off <- 4 / nrow(mydata)
cooks_data <- data.frame(cooksd, cut_off, test = cooksd >= cut_off)
mydata <- mydata %>% mutate(cooksd = cooks_data$test)
rm(mod, cooksd, cut_off, form, cooks_data)
summary(mydata$cooksd)
mydata <- mydata %>% filter(cooksd == FALSE) %>% select(-cooksd)

# Plot results

par(mfrow = c(2, 2))
hist(mydata$sales, main = "", col = "steelblue", xlab = "sales")
hist(mydata$volume, main = "", col = "skyblue", xlab = "volume")
hist(mydata$price, main = "", col = "lightgreen", xlab = "price")
par(mfrow = c(1,1))

# Check if the data is homogeneous

tribble(~feature, ~coeff.variation,
        "sales", sd(mydata$sales) / mean(mydata$sales) * 100,
        "price", sd(mydata$price) / mean(mydata$price) * 100,
        "volume", sd(mydata$volume) / mean(mydata$volume) * 100)

# Plot the sales

hist(mydata$sales, main = "", col = "steelblue", xlab = "sales")


# Choose top items and top shops ------------------------------------------

# The model needs significant items only. Here I apply Pareto method to find
# top items that together contribute to 80% of volume sales;

by_skuid <- mydata %>% 
        group_by((sku)) %>% 
        summarise(volume = sum(volume)) %>% arrange(desc(volume))

by_skuid <- by_skuid %>% 
        mutate(weight = volume / sum(volume), acc_weight = cumsum(weight)) %>% 
        filter(acc_weight < 0.81)

head(by_skuid)

# Make a list of top items;
# Filter the dataset to leave top items only;

sku_list <- unique(by_skuid$`(sku)`)
mydata <- mydata %>% filter(sku %in% sku_list)
rm(by_skuid, sku_list)

# Repeat Pareto exercise for shops;

top_shops <- mydata %>% 
        group_by(shop) %>% 
        summarise(volume = sum(volume)) %>% arrange(desc(volume)) %>% 
        mutate(weight = volume / sum(volume), acc_weight = cumsum(weight)) %>% 
        filter(acc_weight < 0.81)

head(top_shops)

shops_list <- unique(top_shops$shop)
mydata <- mydata %>% filter(shop %in% shops_list)
rm(top_shops, shops_list)

# Plot mydata;
# There are 4 levels of sales - consider them as factor variable, as if
# they were representative of different retail shops. Test this hypothesis;

par(mfrow = c(2, 2))
hist(mydata$sales, main = "", col = "steelblue", xlab = "sales")
hist(mydata$volume, main = "", col = "skyblue", xlab = "volume")
hist(mydata$price, main = "", col = "lightgreen", xlab = "price")
par(mfrow = c(1,1))

hist(mydata$sales[mydata$sales > 6000 & mydata$sales <= 10000], 
     main = "", col = "steelblue", xlab = "sales")
unique(mydata$sales)

# The hypothesis to make factor out of sales does not work straight -
# we need sales to sum up and find how the sales changed over time;
# Another hypothesis: cluster sales and test if the same shops will be in
# the same clusters. Then use clusters as a factor variables?
# Not needed - I won't use sales as a factor. The only external regressor I
# plan to use is price.

# Find how the volume changed on weekly basis;

by_weeks <- mydata %>% group_by(week) %>% summarise(sales = sum(sales),
                                                     volume = sum(volume),
                                                     price = mean(price))

# There are 62 weeks in the dataset - while the starting and ending dates
# are within one year 2019. This means that some of the dates double the weeks.
# Add week as a repetitive date and then use it to group the dataset in 52 
# groups.

length(unique(by_weeks$week))
rm(by_weeks)

mydata$wk <- lubridate::week(mydata$week)
by_weeks <- mydata %>% group_by(wk) %>% summarise(sales = sum(sales), 
                                                  volume = sum(volume), 
                                                  price = mean(price))

# Plotting volume by weeks shows decreasing volume sales with local extreme
# on week 37. There are weeks when volume is very high compared to other weeks;

ggplot(by_weeks) + 
        geom_point(aes(wk, volume, color = price, size = sales), alpha = 3/4) +
        scale_color_gradient(low = "green", high = "red") + xlab("") +
        geom_smooth(aes(wk, volume))

# Filter weeks with volume over 1.5M;
# Check if increase was due to specific shops;

top_weeks <- by_weeks %>% filter(volume > 1500000)

chk_tweeks <- mydata %>% filter(wk %in% unique(top_weeks$wk))

# All shops in both data frames are equal;
# All sju in both data frames are equal;

length(unique(chk_tweeks$shop)) == length(unique(mydata$shop))
length(unique(chk_tweeks$sku)) == length(unique(mydata$sku))

# Impute values on top weeks with mean of nearest 4 values;
# Write imputing function to substitue sales and volume;

unique(chk_tweeks$wk)
rm(chk_tweeks)

impute.outs <- function(wks, n, data, column) {
        low_wk <- wks - n # setup lower limit;
        hi_wk <- wks + n  # setup higher limit;
        range <- low_wk:hi_wk # make range;
        mean_dat <- data %>% filter(wk %in% range & !wk == wks)
        mean_dat <- mean_dat[, column]
        mean_dat <- mean(as.vector(unlist(mean_dat)))
        mean_dat
}

impute.outs(1, 2, by_weeks, column = "volume") # test function;

# Run imputing function across sequence of top weeks;

list_weeks <- unique(top_weeks$wk)

for (i in list_weeks) {
        by_weeks$volume[i] <- impute.outs(i, 1, by_weeks, "volume")
        by_weeks$sales[i] <- impute.outs(i, 1, by_weeks, "sales")
}

# The volume tend to decrease over time and with price increase;

ggplot(by_weeks) + 
        geom_point(aes(wk, volume, color = price, size = sales), alpha = 3/4) +
        scale_color_gradient(low = "green", high = "red") + xlab("") +
        geom_smooth(aes(wk, volume))

rm(top_weeks, by_weeks, i, list_weeks)

# We use time series with weekly data;
# There has to be a possibility to choose different SKU by week;
# Group data by SKU and week;
# Write final result for SKU and week in .csv file for future Shiny app;

mydata <- mydata %>% group_by(wk, sku) %>% summarise(sales = sum(sales), 
                                                     volume = sum(volume), 
                                                     price = mean(price))
head(mydata)
write.csv(mydata, "mydata.csv")
rm(mydata)
warnings()


