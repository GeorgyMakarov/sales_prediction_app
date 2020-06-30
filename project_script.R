# reading data

setwd("/home/georgy/Документы/Rstudio")
mydata <- read.csv(file = "data.csv")

# transforming data

str(mydata)
head(mydata)
dim(mydata)

library(dplyr)
library(lubridate)

mydata$Date_Id <- ymd(mydata$Date_Id)
mydata$SalesItem <- as.numeric(as.character(mydata$SalesItem))

mydata <- mydata %>% filter(!is.na(SalesItem))
length(complete.cases(mydata))

# exploratory data analysis

summary(mydata)

library(ggplot2)

## how the price and sales volumes changed over time

by_date <- mydata %>% group_by(Date_Id) %>% summarise(price = mean(valuesales),
                                                      sales = sum(volumesales))
ggplot(by_date) +
    geom_point(aes(Date_Id, sales, color = price), alpha = 3/4) +
    xlab("date") + ylab("volumesales") + theme_classic() + 
    geom_smooth(aes(Date_Id, sales)) + ggtitle("Sales by date and price")

## how the price and sales correlate to sales item

by_sku_item <- mydata %>% 
    filter(!valuesales == 0) %>% 
    group_by(SKU_Id, SalesItem) %>% 
    summarise(price = mean(valuesales), sales = sum(volumesales)) %>% 
    filter(SalesItem > 0)

ggplot(by_sku_item) +
    geom_point(aes(price, sales, color = SalesItem), alpha = 1/3) + 
    theme_classic()

# filter data for certain SKU and Shops

shop_id <- mydata %>% filter(Shop_Id == 2838)
sku_id <- shop_id %>% filter(SKU_Id > 0 & SKU_Id <= 1880 & !valuesales == 0)

# train linear model: how volume depends on prices

library(splines)

mod1 <- lm(volumesales ~ valuesales, data = sku_id)
new_price <- 5.2
pred1 <- predict(mod1, newdata = data.frame(valuesales = new_price))
summary(mod1)

mod2 <- lm(volumesales ~ ns(valuesales, 3), data = sku_id)
pred2 <- predict(mod2, newdata = data.frame(valuesales = new_price))
summary(mod2)

price_max <- max(sku_id$valuesales)
mod2lines <- predict(mod2, newdata = data.frame(valuesales = 0:price_max))

mycol <- rgb(0.3, 0.3, 1, alpha = 0.5)
plot(sku_id$valuesales, sku_id$volumesales, xlab = "price", ylab = "sales",
     bty = "n", pch = 16, col = mycol)
abline(mod1, col = "red", lwd = 2)
points(new_price, pred1, col = "red", pch = 16, cex = 1)
lines(0:price_max, mod2lines, col = "blue", lwd = 2)
points(new_price, pred2, col = "blue", pch = 16, cex = 1)





