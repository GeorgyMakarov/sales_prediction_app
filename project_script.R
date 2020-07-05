# prerequisite libraries

library(dplyr)
library(lubridate)
library(splines)
library(ggplot2)

# reading data

setwd("/home/georgy/Документы/Rstudio")
mydata <- read.csv(file = "data.csv")

# transforming data

str(mydata)
head(mydata)
dim(mydata)

mydata$Date_Id <- ymd(mydata$Date_Id)
mydata$SalesItem <- as.numeric(as.character(mydata$SalesItem))

mydata <- mydata %>% filter(!is.na(SalesItem))
mydata <- mydata %>% filter(!valuesales == 0)
mydata <- mydata %>% filter(valuesales > 0)
mydata <- mydata %>% filter(volumesales > 0)
mydata <- mydata %>% filter(SKU_Id %in% c(1887, 1239, 1527, 1890, 
                                          1648, 1892, 1880))
mydata <- mydata %>% filter(valuesales < 15)

# exploratory data analysis

summary(mydata)
ggplot(mydata) + geom_point(aes(valuesales, volumesales))
hist(mydata$valuesales)

# filter data for certain SKU

sku_id <- mydata %>% filter(SKU_Id == 1880)

# train linear model: how volume depends on prices

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
points(new_price, pred1, col = "red", pch = 16, cex = 1.5)
lines(0:price_max, mod2lines, col = "blue", lwd = 2)
points(new_price, pred2, col = "blue", pch = 16, cex = 1.5)

top3 <- sku_id %>% group_by(Shop_Id) %>% summarise(sales = sum(volumesales)) %>% 
    arrange(desc(sales)) %>% head(3)
top3 <- top3$Shop_Id

top3_filter <- sku_id %>% filter(Shop_Id %in% top3)
points(top3_filter$valuesales, top3_filter$volumesales, 
       col = "blue", pch = 16, cex = 1)

ggplot(sku_id) +
    geom_point(aes(Date_Id, volumesales), color = "skyblue", alpha = 1/3) +
    geom_smooth(aes(Date_Id, volumesales)) + theme_classic() + xlab("day") +
    ylab("sales")



