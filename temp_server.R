library(shiny)
library(dplyr)
library(lubridate)
library(splines)

shinyServer(function(input, output){
    mydata <- read.csv(file = "data.csv")
    mydata$Date_Id <- ymd(mydata$Date_Id)
    mydata$SalesItem <- as.numeric(as.character(mydata$SalesItem))
    mydata <- mydata %>% filter(!is.na(SalesItem))
    mydata <- mydata %>% filter(!valuesales == 0)
    
    shop_id <- reactive({
        shop_min <- input$sliderShop[1]
        shop_max <- input$sliderShop[2]
        mydata %>% filter(Shop_Id >= shop_min & Shop_Id <= shop_max)
    })
    
    sku_id <- reactive({
        sku_min <- input$sliderSKU
        shop_id %>% filter(SKU_Id = sku_min)
    })
    
    model1 <- reactive({
        lm(volumesales ~ valuesales, data = sku_id())
    })
    
    model2 <- reactive({
        lm(volumesales ~ ns(valuesales, 3), data = sku_id())
    })
    
    model1pred <- reactive({
        new_price <- input$newPrice
        predict(model1(), newdata = data.frame(valuesales = new_price))
    })
    
    output$pred1 <- renderText({model1pred()})
    
})
