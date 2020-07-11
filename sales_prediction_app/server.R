library(shiny)
library(dplyr)
library(lubridate)
library(splines)
library(ggplot2)
library(forecast)

shinyServer(function(input, output){
    
    mydata <- read.csv("mydata.csv")
    mydata <- mydata %>% select(-X)
    mydata$Date_Id <- ymd(mydata$Date_Id)
    
    training <- reactive({
        select_sku <- input$selectSKU
        mydata %>% filter(SKU_Id == select_sku)
    })
    
    training_week <- reactive({
        training() %>% mutate(week = week(Date_Id))
    })
    
    by_weeks <- reactive({
        training_week() %>% 
            group_by(week) %>% 
            summarise(Date_Id = mean(Date_Id), 
                      sales = sum(sales), 
                      volume = sum(volume), 
                      price = mean(price))
        })
    
    ts_sales <- reactive({
        ts(by_weeks()$volume, 
           frequency = 365.25 / 7,
           start = decimal_date(ymd("2019-01-01")))
    })
    
    ext_regressor <- reactive({by_weeks()$price})
    
    model <- reactive({auto.arima(ts_sales(), xreg = ext_regressor())})
    
    pred_model <- reactive({
        price_coeff <- input$newPrice
        num_rows <- nrow(by_weeks())
        ext_forecast <- by_weeks()$price * price_coeff
        pred.model2 <- forecast(model(), h = num_rows, xreg = ext_forecast)
        pred.model2 <- as.numeric(pred.model2$mean)
        fc_data <- data.frame(week = 1:num_rows, 
                              price = ext_forecast, 
                              volume = pred.model2,
                              sales = ext_forecast * pred.model2)
        fc_data$Date_Id <- seq(max(by_weeks()$Date_Id)+2, 
                               by = 'week', length.out = num_rows)
        fc_data <- fc_data %>% select(Date_Id, sales, price, volume)
        
        impute.mean <- function(x) replace(x, x < 0, mean(x, na.rm = TRUE))
        fc_data <- fc_data %>% mutate(volume = impute.mean(volume),
                                      sales = price * volume)
        ac_data <- by_weeks() %>% select(Date_Id, sales, price, volume)
        rbind(ac_data, fc_data)
    })
    
    pred_volume <- reactive({
        sum(pred_model()$volume)
    })
    
    pred_sales <- reactive({
        sales_p <- sum(pred_model()$sales)
        
        if (sales_p < 0) {sfc = 0} else {sfc = sales_p}
        sfc
        
    })
    
    half_rows <- reactive({
        nrow(pred_model()) * 0.5
    })
    
    output$plot1 <- renderPlot({
        ggplot(pred_model()) +
            geom_point(aes(Date_Id, volume, color = price, size = sales), alpha = 3/4) +
            geom_smooth(aes(Date_Id, volume)) + 
            geom_vline(xintercept = as.numeric(pred_model()$Date_Id[half_rows()]),
                       color = "red", size = 0.8) +
            xlab("date")
    })
    
    output$pred1 <- renderText({pred_volume()})
    output$pred2 <- renderText({pred_sales()})

})


