library(shiny)
library(dplyr)
library(lubridate)
library(splines)

shinyServer(function(input, output){
    
    mydata <- read.csv("mydata.csv")
    mydata <- mydata %>% select(-X)
    mydata$Date_Id <- ymd(mydata$Date_Id)
    mydata <- mydata %>% filter(valuesales < 10)
    
    sku_id <- reactive({
        sku_selected <- input$selectSKU
        mydata %>% filter(SKU_Id == sku_selected)
    })
    
    model1 <- reactive({
        lm(volumesales ~ valuesales, data = sku_id())
    })
    
    model2 <- reactive({
        lm(volumesales ~ ns(valuesales, 3), data = sku_id())
    })
    
    new_price <- reactive({
        price_coeff <- input$newPrice
        current_meanp <- mean(sku_id()$valuesales)
        newp <- current_meanp * (1 + price_coeff)
        data.frame(valuesales = newp)
    })
    
    model1pred <- reactive({
        predict(model1(), new_price())
    })
    
    model2pred <- reactive({
        predict(model2(), new_price())
    })
    
    output$pred1 <- renderText({model1pred()})
    output$pred2 <- renderText({model2pred()})
    
    output$plot1 <- renderPlot({
        
        price_max <- max(sku_id()$valuesales)
        mod2lines <- predict(model2(), 
                             newdata = data.frame(valuesales = 0:price_max))
        
        top3 <- sku_id() %>% 
            group_by(Shop_Id) %>% 
            summarise(sales = sum(volumesales)) %>% 
            arrange(desc(sales)) %>% head(3)
        top3 <- top3$Shop_Id
        top3_filter <- sku_id() %>% filter(Shop_Id %in% top3)
        
        mycol <- rgb(0.3, 0.3, 1, alpha = 0.5)
        plot(sku_id()$valuesales, sku_id()$volumesales, xlab = "price", 
             ylab = "sales", bty = "n", pch = 16, col = mycol)
        if (input$showModel1) {abline(model1(), col = "red", lwd = 2)}
        if (input$showModel1) {points(new_price(), model1pred(), col = "red", 
                                      pch = 16, cex = 1.5)}
        if (input$showModel2) {lines(0:price_max, mod2lines, col = "blue", 
                                     lwd = 2)}
        if (input$showModel2) {points(new_price(), model2pred(), col = "blue", 
                                      pch = 16, cex = 1.5)}
        if (input$showTop) {points(top3_filter$valuesales, 
                                   top3_filter$volumesales, col = "green", 
                                      pch = 16, cex = 1.0)}
    })
})


