library(shiny)

shinyUI(fluidPage(
    titlePanel("Predict sales from price"),
    sidebarLayout(
        sidebarPanel(
            selectInput("selectSKU", "Select SKU", 
                        choices = c(1545, 1881, 1894, 1426, 1239), 
                        selected = 1545),
            sliderInput("newPrice", "Price multiplier", 0, 2, 
                        value = 1, step = 0.1),
            sliderInput("chooseShop", "Choose shops", 0, 13000, 
                        value = c(0, 13000), step = 5),
            h4("How to use the app"),
            print("Choose SKU. Move price change slider to multiply current
                  price with increasing or decreasing multiplier. Move shop
                  choice slider to limit the number of shops in the model.")
        ),
        mainPanel(
            plotOutput("plot1"),
            h3("Predicted volume:"),
            textOutput("pred1"),
            h3("Predicted sales:"),
            textOutput("pred2")
        )
    )
))
