library(shiny)

shinyUI(fluidPage(
    titlePanel("Predict sales from price"),
    sidebarLayout(
        sidebarPanel(
            selectInput("selectSKU", "Select SKU", 
                        choices = c(1239, 1426, 1881, 1892, 1894, 1899), 
                        selected = 1426),
            sliderInput("newPrice", "Price multiplier", 0, 2, value = 1, step = 0.1),
            h4("How to use the app"),
            print("Choose SKU and future expected price change")
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
