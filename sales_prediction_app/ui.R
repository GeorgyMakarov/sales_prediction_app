library(shiny)

shinyUI(fluidPage(
    titlePanel("Predict sales from price"),
    sidebarLayout(
        sidebarPanel(
            selectInput("selectSKU", "Select SKU", 
                        choices = c(1887, 1239, 1527, 1890, 1648, 1892, 1880), 
                        selected = 1880),
            sliderInput("newPrice", "Price multiplier", -1, 5, value = 0, step = 0.1),
            checkboxInput("showModel1", "Show/Hide Model 1", value = FALSE),
            checkboxInput("showModel2", "Show/Hide Model 2", value = FALSE),
            checkboxInput("showTop", "Show/Hide Top-3 shops", value = FALSE),
            h4("How to use the app"),
            print("Choose SKU and future expected price change. Tick the models you 
                  want to look at. You can show/hide top-3 shops sales for each
                  SKU. Both models are linear. The difference is in tuning of
                  an algorithm.")
        ),
        mainPanel(
            plotOutput("plot1"),
            h3("Predicted sales from Model 1:"),
            textOutput("pred1"),
            h3("Predicted sales from Model 2:"),
            textOutput("pred2")
        )
    )
))
