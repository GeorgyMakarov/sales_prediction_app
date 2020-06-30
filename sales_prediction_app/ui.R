library(shiny)

shinyUI(fluidPage(
    titlePanel("Predict sales from price"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("sliderSKU", "Choose SKU", 0, 2603, value = 1880),
            sliderInput("sliderShop", "Choose shop", 0, 40820, value = c(0, 40820)),
            numericInput("newPrice", "What is the new price?", 5.5, min = 0.1, max = 400, step = 0.2),
            checkboxInput("showModel1", "Show/Hide Model 1", value = TRUE),
            checkboxInput("showModel2", "Show/Hide Model 2", value = TRUE)
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
