library(shiny)
library(ggplot2)
library(dplyr)
library(datateachr) # for cancer_sample dataset
library(tidyverse)
library(shinyWidgets)
bcl <- read.csv("https://raw.githubusercontent.com/daattali/shiny-server/master/bcl/data/bcl-data.csv")
options(shiny.autoreload = TRUE)
ui <- fluidPage(
  img(src='liquor.jpg', align = "right", height="30%", width="30%"),
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      radioButtons("typeInput", "Product type",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      # Feature 1: adding selectInput feature to the shiny app to query multiple countries' results
      selectInput("countryInput", "Select a country", choices = bcl$Country, selected = "CANADA", multiple = TRUE),
    ),

    mainPanel(
      ##Feature 2: added an image of Liquor.
      plotOutput("coolplot"),
      br(), br(),
      tableOutput("results")
    )
  ),
  # Feature 3: add a background color
  setBackgroundColor(
    color = "ghostwhite",
    gradient = c("linear", "radial"),
    direction = c("bottom", "top", "right", "left"),
    shinydashboard = FALSE
  )

)

server <- function(input, output) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })

  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }

    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      )
  })

  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram()
  })

  output$results <- renderTable({
    filtered()
  })
}

shinyApp(ui = ui, server = server)
