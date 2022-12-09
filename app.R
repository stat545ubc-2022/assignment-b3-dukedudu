library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(shinyWidgets)
library(shinythemes)
bcl <- read.csv("https://raw.githubusercontent.com/daattali/shiny-server/master/bcl/data/bcl-data.csv")
options(shiny.autoreload = TRUE)
ui <- fluidPage(
  # Feature 1 (new launched): added a pre-defined theme for the shiny app.
  theme = shinytheme("sandstone"),
  # Feature 2 (modified): added a liquor images and using HTML format to change the image size.
  HTML('<center><img src="liquor.jpg" width="1000"></center>'),
  # Feature 3 (new launched): used heading format and center the header.
  titlePanel(h1("BC Liquor Store prices", align = "center")),
  sidebarLayout(
    # Designed a new shiny app with liquor data. Users can select alcohol from different countries and sweetness.
    sidebarPanel(
      sliderInput("sweetnessInput", "Sweetness", 0, 10, c(1, 2), pre = "Sweet"),
      radioButtons("typeInput", "Product type",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      # Feature 4 (previous implemented): added selectInput feature to the shiny app to query multiple countries' results
      selectInput("countryInput", "Select a country", choices = bcl$Country, selected = "CANADA", multiple = TRUE),
      # Feature 5 (new launched): added a download button that user can download the current query result.
      downloadButton("download")
    ),
    mainPanel(
      # Feature 6 (new launched): added a tab view to separate plot and query table results.
      tabsetPanel(
        tabPanel("Scatter Plot", plotOutput("scatterplot")),
        tabPanel("Histo Plot", plotOutput("histoplot")),
        tabPanel("Table", tableOutput("results"))
      ),
    )
  ),

  # Feature 7 (previous implemented): added a background color
  setBackgroundColor(
    color = "ghostwhite",
    gradient = c("linear", "radial"),
    direction = c("bottom", "top", "right", "left"),
    shinydashboard = FALSE
  ),
  # Feature 8 (new launched): added a hyperlink for the raw dataset.
  a(href="https://raw.githubusercontent.com/daattali/shiny-server/master/bcl/data/bcl-data.csv",
    "Link to the original data set")

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
      filter(Sweetness >= input$sweetnessInput[1],
             Sweetness <= input$sweetnessInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      )
  })

  # Feature 5 (new launched): user can download the current query results.
  output$download <-
    downloadHandler(
      filename = function(){
        "tmp_download.csv"
      },
      content = function(file){
        write.csv(filtered(), file)
      }
    )

  output$scatterplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content, Price, colour = Country)) +
      geom_point(alpha = 1, size=3)
  })

  output$histoplot <- renderPlot({
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
