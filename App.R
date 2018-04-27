library(shiny)
library(ggplot2)
library(tidyverse)
library(DT)
ui <- fluidPage(
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      radioButtons("typeInput", "Product type",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      uiOutput("countryOutput")
    ),
    mainPanel(
      plotOutput("coolplot"),
      br(), br(),
      dataTableOutput("results")
    )
  )
)


server <- function(input, output) {
  filtered = reactive({
    data %>% filter(
      CURRENT_DISPLAY_PRICE >= input$priceInput[1],
      CURRENT_DISPLAY_PRICE <= input$priceInput[2],
      PRODUCT_CLASS_NAME == input$typeInput,
      PRODUCT_COUNTRY_ORIGIN_NAME == input$countryInput
    )
  })
  output$coolplot = renderPlot({
    ggplot(filtered(), aes(PRODUCT_ALCOHOL_PERCENT))+
      geom_histogram()
  })
  output$results = renderDataTable({
    filtered()  
  })
  output$countryOutput = renderUI({
    selectInput("countryInput", "Country",
                sort(unique(data$PRODUCT_COUNTRY_ORIGIN_NAME)),
                selected = "CANADA")
  })
}
options(shiny.sanitize.errors = TRUE)
shinyApp(ui = ui, server = server)