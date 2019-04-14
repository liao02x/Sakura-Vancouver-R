library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- dashboardPage(
  #includeCSS("www/bcl.css"),
  dashboardHeader(title = "BC Liquor Store Prices",
  								tags$li(a(href='http://www.bcliquorstores.com',
  													tags$img(src='https://bit.ly/2ClIIHW',
  																	 height='30',width='60')),
  												class = "dropdown")),
  skin = "blue",
  dashboardSidebar(
  	sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
  	sliderInput("binsNumber", "Bins number", 5, 95, 30, step = 5),
  	checkboxGroupInput("typeInput", "Product type",
  										 choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
  										 selected = "WINE"),
  	uiOutput("countryOutput"),
  	actionButton("selectAll", "Select all countries", value = FALSE),
  	br(), br(),
  	hr(),
  	span("Data source:", 
  			 tags$a("OpenDataBC",
  			 			 href = "https://www.opendatabc.ca/dataset/bc-liquor-store-product-price-list-current-prices")),
  	br(),
  	span("Built following this tutorial: ", 
  			 a(href = "http://deanattali.com/blog/building-shiny-apps-tutorial/", "Building shiny apps tutorial")),
  	br(), br(),
  	em(
  		span("Created by Minzhi Liao"),
  		HTML("&bull;"),
  		span("Code", a(href = "https://github.com/STAT545-UBC-students/hw08-liao02x", "on GitHub"))
  	)
  ),
  
  dashboardBody(
  	div(img(src = "https://bit.ly/2DJBI9Z", class = "titleimg"), style="text-align: center;"),
  	tags$h3(tags$em(verbatimTextOutput("textresult"))),
  	hr(),
  	tabsetPanel(
  		tabPanel("Plot", plotOutput("coolplot")),
  		tabPanel("Table", checkboxInput("arrangePrice", "Sort by price"), DT::dataTableOutput("results"))
  	),
  	hr(),
  	downloadButton("downloadData", "Download result data"),
  	downloadButton("downloadPlot", "Download result plot")
  )
)

server <- function(input, output) {
  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }    
    filtered <- bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type %in% input$typeInput,
             Country %in% input$countryInput)
    if (input$arrangePrice) {
      filtered <- filtered %>%
        arrange(Price)
    }
    filtered
  })
  myplot <- reactive({
    ggplot(filtered(), aes(Alcohol_Content, fill = Type)) +
      geom_histogram(bins = input$binsNumber)
  })
  output$countryOutput <- renderUI({
    if (input$selectAll) {
      selectInput("countryInput", "Country",
                  sort(unique(bcl$Country)),
                  selected = unique(bcl$Country), multiple = TRUE)
    }
    else {
      selectInput("countryInput", "Country",
                  sort(unique(bcl$Country)),
                  selected = "CANADA", multiple = TRUE)
    }
  })
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    myplot()
  })
  
  output$results <- DT::renderDataTable({
    filtered()
  })
  output$textresult <- renderText({
    paste("We found ", nrow(filtered()), " option(s) for you.")
  })
  output$downloadData <- downloadHandler(
    filename = function () {
      paste0("data-", 
             input$priceInput[1],
             "-",
             input$priceInput[2],
             ".csv")
    },
    content = function(file) {
      write.csv(filtered(), file)
    },
    contentType = "text/csv"
  )
  output$downloadPlot <- downloadHandler(
    filename = function () {
      paste0("data-", 
             input$priceInput[1],
             "-",
             input$priceInput[2],
             ".png")
    },
    content = function(file) {
      ggsave(file, myplot())
    },
    contentType = "image/png"
  )
}

shinyApp(ui = ui, server = server)
