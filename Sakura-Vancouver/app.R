library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(leaflet.extras)

sakura <- read.csv("sakura.csv", stringsAsFactors = FALSE)
sakura <- sakura %>%
	mutate(
		StartDate = as.Date(Start, '%m/%d'),
		EndDate = as.Date(End, '%m/%d'),
		Popular = if_else(Favorate==1, 'Favorate', 'Other')
	)
startdate <- min(sakura$StartDate)
enddate <- max(sakura$EndDate)
sakura <- sakura %>% select(Cultivar, 
														Neighborhood,
														Location,
														StartDate,
														EndDate,
														Longitude,
														Latitude,
														Popular,
														Description,
														ForumLink,
														PicLink
														)

ui <- dashboardPage(
  dashboardHeader(title = "Sakura-Vancouver"),
  #								tags$li(a(href='http://www.bcliquorstores.com',
  #													tags$img(src='https://bit.ly/2ClIIHW',
  #																	 height='30',width='60')),
  #												class = "dropdown")),
  skin = "blue",
  dashboardSidebar(
  	width = '20vw',
  	sliderInput("dateInput", "Date", startdate, enddate, as.Date(c("2019-03-01", "2019-04-01"))),
  	uiOutput("neighborhoodOutput"),
  	checkboxGroupInput("typeInput", "Popular",
  										 choices = c("Favorate", "Other"),
  										 selected = "Favorate"),
  	uiOutput("cultivarOutput"),
  	actionButton("selectAll", "Select all cultivars", value = FALSE),
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
  	tags$h3(tags$em(verbatimTextOutput("textresult"))),
  	hr(),
  	leafletOutput(outputId = "sakuraMap"), 
  	DT::dataTableOutput("results"),
  	hr(),
  	downloadButton("downloadData", "Download result data"),
  	downloadButton("downloadPlot", "Download result plot")
  )
)

server <- function(input, output) {
  filtered <- reactive({
    if (is.null(input$cultivarInput)) {
      return(NULL)
    }    
    filtered <- sakura %>%
      filter(input$dateInput[1] <= EndDate, 
      			 StartDate <= input$dateInput[2],
      			 Popular %in% input$typeInput,
             Cultivar %in% input$cultivarInput)
    filtered
  })
  myplot <- reactive({
    ggplot(filtered(), aes(Alcohol_Content, fill = Type)) +
      geom_histogram(bins = input$binsNumber)
  })
  output$cultivarOutput <- renderUI({
    if (input$selectAll) {
      selectInput("cultivarInput", "Cultivar",
                  sort(unique(sakura$Cultivar)),
                  selected = unique(sakura$Cultivar), multiple = TRUE)
    }
    else {
      selectInput("cultivarInput", "Cultivar",
      						sort(unique(sakura$Cultivar)),
      						selected = unique(sakura$Cultivar), multiple = TRUE)
    }
  })
  output$neighborhoodOutput <- renderUI({
  	if (input$selectAll) {
  		selectInput("neighborhoodInput", "Neighborhood",
  								sort(unique(sakura$Neighborhood)),
  								selected = unique(sakura$Neighborhood), multiple = TRUE)
  	}
  	else {
  		selectInput("neighborhoodInput", "Neighborhood",
  								sort(unique(sakura$Neighborhood)),
  								selected = unique(sakura$Neighborhood), multiple = TRUE)
  	}
  })
  
  pal <- colorFactor(c("red", "navy"), domain = c("Favorate", "Other"))
  output$sakuraMap <- renderLeaflet({
  	leaflet(filtered()) %>% 
  		setView(lng = -123.1471137, lat = 49.2341169, zoom = 12)  %>% 
  		addTiles() %>% 
  		addCircleMarkers(data = filtered(), 
  							 lat = ~Latitude, 
  							 lng = ~Longitude, 
  							 label = ~as.character(Location),
  							 popup = ~as.character(Description),
  							 color = ~pal(Popular),
  							 radius = ~ifelse(Popular == "Favorate", 12, 6),
  #							 clusterOptions = markerClusterOptions()
  							 )
  })
  
  output$results <- DT::renderDataTable({
    filtered()
  })
  output$textresult <- renderText({
    paste(nrow(filtered()), 'option(s) found for you.')
  })
  output$downloadData <- downloadHandler(
    filename = function () {
      paste0("data-", 
             input$dateInput[1],
             "-",
             input$dateInput[2],
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
             input$dateInput[1],
             "-",
             input$dateInput[2],
             ".png")
    },
    content = function(file) {
      ggsave(file, myplot())
    },
    contentType = "image/png"
  )
}

shinyApp(ui = ui, server = server)
