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
		Popular = if_else(Favorate == 1, 'Favorate', 'Other')
	)
startdate <- min(sakura$StartDate)
enddate <- max(sakura$EndDate)
sakura <- sakura %>% select(
	Cultivar,
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
	skin = "blue",
	dashboardSidebar(
		width = '20vw',
		checkboxGroupInput(
			"typeInput",
			"Popular",
			choices = c("Favorate", "Other"),
			selected = "Favorate",
			inline = TRUE
		),
		uiOutput("neighborhoodOutput"),
		actionButton("selectAllNeighborhood", "Select all neighborhoods", value = TRUE),
		uiOutput("cultivarOutput"),
		actionButton("selectAllCultivar", "Select all cultivars", value = TRUE),
		br(),
		br(),
		hr(),
		span(
			"Data source:",
			tags$a("http://maps.vcbf.ca/map/",
						 href = "http://maps.vcbf.ca/map/")
		),
		br(),
		br(),
		em(span("Created by Minzhi Liao"),
			 HTML("&bull;"),
			 span(
			 	"Code",
			 	a(href = "https://github.com/liao02x/Sakura-Vancouver", "on GitHub")
			 ))
	),
	
	dashboardBody(
		sliderInput(
			"dateInput",
			"Date",
			startdate,
			enddate,
			c(Sys.Date(), min(enddate, Sys.Date() + 7)),
			width = '100%',
			timeFormat = '%m/%d'
		),
		tags$h3(tags$em(verbatimTextOutput("textresult"))),
		hr(),
		leafletOutput(outputId = "sakuraMap"),
		DT::dataTableOutput("results"),
		hr(),
		downloadButton("downloadData", "Download result data")
	)
)

server <- function(input, output) {
	filtered <- reactive({
		if (is.null(input$cultivarInput)) {
			return(sakura)
		}
		if (is.null(input$neighborhoodInput)) {
			return(sakura)
		}
		filtered <- sakura %>%
			filter(
				input$dateInput[1] <= EndDate,
				StartDate <= input$dateInput[2],
				Popular %in% input$typeInput,
				Cultivar %in% input$cultivarInput,
				Neighborhood %in% input$neighborhoodInput
			)
		filtered
	})
	myplot <- reactive({
		ggplot(filtered(), aes(Alcohol_Content, fill = Type)) +
			geom_histogram(bins = input$binsNumber)
	})
	output$cultivarOutput <- renderUI({
		if (input$selectAllCultivar) {
			selectInput(
				"cultivarInput",
				"Cultivar",
				sort(unique(sakura$Cultivar)),
				selected = unique(sakura$Cultivar),
				multiple = TRUE
			)
		}
		else {
			selectInput(
				"cultivarInput",
				"Cultivar",
				sort(unique(sakura$Cultivar)),
				selected = unique(sakura$Cultivar),
				multiple = TRUE
			)
		}
	})
	output$neighborhoodOutput <- renderUI({
		if (input$selectAllNeighborhood) {
			selectInput(
				"neighborhoodInput",
				"Neighborhood",
				sort(unique(sakura$Neighborhood)),
				selected = unique(sakura$Neighborhood),
				multiple = TRUE
			)
		}
		else {
			selectInput(
				"neighborhoodInput",
				"Neighborhood",
				sort(unique(sakura$Neighborhood)),
				selected = unique(sakura$Neighborhood),
				multiple = TRUE
			)
		}
	})
	
	pal <-
		colorFactor(c("red", "navy"), domain = c("Favorate", "Other"))
	output$sakuraMap <- renderLeaflet({
		leaflet(filtered()) %>%
			setView(lng = -123.1471137,
							lat = 49.2341169,
							zoom = 12)  %>%
			addTiles() %>%
			addCircleMarkers(
				data = filtered(),
				lat = ~ Latitude,
				lng = ~ Longitude,
				label = ~ paste(Location,
												Cultivar,
												sep = ", "),
				popup = ~ paste(
					paste("<img src =", PicLink, "width=300 height=200>"),
					paste("<b>", Location, "<b/>"),
					paste("<b>", Cultivar, "<b/>"),
					paste("<i>", Description, "<i/>"),
					paste0(format(StartDate, "%m/%d"), "-", format(EndDate, "%m/%d")),
					sep = "<br/>"
				),
				color = ~ pal(Popular),
				radius = ~ ifelse(Popular == "Favorate", 12, 6),
				#										 clusterOptions = markerClusterOptions()
			)
	})
	
	output$results <- DT::renderDataTable({
		filtered() %>%
			select(-ForumLink,-PicLink,-Longitude,-Latitude)
	}, options = list(pageLength = 10))
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
}

shinyApp(ui = ui, server = server)
