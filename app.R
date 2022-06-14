## LIBRARIES

library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)
library(viridis)
library(readr)
library(utils)
library(raster)
library(tmap)
library(shiny)
library(bslib)
library(shinyjs)

## DATA BASES IN GOOGLE SHEETS

# Bases de datos
bunda <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQmlwv2tKxwK-BLdhgWcmtDBWxq-OCKsrwaBBUJLgzFlyzR3Q82pdJrl7b3oxUX62lIM6JaC9_0znd2/pub?gid=0&single=true&output=csv")

# Capa shp
bunda_shp <- st_as_sf(bunda, coords = c("lon", "lat"), crs = "WGS 84")


#### UI ####
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "simplex"),
  tags$style(type = "text/css", "html, body {width:100%; height:100%} #map{height:80vh !important;}"),
    tabsetPanel(
    tabPanel("Mapa", 
    # Application title
    hr(), 

    # Sidebar with a slider input for number of bins
        # Show a plot of the generated distribution
          fluidRow(
            width = 7,h5("Bunda", style='color:#B3282D'), leafletOutput("map_1"), selectInput(inputId = "select_1", label = "", choices = unique(bunda$CATEGORIA))),
    ),
    tabPanel("Lista", DT::dataTableOutput("ziptable"))
    ))

#### SERVER ####

server <- function(input, output) {
  
    output$map_1 <- renderLeaflet({
      tm <- bunda_shp %>% 
        filter(CATEGORIA == input$select_1) %>%
        tm_shape() +
        tm_dots(id = "NOMBRE", col = "PRECIO", style = "fixed", breaks = c(100, 300, 500, 700, 900, 1000), palette = c("#3D31F6", "#EBC507" ,"#F59608" ,"#F24E0A", "#AA282D"), legend.show = FALSE) + 
        tm_basemap("OpenStreetMap")
      
      tmap_leaflet(tm)
    })
    
    
  
    output$ziptable <- DT::renderDataTable({
      tabla <- bunda %>% 
        dplyr::select(NOMBRE, CATEGORIA, PRECIO, Liga)
      
      
      DT::datatable(tabla, options = list(pageLength = 50))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
