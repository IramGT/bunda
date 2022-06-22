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
bunda <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSKk4mplMe0S_jRYPX8uC4ZA2mH4XmAQzKnCyCn4UktJrGF8DbOuHrBSQDzCFgFY_cQqD92SPSf4h24/pub?gid=0&single=true&output=csv")

# Capa shp
bunda_shp <- dplyr::select(bunda, ZONA, CATEGORIA, TIPO, PRECIO, lat, lon, LIGA, CAT) %>%
  na.omit() %>% 
  st_as_sf(coords = c("lon", "lat"), crs = "WGS 84")


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
            width = 7,h5("Bunda", style='color:#B3282D'), leafletOutput("map_1"), selectInput(inputId = "select_1", label = "", choices = unique(bunda_shp$ZONA))),
    ),
    tabPanel("Lista", DT::dataTableOutput("ziptable"))
    ))

#### SERVER ####

server <- function(input, output) {
  
  
  
    output$map_1 <- renderLeaflet({
      tm <- bunda_shp %>% 
        filter(ZONA == input$select_1) %>%
        tm_shape() +
        tm_dots(id = "ZONA", size = .07, alpha=.4, col = "CAT", style = "fixed", breaks = c(-Inf,11,21,31,Inf), palette = c("#4D12EB", "#D43E06" ,"#FACA00" ,"#2AD600"), legend.show = FALSE, popup.vars = c("Categoria" = "CATEGORIA", "Tipo" = "TIPO", "Precio" = "PRECIO", "Liga" = "LIGA"), popup.format = list(html.escape = F)) + 
        tm_basemap("OpenStreetMap") 
      
      tmap_leaflet(tm)
    })
    
    
  
    output$ziptable <- DT::renderDataTable({
      tabla <- bunda %>% 
        dplyr::select(ZONA, CATEGORIA, TIPO, PRECIO, LIGA) %>%
        na.omit() 
      
      
      DT::datatable(tabla, options = list(pageLength = 50), escape = FALSE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
