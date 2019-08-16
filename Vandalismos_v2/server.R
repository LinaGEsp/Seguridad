#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)
library(stringr)
library(htmltools)

# loading the data. It has the timestamp, lon, lat, and the accuracy (size of circles)
data <- readRDS("./data/201907-geo_vandalismo.RDS") %>%
  filter(!is.na(`Fecha apertura`)) %>%
  mutate(`Fecha apertura` = as.Date(`Fecha apertura`))

cajeros_bbva <- readRDS('./data/201907_def_Cajeros.RDS')

labs_vandal <- lapply(seq(nrow(data)), function(i) {
  paste0( '<p>', paste0('Módulo: ', data$Modulo[i]), '</p>', 
          paste0('Ubicación: ', data$`TIPO DE UBICACIÓN`[i]), '</p> ', 
          paste0('Lugar: ', data$`DIRECCION DEL ATM`[i], '-', data$CIUDAD[i]), '</p> ',
          paste0('Fecha: ', 
                 year(data$Mes[i]), '-', 
                 str_pad(month(data$Mes[i]), 2, pad = '0')),
          '</p><p>') 
})


# Define server logic required to draw a histogram
shinyServer(
  function(input, output) {
    #stuff in server
    filteredData <- reactive({
      #add rollified thing
      from<- input$animation
      till<- input$animation
      data %>% filter(`Fecha apertura` >= from & `Fecha apertura` <= till )
    })
    
    output$mapAct<-renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>%
        addTiles(group = "OSM") %>%
        addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
        
        # Markers Oficinas
        addCircleMarkers(~lon, ~lat, 
                         label = paste0('Servicio BBVA: ', cajeros_bbva$`NOMBRE DEL ATM`, 
                                        ' - ', cajeros_bbva$`DIRECCION DEL ATM`),
                         weight = 5, stroke = FALSE,
                         fillOpacity = 0.5,
                         radius = 6,
                         color = 'darkblue', 
                         data = cajeros_bbva)  %>%
        
        # Layers control
        addLayersControl(
          baseGroups = c("Positron", "OSM", "Toner Lite"),
          #overlayGroups = c("Vandalismo", "Hurto_Bancos"),
          options = layersControlOptions(collapsed = FALSE),
          position = c("bottomright")
        )#%>%
      
      #fitBounds(lng1 = 5,lat1 = 52,lng2 = 5.2,lat2 = 52.2)# set to reactive minimums
    })
    
    observe({
      labs_vandal <- lapply(seq(nrow(filteredData())), function(i) {
        paste0( '<p>', paste0('Módulo: ', filteredData()$Modulo[i]), '</p>', 
                paste0('Ubicación: ', filteredData()$`TIPO DE UBICACIÓN`[i]), '</p> ', 
                paste0('Lugar: ', filteredData()$`DIRECCION DEL ATM`[i], '-', filteredData()$CIUDAD[i]), '</p> ',
                paste0('Fecha: ', 
                       year(filteredData()$Mes[i]), '-', 
                       str_pad(month(filteredData()$Mes[i]), 2, pad = '0')),
                '</p><p>') 
      })
      
      leafletProxy("mapAct", data = filteredData()) %>%
        clearShapes() %>%
        addCircles(lng = ~lon, lat = ~lat,
                   radius = 50, weight = 30, fillOpacity = 1,color = "red",
                   label = lapply(labs_vandal, htmltools::HTML) #filteredData()$`DIRECCION DEL ATM`
        )
    })
    
  }
)
