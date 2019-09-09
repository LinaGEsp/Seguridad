#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

# Define UI for application that draws a histogram
shinyUI(
  bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("mapAct", width = "100%", height = "100%"),
    absolutePanel(top = 10, right = 10,
                  sliderInput("animation", "Time:",
                              min = as.Date.character("2018-09-01"), # min(data$`Fecha apertura`, na.rm=T),
                              max = as.Date.character("2019-06-30"), #max(data$`Fecha apertura`, na.rm=T),
                              value = as.Date.character("2018-09-01"), #min(data$`Fecha apertura`, na.rm=T),
                              #timezone = "+0200",
                              step = 1,
                              animate = animationOptions(interval = 400, loop = TRUE))
    )
    
  )
)
