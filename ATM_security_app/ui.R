library(shinydashboard)
options(scipen=999)
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
source("fun.R")

#*********************
## Load data ####
#*********************


#load("./results/serie.290(features).RData")
#load("./results/m_serie.290(features).RData")
#years <- readRDS("./results/years.rds")
serie.290 <- readRDS("../datos_pruebas/serie.290(features).rds")
m_serie.290 <- readRDS("../datos_pruebas/m_serie.290(features).rds")
Ramos_grupos <- readRDS("../datos_pruebas/Ramos_grupos.rds")


#*****************************************
## 1. Header ####
#*****************************************
header <- dashboardHeader(
  title = h4("Pizarra F290"),
  dropdownMenuOutput("messageMenu")
)


#*****************************************
## 2. Sidebar ####
#*****************************************
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "Home", icon = icon("dashboard")),
    menuItem("Generador", tabName = "generador", icon = icon("wrench"), badgeLabel = "nuevo", 
             badgeColor = "green"),
    
    ## Menu generado en el server.R
    ## Se puede generar un menu que se active unicamente si se cumple una condicion deseada
    sidebarMenuOutput("menu")
  )
)

#*****************************************
## 3. Body ####
#*****************************************
body <- dashboardBody(
  tabItems(
    ## C1. First tab content ####
    #*****************************
    tabItem(tabName = "Home",
            fluidRow(
              box(title ="Panel de control",  width = 2,
                  status = "primary",solidHeader = TRUE,
                  selectInput("grupo_ramos", h3("Nivel del análisis"),
                              choices = names(Ramos_grupos), selected = "Ramos_Objetivo"
                  ),
                  
                  radioButtons("horizonte", h3("Horizonte de tiempo"), 
                               choices = list("mensual", "anual"), selected = "mensual"
                  ),
                  selectInput("periodo", h3("Tipo de periodo"), choices = periodos(), 
                              selected = periodos()[length(periodos())]
                  ),
                  selectInput("Ramos", h3("Seleccione varios:"), multiple = T, 
                              choices = c("Ramo1","Ramo2","Ramo3","Ramo4"), selected = "Ramo1", 
                              width = validateCssUnit("85%"))
              ),
              
              
              valueBoxOutput("PrEmitida_Box", width = 3),
              valueBoxOutput("PrDevengada_Box", width = 3),
              valueBoxOutput("ResTecnicio_Box", width = 3),
              
              box(title = "Participación del mercado",  width = 6,
                  status = "success",solidHeader = TRUE,
                  tabBox(width = 12, height = "600px",
                         selected = "Primas Emitidas",
                         # The id lets us use input$tabset1 on the server to find the current tab
                         id = "part_mercado", 
                         
                         tabPanel("Primas Emitidas", 
                                  plotlyOutput("pie_Emitidas")
                         ),
                         
                         tabPanel("Primas Devengadas", 
                                  plotlyOutput("pie_Devengadas")
                         ),
                         
                         tabPanel("Resultado Técnico",
                                  plotlyOutput("pie_ResTec")
                         )
                  )
              ),
              box(title = "Ranking",  width = 4,
                  status = "info",solidHeader = TRUE,
                  DT::dataTableOutput("tbl_rank")
                  
              )
            ) ## Final fluidRow 2
    ),
    
    ## C2. Second tab content ####
    #*****************************
    tabItem(tabName = "generador",
            fluidRow(
              tabBox(
                title = tagList(shiny::icon("gear"), "Info 2"),
                selected = "Tab2",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", height = "250px",
                tabPanel("Tab1", "First tab content"),
                tabPanel("Tab2", "Tab content 2")
              ),
              
              box(
                title = "Controls",
                width=3,
                collapsible = T, collapsed = F,
                uiOutput("moreControls")
              )
            ),
            
            fluidRow(
              tabBox(
                # Title can include an icon
                title = tagList(shiny::icon("gear"), "tabBox status"),
                tabPanel("Tab1",
                         "Currently selected tab from first box:",
                         verbatimTextOutput("tabset1Selected")
                ),
                tabPanel("Tab2", "Tab content 2")
              )
            )
    ),
    
    ## C3. 3th tab content ####
    #*****************************
    tabItem(tabName = "menuServer",
            h2("Generado desde el server")
    )
  )
)


#*****************************************
## 4. Compilation ####
#*****************************************
dashboardPage(header, sidebar, body)


