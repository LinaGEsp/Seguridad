library(shinydashboard)
options(scipen=999)
library(shiny)
library(shinyWidgets)
library(shinydashboardPlus)
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

# cajeros <- readRDS("./data/BBVA/data_wrangling/cajeros/201907_Cajeros.rds")
# cajeros_def <- readRDS("./data/BBVA/data_wrangling/cajeros/201907_def_Cajeros.rds")
# caj_vecinos <- readRDS("./data/BBVA/data_wrangling/cajeros_vecinos/201907_cajeros_vecinos.rds")
# oficinas <- readRDS("./data/BBVA/data_wrangling/oficinas/201907-Oficinas.rds")
# vandalismo <- readRDS("./data/BBVA/data_wrangling/vandalismo/201907-geo_vandalismo.rds")

serie.290 <- readRDS("./data/datos_pruebas/serie.290(features).rds")
m_serie.290 <- readRDS("./data/datos_pruebas/m_serie.290(features).rds")
Ramos_grupos <- readRDS("./data/datos_pruebas/Ramos_grupos.rds")

#*****************************************
## 1. Header ####
#*****************************************
header <- dashboardHeader(
  title = h4("Seguridad - BBVA"),
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
    fluidRow(
      column(
        width = 6,
        align = "center",
        flipBox(
          id = 1,
          main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
          header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
          front_title = "John Doe",
          back_title = "About John",
          "Lorem ipsum dolor sit amet, consectetur adipiscing elit, 
          sed do eiusmod tempor incididunt ut labore et dolore magna 
          aliqua. Ut enim ad minim veniam, quis nostrud exercitation 
          ullamco laboris nisi ut aliquip ex ea commodo consequat. 
          Duis aute irure dolor in reprehenderit in voluptate velit 
          esse cillum dolore eu fugiat nulla pariatur. Excepteur sint 
          occaecat cupidatat non proident, sunt in culpa qui officia 
          deserunt mollit anim id est laborum",
          fluidRow(
            dashboardLabel("Label 1", status = "info"),
            dashboardLabel("Label 2", status = "success"),
            dashboardLabel("Label 3", status = "warning"),
            dashboardLabel("Label 4", status = "primary"),
            dashboardLabel("Label 5", status = "danger")
          ),
          hr(),
          fluidRow(
            column(
              width = 6,
              align = "center",
              starBlock(grade = 5),
              starBlock(grade = 5, color = "olive"),
              starBlock(grade = 1, color = "maroon"),
              starBlock(grade = 3, color = "teal")
            ),
            column(
              width = 6,
              align = "center",
              appButton(
                url = "http://google.com",
                label = "Users",
                icon = "fa fa-users",
                enable_badge = TRUE,
                badgeColor = "purple",
                badgeLabel = 891
              ),
              appButton(
                label = "Edit",
                icon = "fa fa-edit",
                enable_badge = FALSE,
                badgeColor = NULL,
                badgeLabel = NULL
              )
            )
          ),
          back_content = tagList(
            column(
              width = 12,
              align = "center",
              sliderInput(
                "obs", 
                "Number of observations:",
                min = 0, 
                max = 1000, 
                value = 500
              )
            ),
            plotOutput("distPlot")
          )
        )
    ),
    column(
      width = 6,
      align = "center",
      flipBox(
        id = 2,
        main_img = "https://image.flaticon.com/icons/svg/149/149073.svg",
        header_img = "https://image.flaticon.com/icons/svg/119/119598.svg",
        front_title = "Johanna Doe",
        back_title = "About Johanna",
        fluidRow(
          column(
            width = 6,
            align = "center",
            boxPad(
              color = "green",
              descriptionBlock(
                header = "8390",
                text = "VISITS",
                right_border = FALSE,
                margin_bottom = TRUE
              ),
              descriptionBlock(
                header = "30%",
                text = "REFERRALS",
                right_border = FALSE,
                margin_bottom = TRUE
              ),
              descriptionBlock(
                header = "70%",
                text = "ORGANIC",
                right_border = FALSE,
                margin_bottom = FALSE
              )
            )
          ),
          column(
            width = 6,
            align = "center",
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit, 
            sed do eiusmod tempor.",
            br(),
            verticalProgress(
              value = 10,
              striped = TRUE,
              active = TRUE
            ),
            verticalProgress(
              value = 50,
              active = TRUE,
              status = "warning",
              size = "xs"
            ),
            verticalProgress(
              value = 20,
              status = "danger",
              size = "sm",
              height = "60%"
            )
          )
        ),
        back_content = tagList(
          column(
            width = 12,
            align = "center",
            radioButtons(
              "dist", 
              "Distribution type:",
              c("Normal" = "norm",
                "Uniform" = "unif",
                "Log-normal" = "lnorm",
                "Exponential" = "exp"
              )
            )
          ),
          plotOutput("plot")
        )
    )
    )
  )
),
#title = "flipBox"
    
    ## C2. Second tab content ####
    #*****************************
    tabItem(tabName = "Plots",
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
    
    ## C3. Third tab content ####
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
    
    ## C4. Forth tab content ####
    #*****************************
    tabItem(tabName = "menuServer",
            h2("Generado desde el server")
    )
)

#*****************************************
## 4. Compilation ####
#*****************************************
dashboardPage(header, sidebar, body)


