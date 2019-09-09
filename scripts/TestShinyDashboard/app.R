library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
shinyApp(
  ui = dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(
      setShadow("card"),
      fluidRow(
        column(
          width = 6,
          align = "center",
          flipBox(
            id = 1,
            main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
            header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
            front_title = "ABSTRACT",
            back_title = "About John",
            "Se plantea la necesidad de realizar un estudio estadístico que permita 
            determinar las necesidades de seguridad a atender para cada una de las 
            sucursales del BBVA en Colombia. En este sentido, el objetivo es 
            cuantificar el riesgo de hurto para cada sucursal, dependiendo de su 
            ubicación geográfica.
            
            ORIGEN DE LA INFORMACIÓN: Como punto de partida, el presente estudio
            determinó como fuente de información las Estadísticas delictivas  de la 
            Policía Nacional",
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
                  url = "https://www.policia.gov.co/grupo-informaci%C3%B3n-criminalidad/estadistica-delictiva",
                  label = "Info",
                  icon = "fas fa-info",
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
  title = "flipBox"
  ),
  server = function(input, output) {
    output$distPlot <- renderPlot({
      hist(rnorm(input$obs))
    })
    output$plot <- renderPlot({
      dist <- switch(input$dist,
                     norm = rnorm,
                     unif = runif,
                     lnorm = rlnorm,
                     exp = rexp,
                     rnorm)
      
      hist(dist(500))
    })
  }
  )