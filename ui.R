#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  fluidRow(
    column(12,
           titlePanel("Proyecto de calidad de aire")
    )
  ),
  fluidRow(
    column(4,
           h3("Acciones"),
           hr(),
           actionButton("dataLoading_btn", "Carga de datos", width ="80%", hover="dataLoading_hover"),
           actionButton("dataAvailability_btn", "Limpieza de datos",width="80%", hover = "dataAvailability_hover"),
           actionButton("dataAnalysis_btn", "Análisis de datos",width="80%", hover = "dataAnalysis_hover"),
           actionButton("reports_btn", "Reportes",width="80%", hover = "reports_hover"),
           actionButton("dashboard_btn", "Cuadro de control",width="80%", hover = "dashboard_hover")
    ),
    column(8,
           uiOutput("actionDescription")
    )
  )
))
