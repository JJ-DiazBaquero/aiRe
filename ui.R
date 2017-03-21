#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source("modules/functionDescription.R")
source("modules/dataLoading.R")
source("modules/dataCleaning.R")
source("modules/dataAnalysis.R")

# Define UI for application that draws a histogram
shinyUI(
  navbarPage("Proyecto de calidad de aire",inverse = T,
  tabPanel("Descripción", 
           descriptionUI("functionDescription"),
           tags$head(tags$script(src="google-maps.js")),
           tags$head(tags$script(src="MainCtrl.js"))
           ),
  tabPanel("Carga de datos",
            dataLoadingUI("dataLoading")),
  tabPanel("Limpieza de datos",
           dataCleaningUI("dataCleaning")),
  tabPanel("Análisis de datos",
           dataAnalysisUI("dataAnalysis")),
  tabPanel("Reportes"),
  tabPanel("Cuadro de control")
  )
)
