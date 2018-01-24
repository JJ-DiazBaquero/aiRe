#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source("modules/functionDescription.R", encoding = "UTF-8")
source("modules/dataLoading.R")
source("modules/dataCleaning.R")
source("modules/dataAnalysis.R")
source("modules/reports.R")
# "<img src='University_of_Los_Andes_logo.svg' height = 70 align=right style= 'position:relative; z-index:-1; right:0;padding-right: 0px;' />"
library(shinythemes)

shinyUI(
  navbarPage(theme = shinytheme("lumen"),title = "Herramienta de análisis de datos de calidad de aire",
             windowTitle = "Proyecto de calidad de aire",inverse = T,
             tabPanel("Descripción", 
                      descriptionUI("functionDescription"),
                      tags$head(tags$script(src="google-maps.js")),
                      tags$head(tags$script(src="MainCtrl.js"))
             ),
             tabPanel("Carga de datos",
                      dataLoadingUI("dataLoading")),
             tabPanel("Limpieza de datos",
                      dataCleaningUI("dataCleaning")),
             tabPanel("Analisis de datos",
                      dataAnalysisUI("dataAnalysis")),
             tabPanel("Reportes",
                      reportsUI("reports"))
  )
)