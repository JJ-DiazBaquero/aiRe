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
source("modules/dataCleaning.R")

# Define UI for application that draws a histogram
shinyUI(navbarPage("Proyecto de calidad de aire",inverse = T,
  tabPanel("Descripción",
           descriptionUI("functionDescription")),
  tabPanel("Carga de datos"),
  tabPanel("Limpieza de datos",
           dataCleaningUI("dataCleaning")),
  tabPanel("Análisis de datos"),
  tabPanel("Reportes"),
  tabPanel("Cuadro de control")
  )
)