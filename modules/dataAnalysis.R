source("modules/dataAnalysis/dataAvailability.R")

dataAnalysisUI <- function(id) {
  ns <- NS(id)
  navbarPage(
    "Herramientas de analisis de datos",
    tabPanel("Disponibilidad de datos",
             dataAvailabilityUI(ns("dataAvailability"))),
    tabPanel("Analisis temporales (*)"),
    tabPanel("Pronosticos (*)")
  )
}
dataAnalysis <- function(input, output, session, database){
  controllerAvailability <- callModule(dataAvailability, "dataAvailability", database = database)
}
  