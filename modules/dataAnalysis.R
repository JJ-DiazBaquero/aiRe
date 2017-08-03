source("modules/dataAnalysis/dataAvailability.R")
source("modules/dataAnalysis/comparativeAnalysis.R")
source("modules/dataAnalysis/excedenceAnalysis.R")
dataAnalysisUI <- function(id) {
  ns <- NS(id)
  navbarPage(
    "Herramientas de analisis de datos",
    tabPanel("Disponibilidad de datos",
             dataAvailabilityUI(ns("dataAvailability"))),
    tabPanel("Analisis comparativos",
             comparativeAnalysisUI(ns("comparativeAnalysis"))),
    tabPanel("Dias que superan la norma",
             excedenceAnalysisUI(ns("excedenceAnalysis")))
  )
}
dataAnalysis <- function(input, output, session, database){
  controllerAvailability <- callModule(dataAvailability, "dataAvailability", database = database)
  controllerComparativeAnalysis <- callModule(comparativeAnalysis, "comparativeAnalysis", database = database)
  controllerExcedenceAnalysis <- callModule(excedenceAnalysis, "excedenceAnalysis", database = database)
}
  
