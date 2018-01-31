source("modules/dataAnalysis/dataAvailability.R", encoding = "UTF-8")
source("modules/dataAnalysis/comparativeAnalysis.R", encoding = "UTF-8")
source("modules/dataAnalysis/excedenceAnalysis.R", encoding = "UTF-8")
dataAnalysisUI <- function(id) {
  ns <- NS(id)
  navbarPage(
    "Herramientas de análisis de datos",
    tabPanel("Disponibilidad de datos",
             dataAvailabilityUI(ns("dataAvailability"))),
    tabPanel("Análisis comparativos",
             comparativeAnalysisUI(ns("comparativeAnalysis"))),
    tabPanel("Días que superan la norma",
             excedenceAnalysisUI(ns("excedenceAnalysis")))
  )
}
dataAnalysis <- function(input, output, session, database){
  controllerAvailability <- callModule(dataAvailability, "dataAvailability", database = database)
  controllerComparativeAnalysis <- callModule(comparativeAnalysis, "comparativeAnalysis", database = database)
  controllerExcedenceAnalysis <- callModule(excedenceAnalysis, "excedenceAnalysis", database = database)
}
  
