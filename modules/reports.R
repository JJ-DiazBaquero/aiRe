source("modules/reports/timeSeries.R")
source("modules/reports/ICA.R")
reportsUI <- function(id){
  ns <- NS(id)
  navbarPage(
    "Reporte de datos",
    tabPanel("Series de tiempo", timeSeriesUI(ns("timeSeries"))),
    tabPanel("ICA", ICAUI(ns("ICA")))
  )
}
reports <- function(input, output, session, database){
  controllerTimeSeries <- callModule(timeSeries, "timeSeries", database = database)
  controllerICA <- callModule(ICA, "ICA", database = database)
}
