source("modules/reports/timeSeries.R")

reportsUI <- function(id){
  ns <- NS(id)
  navbarPage(
    "Resporte de datos",
    tabPanel("Series de tiempo", timeSeriesUI(ns("timeSeries"))),
    tabPanel("ICA")
  )
}
reports <- function(input, output, session, database){
  controllerTimeSeries <- callModule(timeSeries, "timeSeries", database = database)
}
