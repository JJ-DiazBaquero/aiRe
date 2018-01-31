source("modules/reports/timeSeries.R", encoding = "UTF-8")
source("modules/reports/ICA.R", encoding = "UTF-8")
source("modules/reports/trends.R", encoding = "UTF-8")

reportsUI <- function(id){
  ns <- NS(id)
  navbarPage(
    "Reporte de datos",
    tabPanel("Series de tiempo", timeSeriesUI(ns("timeSeries"))),
    tabPanel("ICA", ICAUI(ns("ICA"))),
    tabPanel("Tendencias", trendsUI(ns("trends")))
  )
}
reports <- function(input, output, session, database){
  controllerTimeSeries <- callModule(timeSeries, "timeSeries", database = database)
  controllerICA <- callModule(ICA, "ICA", database = database)
  controllerTrends <- callModule(trends, "trends", database = database)
}
