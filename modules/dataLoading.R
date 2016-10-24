# https://www.rstudio.com/resources/cheatsheets/

dataLoadingUI <- function(id, label = "Data Loading") {
  ns <- NS(id)
  fluidPage(
  title = "Visualizacion de datos",
  dataTableOutput(ns("summary"))
  )
}

dataLoading <- function(input, output, session) {
  database <- reactiveValues(data = read.csv("databases/PM2.5_1998_2014_Encsv.csv", sep=";", row.names=NULL, stringsAsFactors=TRUE))
  changeDates <- observe({
    isolate({
      database$data[,1] = as.POSIXct(as.character(database$data[,1]), format="%d/%m/%Y %H:%M")
    })  
  })
  output$summary = renderDataTable({
    database$data
  })
  return(database)
}