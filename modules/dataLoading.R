# https://www.rstudio.com/resources/cheatsheets/

dataLoadingUI <- function(id, label = "Data Loading") {
  ns <- NS(id)
  fluidPage(
  title = "Visualizacion de datos",
  selectInput(ns("dataBase"), label = h3("Seleccione una base datos"), 
              choices = list("Ninguna" = 0, "PM2.5" = 1, "PM10" = 2), 
              selected = 1),
  hr(),
  dataTableOutput(ns("summary"))
  )
}

dataLoading <- function(input, output, session) {
  
  database <- reactiveValues(data = read.csv("databases/PM2.5_1998_2014_Encsv.csv", sep=";", row.names=NULL, stringsAsFactors=TRUE),
                             datapm10 = read.csv("databases/PM10_1998_2014_Encsv.csv", sep=";", row.names=NULL, stringsAsFactors=TRUE))
  changeDates <- observe({
    isolate({
      database$data[,1] = as.POSIXct(as.character(database$data[,1]), format="%d/%m/%Y %H:%M")
      database$datapm10[,1] = as.POSIXct(as.character(database$datapm10[,1]), format="%d/%m/%Y %H:%M")
    })  
  })
  output$summary = renderDataTable({
    if(input$dataBase == 1){
      database$data
    }
    else if (input$dataBase == 2){
      database$datapm10
    }
  })
  return(database)
}