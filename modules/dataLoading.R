# https://www.rstudio.com/resources/cheatsheets/

dataLoadingUI <- function(id, label = "Data Loading") {
  ns <- NS(id) 
  sidebarLayout(
    sidebarPanel(
      
    ),
    mainPanel(
      dataTableOutput(ns("summary"))
    )
  )
  
}

dataLoading <- function(input, output, session) {
  database <- reactive({
    read.csv("databases/PM2.5_1998_2014_Encsv.csv", sep=";", row.names=NULL, stringsAsFactors=FALSE)
  })
    output$summary = renderDataTable({
    database
  })
  return(database)
}