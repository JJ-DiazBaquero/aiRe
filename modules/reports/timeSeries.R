timeSeriesUI <- function(id){
  ns <- NS(id)
  titlePanel("Series de tiempo")
  fluidPage(
    radioButtons(ns("AgregationLevel"), "Seleccion de agrupacion", 
                 choices = list(hora = 1, dia = 2, semana = 3, mes = 4, anio = 5),
                 selected = 2),
    uiOutput(ns("stations")),
    plotlyOutput(ns("timeSeries"))
  )
}
timeSeries <- function(input, output, session, database){
  
  output$stations <- renderUI({
    selectInput("selectedStation", "Seleccionar estacion a mostrar", 
                choices = colnames(database[['data']])[-1])
  })
  
  output$timeSeries <- renderPlotly({
    p = plot_ly(x = as.POSIXct(database[['data']][,1]),
                y = database[['data']][,which(colnames(database[['data']])==input$selectedStation)],
                type = 'scatter', mode = 'lines')
  })
}
