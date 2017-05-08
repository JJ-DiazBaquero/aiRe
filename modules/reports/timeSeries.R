library(openair)
timeSeriesUI <- function(id){
  ns <- NS(id)
  titlePanel("Series de tiempo")
  fluidPage(
    radioButtons(ns("AgregationLevel"), "Seleccion de agrupacion", 
                 choices = list(hora = 1, dia = 2, semana = 3, mes = 4, trimestre = 5, anio = 6),
                 inline = TRUE,
                 selected = 3),
    uiOutput(ns("stations")),
    plotlyOutput(ns("timeSeries"))
  )
}
timeSeries <- function(input, output, session, database){
  ns <- session$ns
  output$timeSeries <- renderPlotly({
    temporality = switch(input$AgregationLevel, '1' = 'hour', '2' = 'day', '3' = 'week', '4' = 'month', '5' = 'quarter', '6' = 'year')
    dataToAvg = data.frame(date = database[['data']][,1],
                           var = database[['data']][,which(colnames(database[['data']])==input$selectedStation)])
    dataAvged = timeAverage(dataToAvg, avg.time = temporality, interval = "hour")
    p = plot_ly(x = dataAvged$date,y = dataAvged$var,
                type = 'scatter', mode = 'lines')
    p <- layout(p, title = paste("Concentracion de",database$currentData,"en", input$selectedStation), 
                          xaxis = list(title = "Tiempo"), 
                          yaxis = list(title = paste("Concentracion de",database$currentData)))
    p
  })
  
  output$stations <- renderUI({
    options = colnames(database[['data']])[-1]
    options = setNames(options,colnames(database[['data']])[-1])
    selectInput(ns("selectedStation"), "Seleccionar estacion a mostrar", 
                choices = options,
                selected = options[1])
  })
}
