library(openair)
timeSeriesUI <- function(id){
  ns <- NS(id)
  titlePanel("Series de tiempo")
  fluidPage(
    radioButtons(ns("AgregationLevel"), "Seleccion de agrupacion", 
                 choices = list(Hora = 1, Dia = 2, Semana = 3, Mes = 4, Trimestre = 5, Anio = 6),
                 inline = TRUE,
                 selected = 3),
    uiOutput(ns("stations")),
    plotlyOutput(ns("timeSeries"))
  )
}
timeSeries <- function(input, output, session, database){
  ns <- session$ns
  output$timeSeries <- renderPlotly({
    progress <- Progress$new(session, min  = 0, max = 3)
    progress$set(message="Series de tiempo",value =0)
    on.exit(progress$close())
    
    progress$inc(amount = 1, message = "Recuperando datos", detail = NULL)
    
    temporality = switch(input$AgregationLevel, '1' = 'hour', '2' = 'day', '3' = 'week', '4' = 'month', '5' = 'quarter', '6' = 'year')
    temporalidad = switch(input$AgregationLevel, '1' = 'hora', '2' = 'dia', '3' = 'semana', '4' = 'mes', '5' = 'trimestre', '6' = 'anio')
    dataToAvg = data.frame(date = database[['data']][,1],
                           var = database[['data']][,which(colnames(database[['data']])==input$selectedStation)])
    progress$inc(amount = 1, message = "Calculando agregacion", detail = NULL)
    dataAvged = timeAverage(dataToAvg, avg.time = temporality, interval = "hour")
    progress$inc(amount = 1, message = "Construyendo grafico", detail = NULL)
    p = plot_ly(x = dataAvged$date,y = dataAvged$var,
                type = 'scatter', mode = 'lines')
    p <- layout(p, title = paste("Promedio",temporalidad,"de",database$currentData,"en", input$selectedStation), 
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
