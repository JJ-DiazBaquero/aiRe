library(openair)
timeSeriesUI <- function(id){
  ns <- NS(id)
  titlePanel("Series de tiempo")
  fluidPage(
    radioButtons(ns("AgregationLevel"), "Selección de agrupación", 
                 choices = list(Horario = 1, Díario = 2, Semanal = 3, Mensual = 4, Trimestral = 5, Anual = 6),
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
    temporalidad = switch(input$AgregationLevel, '1' = 'horario', '2' = 'diario', '3' = 'semanal', '4' = 'mensual', '5' = 'trimestral', '6' = 'anual')
    dataToAvg = data.frame(date = database[['data']][,1],
                           var = database[['data']][,which(colnames(database[['data']]) == input$selectedStation)])
    progress$inc(amount = 1, message = "Calculando agregación", detail = NULL)
    dataAvged = timeAverage(dataToAvg, avg.time = temporality, interval = "hour")
    progress$inc(amount = 1, message = "Construyendo gráfico", detail = NULL)
    p = plot_ly(x = dataAvged$date,y = dataAvged$var,
                type = 'scatter', mode = 'lines')
    p <- layout(p, title = paste("Promedio",temporalidad,"de",database$currentData,"en", input$selectedStation), 
                          xaxis = list(title = "Tiempo"), 
                          yaxis = list(title = paste("Concentración de",database$currentData)))
    p
  })
  
  output$stations <- renderUI({
    options = colnames(database[['data']])[-1]
    options = setNames(options,colnames(database[['data']])[-1])
    selectInput(ns("selectedStation"), "Seleccionar estación a mostrar", 
                choices = options,
                selected = options[1])
  })
}
