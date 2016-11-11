library("openair")
dataAvailabilityUI <- function(id) {
  ns <- NS(id)
  titlePanel("Disponibilidad de datos")
  fluidPage(title = "Disponibilidad de datos",
            plotlyOutput(ns("heatMap")))
}

dataAvailability <- function(input, output, session, database) {
  dataSummary <- reactiveValues(data = list(), plotData = list())
  calcAvailavility <- observe({
    progress <- Progress$new(session, min  = 2, max = length(database[['data']]))
    progress$set(message="Análisis de datos - Matriz de disponibilidad",value =2)
    on.exit(progress$close())
    database[['data']]
    isolate({
      dataSummary$data = matrix(0,
                                nrow = nrow(database[['data']]),
                                ncol = length(database[['data']][2:12]))
      cat("Calcular matriz de disponibilidad")
      dataSummary[['data']][!is.na(database[['data']][2:12])] = 1
      vars = list()
      for (i in 2:length(database[['data']])) {
        progress$inc(1)
        dataToAvg = data.frame(date = database[['data']][, 1], var = dataSummary$data[,i-1])
        dataAvged = timeAverage(dataToAvg, avg.time = "day", interval = "hour")
        vars$Fecha[length(vars$Fecha):(length(vars$Fecha) + length(dataAvged$date))] = dataAvged$date
        vars$Mean[length(vars$Mean):(length(vars$Mean) + length(dataAvged$var))] = dataAvged$var
        vars$Estacion[length(vars$Estacion):(length(vars$Estacion) + length(dataAvged$var))] = colnames(database[['data']])[i]
      }
      dataSummary$plotData = data.frame(vars)
    })
  })
  output$heatMap <- renderPlotly({
    p = plot_ly(
      z = dataSummary$plotData[['Mean']],
      type = "heatmap",
      x = as.POSIXct(dataSummary$plotData[['Fecha']], origin ="1970-01-01"),
      xtype = 'date',
      y = dataSummary$plotData[['Estacion']],
      smoothing = 1
    )
  })
}