library("openair")
dataAvailabilityUI <- function(id) {
  ns <- NS(id)
  titlePanel("Disponibilidad de datos")
  fluidPage(title = "Disponibilidad de datos",
            actionButton(ns("recalculateMatrix"),"Recalcular Matriz"),
            plotlyOutput(ns("heatMap")))
}

dataAvailability <- function(input, output, session, database) {
  dataSummary <- reactiveValues(data = list(), plotData = list())
  calcAvailavility <- observe({
    input$recalculateMatrix
    isolate({
      progress <- Progress$new(session, min  = 2, max = length(database[['data']]))
      progress$set(message="Analisis de datos - Matriz de disponibilidad",value =2)
      on.exit(progress$close())
      dataSummary$data = matrix(0,
                                nrow = nrow(database[['data']]),
                                ncol = length(database[['data']][-1]))
      cat("Calcular matriz de disponibilidad")
      dataSummary[['data']][!is.na(database[['data']][-1])] = 1
      
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
    #This command removes any NaN value that may exist in the data
    dataSummary$plotData[is.nan(dataSummary$plotData[,2]),2] = 0
    p = plot_ly(
      z = dataSummary$plotData[['Mean']],
      type = "heatmap",
      x = as.POSIXct(dataSummary$plotData[["Fecha"]], origin = "1970-01-01"),
      xtype = 'date',
      y = as.character(dataSummary$plotData[['Estacion']])
    )
    layout(p, title = paste("Disponibilidad de datos agregado diario para",database$currentData),
           xaxis = list(title = "Tiempo"))
  })
}
