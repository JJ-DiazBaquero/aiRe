library("openair")
dataAvailabilityUI <- function(id) {
  ns <- NS(id)
  titlePanel("Disponibilidad de datos")
  fluidPage(title = "Disponibilidad de datos",
            actionButton(ns("recalculateMatrix"),"Calcular disponibilidad de datos"),
            plotlyOutput(ns("heatMap")))
}

dataAvailability <- function(input, output, session, database) {
  dataSummary <- reactiveValues(data = list(), plotData = list())
  calcAvailavility <- observe({
    input$recalculateMatrix
    isolate({
      if(input$recalculateMatrix == 0) return(NULL)
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
    if(input$recalculateMatrix == 0) return(NULL)
    #Check type of data
    if(database$dataType[database$currentData] == 'auto'){
      
      # estations = c("Cade Energia", "Carvajal","Cazuca","Central \n de Mezclas",
      #                "CAR","Chico.lago \n Sto.Tomas.","Fontibon",
      #                "Guaymaral","Kennedy","Las Ferias","MinAmbiente","Olaya","Puente \n Aranda",
      #                "San \n Cristobal","Suba","Tunal","Univ \n Nacional","Usaquen")
      # estations = unique(as.character(dataSummary$plotData[['Estacion']]))
      # usarPM10 = c(5,2,10,8,9,11,14,15,16,18,13)
      
      # estations = c("Carvajal","CAR","Engativa","Guaymaral","Kennedy","Las Ferias",
      #    "MinAmbiente","San.Cristobal","Suba","Tunal","Usaquen")
      # usarPM2.5 = c(2,1,6,4,5,7,8,9,10,11)
      # estations = unique(as.character(dataSummary$plotData[['Estacion']]))
      # 
      # alguna = dataSummary$plotData[dataSummary$plotData$Estacion == "Guaymaral",]
      # alguna$Mean = rep(0, nrow(alguna))
      # plot(alguna$Mean)
      # alguna$Estacion = rep("Puente.Aranda",nrow(alguna))
      # dataSummary$plotDataEx= rbind(dataSummary$plotData,alguna)
      # dataSummary$plotDataEx= dataSummary$plotDataEx[dataSummary$plotDataEx$Estacion != "Engativa",]
      # 
      # nrow(dataSummary$plotDataEx)
      # nrow(alguna)
      # nrow(dataSummary$plotData)
      
      #This command removes any NaN value that may exist in the data
      dataSummary$plotData[is.nan(dataSummary$plotData[,2]),2] = 0
      p = plot_ly(
        z = dataSummary$plotData[['Mean']],
        type = "heatmap",
        x = as.POSIXct(dataSummary$plotData[["Fecha"]], origin = "1970-01-01"),
        xtype = 'date',
        y = as.character(dataSummary$plotData[['Estacion']]),
        connectgaps = FALSE
      )
      layout(p, title = paste("Disponibilidad de datos agregado diario para",database$currentData),
             xaxis = list(title = "Tiempo"))
      
    } else{
      dataSummary$plotData[is.nan(dataSummary$plotData[,2]),2] = 0
      
      color = list()
      
      p = plot_ly(
        z = dataSummary$plotData[['Mean']],
        type = "heatmap",
        x = as.POSIXct(dataSummary$plotData[["Fecha"]], origin = "1970-01-01"),
        xtype = 'date',
        y = as.character(dataSummary$plotData[['Estacion']]),
        colorscale = c(c(0, 'lightgray'), c(0.3, 'red'),c(0.6, 'green'),c(1, 'blue'))
      )
      layout(p, title = paste("Disponibilidad de datos agregado diario para",database$currentData),
             xaxis = list(title = "Tiempo"))
    }
  })
}
