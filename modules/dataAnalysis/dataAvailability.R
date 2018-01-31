library("openair")

library(RColorBrewer)
dataAvailabilityUI <- function(id) {
  ns <- NS(id)
  titlePanel("Disponibilidad de datos")
  fluidPage(title = "Disponibilidad de datos",
            tabsetPanel(
              tabPanel("Disponibilidad de datos", 
                       wellPanel(
                         p("El siguiente gráfico muestra la diponibilidad de datos a lo largo del tiempo, las zonas en amarillo tienen mayor disponibilidad de datos"),
                         actionButton(ns("recalculateMatrix"),"Calcular disponibilidad de datos")
                       ),plotlyOutput(ns("heatMap"))),
              tabPanel("Densidad de banderas",
                       wellPanel(
                         p("El siguiente gráfico muestra la distribución de banderas en los datos"),
                         sliderInput(ns("dateRangeFlags"), "Años:",
                                     min = 1998, max = 2016, value = c(2013,2016))
                       ),plotlyOutput(ns("heatMapFlags")))
            )
  )
}

dataAvailability <- function(input, output, session, database) {
  dataSummary <- reactiveValues(data = list(), plotData = list())
  calcAvailavility <- observe({
    input$recalculateMatrix
    isolate({
      if(input$recalculateMatrix == 0) return(NULL)
      progress <- Progress$new(session, min  = 2, max = length(database[['data']]))
      progress$set(message="Análisis de datos - Matriz de disponibilidad",value =2)
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
      
    } else if(database$dataType[database$currentData] == 'manual'){
      #dataSummary$plotData[is.nan(dataSummary$plotData[,2]),2] = 0
      p = plot_ly(
        z = dataSummary$plotData[['Mean']],
        type = "heatmap",
        x = as.POSIXct(dataSummary$plotData[["Fecha"]], origin = "1970-01-01"),
        xtype = 'date',
        y = as.character(dataSummary$plotData[['Estacion']])
      )
      layout(p, title = paste("Disponibilidad de datos agregado diario para",database$currentData),
             xaxis = list(title = "Tiempo"))
    }
  })
  output$heatMapFlags <- renderPlotly({
    progress <- Progress$new(session, min  = 1, max = 6)
    progress$set(message="Analisis de datos - Matriz de banderas",value =2)
    on.exit(progress$close())
    
    data = database[['dataFlags']]
    initialDate = as.POSIXct(paste("01-01-",input$dateRangeFlags[1],sep = ""),format = "%d-%m-%Y")
    finalDate = as.POSIXct(paste("31-12-",input$dateRangeFlags[2],sep = ""),format = "%d-%m-%Y")
    data = data[data[,1] >= initialDate & data[,1] <= finalDate,]
    
    progress$inc(1)
    
    y = lapply(data[,-1], levels)
    z = lapply(y, gsub, pattern = ",", replacement = ".")
    numbers = lapply(z, as.numeric)
    
    progress$inc(1)
    
    factors = c()
    for(i in 2:length(data)){
      data[!is.na(as.numeric(gsub(data[,i],pattern = ",", replacement = "."))),i] = NA
      factors = c(factors,levels(factor(data[,i])))
    }
    factors = unique(factors)
    
    progress$inc(1)
    
    
    #w = stack(data, select = paste(colnames(data)[-1], collapse = " + "))
    x = lapply(data[,-1], as.character)
    w = stack(x, select = -date)
    
    progress$inc(1)
    
    w$date = rep(data[,1], length(colnames(data[,-1])))
    
    f = levels(factor(w$values))
    interval.cols <- brewer.pal(length(f),"Set3")
    interval.cols = colorRampPalette(interval.cols)(length(f))
    
    color_s= data.frame(num = 1:(2*length(f)),
                        name = rep("",length(f)), stringsAsFactors = F)
    #color_s <- setNames(data.frame(color_s$num, color_s$name), NULL)
    j = 1
    for(i in seq(1,2*length(f),2)){
      color_s[i,1] = (j-1)/length(f)
      color_s[i+1,1] = (j)/length(f)
      color_s[i,2] = interval.cols[j]
      color_s[i+1,2] = interval.cols[j]
      j = j+1
    }
    w$values = factor(w$values)
    
    progress$set(message = "Renderizando gráfico")
    
    p = plot_ly(
      z = as.numeric(w$values),
      type = "heatmap",
      y = w$ind,
      x = w$date,
      xtype = 'date',
      colorscale= color_s,
      colorbar = list(tickmode='array', tickvals=1:length(levels(w$ind)), 
                      ticktext=levels(w$values), len=0.5))
    p
  })
}
