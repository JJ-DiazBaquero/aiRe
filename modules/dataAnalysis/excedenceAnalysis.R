library(openair)

excedenceAnalysisUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      uiOutput(ns("yearUI")),
      sliderInput(ns("excedenceThreshold"), "Indique porcentaje de tolerancia diaria a datos faltantes (%)",
                  min = 0,max = 100, value = 75)
    ),
    mainPanel(
      plotlyOutput(ns("excedencePlot")),
      tableOutput(ns('excedenceTable'))
    )
  )
}

excedenceAnalysis <- function(input, output, session, database) {
  ns <- session$ns

  output$yearUI <- renderUI({
    #get the range of years available in the data
    years = (as.POSIXlt.POSIXct(database$data[1,1])$year):as.POSIXlt.POSIXct(database$data[nrow(database$data)-1,1])$year + 1900
    sliderInput(ns("yearSelected"),"Seleccione fecha a analizar excedencias de la norma",
                min = years[1], max = years[length(years)], value = years[length(years)])
  })
  
  output$excedencePlot <- renderPlotly({
    
    excedenceDays = rep(0,length(database$data)-1)
    noDataDays = rep(0,length(database$data)-1)
    underLimitDays = rep(0,length(database$data)-1)
    stations = rep(0,length(database$data)-1)
    if(!is.null(input$yearSelected) && !(input$yearSelected == "Ninguno")){
      if(database$currentData == "pm2.5"){
        regulationThresh = 50
      }
      if(database$currentData == "pm10"){
        regulationThresh = 100
      }
      
      #Get the Data of the selected year
      firstDay = as.POSIXct(paste(input$yearSelected,"/01/01", sep = ""))
      lastDay = as.POSIXct(paste(input$yearSelected,"/12/31", sep = ""))
      yearData = database$data[difftime(database$data[,1],firstDay) >= 0 &
                     difftime(database$data[,1],lastDay) <= 0,]
      colnames(yearData)[1] = "date"
      
      avgData = timeAverage(yearData, avg.time = "day", data.thresh = input$excedenceThreshold)
      
      #Estimate number of days thats exceds the norm
      for(i in 2:length(database$data)){
        excedenceDays[i-1] = nrow(avgData[!is.na(avgData[,i]) & avgData[,i]>=regulationThresh,i])
        noDataDays[i-1]    = nrow(avgData[is.na(avgData[,i]),i])
        underLimitDays[i-1]= nrow(avgData) -excedenceDays[i-1] - noDataDays[i-1]
      }
      stations = colnames(avgData)[-1]
    }
    
    p <- plot_ly(x = stations,y = excedenceDays, type = "bar", name = "Dias que exceden la norma", marker = list(color = 'rgba(219, 64, 82, 0.7)') )
    p <- add_trace(p, y = noDataDays,type = "bar", name = "Dias sin informacion", marker = list(color = 'lightgray') )
    p <- add_trace(p, y = underLimitDays,type = "bar", name = "Dias por debajo de la norma", marker = list(color = 'rgba(50, 171, 96, 0.7)'))
    layout(p, barmode = 'stack', 
           title = paste("Numero de dias que exceden la norma por estacion","en",input$yearSelected,"para",database$currentData),
           yaxis = list(title = "Dias"))
    
  })
  
  output$excedenceTable <- renderTable({
    excedenceDays = rep(0,length(database$data)-1)
    noDataDays = rep(0,length(database$data)-1)
    stations = rep(0,length(database$data)-1)
    if(!is.null(input$yearSelected) && !(input$yearSelected == "Ninguno")){
      if(database$currentData == "pm2.5"){
        regulationThresh = 50
      }
      if(database$currentData == "pm10"){
        regulationThresh = 100
      }
      
      #Get the Data of the selected year
      firstDay = as.POSIXct(paste(input$yearSelected,"/01/01", sep = ""))
      lastDay = as.POSIXct(paste(input$yearSelected,"/12/31", sep = ""))
      yearData = database$data[difftime(database$data[,1],firstDay) >= 0 &
                                 difftime(database$data[,1],lastDay) <= 0,]
      colnames(yearData)[1] = "date"
      
      avgData = timeAverage(yearData, avg.time = "day", data.thresh = input$excedenceThreshold)
      
      #Estimate number of days thats exceds the norm
      for(i in 2:length(database$data)){
        excedenceDays[i-1] = nrow(avgData[!is.na(avgData[,i]) & avgData[,i]>=regulationThresh,i])
        noDataDays[i-1]    = nrow(avgData[is.na(avgData[,i]),i])
      }
      stations = colnames(avgData)[-1]
    }
    
    data <- data.frame( station = stations, x = excedenceDays, y = noDataDays)
    colnames(data) <- c("Estacion","Dias que superan la norma", "Dias sin datos")
    return(data)
  })
  
  
  
}