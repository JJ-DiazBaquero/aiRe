library(openair)
ICAUI <- function(id){
  ns <- NS(id)
  titlePanel("Indicador ICA")
  fluidPage(
    actionButton(ns("recalculateMatrix"),"Calcular indice"),
    sliderInput(ns("excedenceThreshold"), "Indique porcentaje permisible de datos válidos (%)",
                min = 0,max = 100, value = 75),
    plotlyOutput(ns("heatMapICA")),
    plotlyOutput(ns("daysOver"))
  )
}
ICA <- function(input, output, session, database){
  ns <- session$ns
  #------------------------------------------------- ICA Indicator--------------------------------
  #Generate ICA dataframe
  ICA_dataSummary <- reactiveValues(plotData = list(), days_over_df_verde = NULL,
                                    days_over_df_amarillo = NULL,
                                    days_over_df_naranja = NULL,
                                    days_over_df_rojo = NULL,
                                    days_over_df_purpura = NULL,
                                    days_over_df_marron = NULL)
  ICA_config <- reactiveValues(ICA_labels = NULL)
  calcICA <- observe({
    input$recalculateMatrix
    isolate({
      if(input$recalculateMatrix == 0) return(NULL)
      if(is.null(database[['data']]))return(NULL)
        ICA_vars = list()
        #Loading bar
        progress <- Progress$new(session, min  = 2, max = length(database[['data']]))
        progress$set(message="Analisis de datos - Indice de Calidad del Aire",value = 2)
        on.exit(progress$close())
        #Parse for plotly
        days_over_list = list()
        ICA_dataSummary$days_over_df_verde <- vector(mode="numeric", length=0)
        ICA_dataSummary$days_over_df_amarillo <- vector(mode="numeric", length=0)
        ICA_dataSummary$days_over_df_naranja <- vector(mode="numeric", length=0)
        ICA_dataSummary$days_over_df_rojo <- vector(mode="numeric", length=0)
        ICA_dataSummary$days_over_df_purpura <- vector(mode="numeric", length=0)
        ICA_dataSummary$days_over_df_marron <- vector(mode="numeric", length=0)

        for (i in 2:length(database[['data']])) {
          #i recorreo filas
          dataToAvg = data.frame(date = database[['data']][, 1], var = database[['data']][,i])
          #dataAvgedDaily  = timeAverage(dataToAvg, avg.time = "day", interval = "hour",data.tresh = input$excedenceThreshold)
          dataAvged  = timeAverage(dataToAvg, avg.time = "day", interval = "hour",data.tresh = input$excedenceThreshold)
          #dataAvged = rollingMean(mydata = dataToAvg,pollutant = "var", width = 24,data.thresh = input$excedenceThreshold, align = "right")
          #Get rolling mean of the days
          
          ICA_vars$Fecha[length(ICA_vars$Fecha):(length(ICA_vars$Fecha) + length(dataAvged$date))] = dataAvged$date
          ICA_vars$Mean[length(ICA_vars$Mean):(length(ICA_vars$Mean) + length(dataAvged$var))] = dataAvged$var
          ICA_vars$Estacion[length(ICA_vars$Estacion):(length(ICA_vars$Estacion) + length(dataAvged$var))] = colnames(database[['data']])[i]
                    
          if (database$currentData == 'pm2.5') {
            #Porcentaje de días en cada color:
            ICA_dataSummary$days_over_df_verde[[i-1]] <- sum(ICA_vars$Mean >= 0 & ICA_vars$Mean <= 12, na.rm = TRUE)/sum(!is.nan(ICA_vars$Mean))
            ICA_dataSummary$days_over_df_amarillo[[i-1]] <- sum(ICA_vars$Mean > 12 & ICA_vars$Mean <= 36, na.rm = TRUE)/sum(!is.nan(ICA_vars$Mean))
            ICA_dataSummary$days_over_df_naranja[[i-1]] <- sum(ICA_vars$Mean > 36 & ICA_vars$Mean <= 56, na.rm = TRUE)/sum(!is.nan(ICA_vars$Mean))
            ICA_dataSummary$days_over_df_rojo[[i-1]] <- sum(ICA_vars$Mean > 56 & ICA_vars$Mean <= 150, na.rm = TRUE)/sum(!is.nan(ICA_vars$Mean))
            ICA_dataSummary$days_over_df_purpura[[i-1]] <- sum(ICA_vars$Mean > 150 & ICA_vars$Mean <= 250, na.rm = TRUE)/sum(!is.nan(ICA_vars$Mean))
            ICA_dataSummary$days_over_df_marron[[i-1]] <- sum(ICA_vars$Mean > 250, na.rm = TRUE)/sum(!is.nan(dataAvged$var))
          }  
          if (database$currentData == 'pm10') {
            ICA_dataSummary$days_over_df_verde[[i-1]] <- sum(dataAvged$var >= 0 & dataAvged$var <= 54, na.rm = TRUE)/sum(!is.nan(dataAvged$var))
            ICA_dataSummary$days_over_df_amarillo[[i-1]] <- sum(dataAvged$var > 54 & dataAvged$var <= 154, na.rm = TRUE)/sum(!is.nan(dataAvged$var))
            ICA_dataSummary$days_over_df_naranja[[i-1]] <- sum(dataAvged$var > 154 & dataAvged$var <= 254, na.rm = TRUE)/sum(!is.nan(dataAvged$var))
            ICA_dataSummary$days_over_df_rojo[[i-1]] <- sum(dataAvged$var > 254 & dataAvged$var <= 354, na.rm = TRUE)/sum(!is.nan(dataAvged$var))
            ICA_dataSummary$days_over_df_purpura[[i-1]] <- sum(dataAvged$var > 424 & dataAvged$var <= 504, na.rm = TRUE)/sum(!is.nan(dataAvged$var))
            ICA_dataSummary$days_over_df_marron[[i-1]] <- sum(dataAvged$var > 504, na.rm = TRUE)/sum(!is.nan(dataAvged$var))
          }

          progress$inc(1)
        }
        ICA_df <- data.frame(ICA_vars)
        ICA_config$ICA_labels <- c(0:7)
        if (database$currentData == 'pm2.5') {
          ICA_vars$ICA <- ifelse(is.na(ICA_df$Mean), 0, ifelse(ICA_df$Mean < 12, 7, ifelse(ICA_df$Mean  < 36, 6, ifelse(ICA_df$Mean  < 56, 5, ifelse(ICA_df$Mean  < 151, 4, ifelse(ICA_df$Mean  < 251, 3, ifelse(ICA_df$Mean < 351, 2, ifelse(ICA_df$Mean < 501, 1, 0))))))))
          ICA_config$ICA_labels <- c("", "352-501", "252-351", "152-251", "57-151", "37-56", "13-36", "0-12")
        }  
        
        if (database$currentData == 'pm10') {
          ICA_vars$ICA <- ifelse(is.na(ICA_df$Mean), 0, ifelse(ICA_df$Mean < 54, 7, ifelse(ICA_df$Mean  < 154, 6, ifelse(ICA_df$Mean  < 254, 5, ifelse(ICA_df$Mean  < 354, 4, ifelse(ICA_df$Mean  < 424, 3, ifelse(ICA_df$Mean < 504, 2, ifelse(ICA_df$Mean < 604, 1, 0))))))))
          ICA_config$ICA_labels <- c("", "505-604", "425-504", "355-424", "255-354", "155-254", "55-154", "0-54")
          
        }
        
        ICA_vars$Color <- ifelse(is.na(ICA_df$Mean), "#c8c8c8", ifelse(ICA_df$Mean < 54, "#00de28", ifelse(ICA_df$Mean  < 154, "#fffd40", ifelse(ICA_df$Mean  < 254, "ff8730", ifelse(ICA_df$Mean  < 354, "#ff3d2b", ifelse(ICA_df$Mean  < 424, "#984796", ifelse(ICA_df$Mean < 504, "#881924", ifelse(ICA_df$Mean < 604, "#881924", "#333333"))))))))
        
        #------------------------------------------------- ICA Days Over Regulation --------------------------------
        
        #names(days_over_df_verde) <- names(database[['data']])[-c(1)]
        
        # Create dataframe for plotly
        ICA_dataSummary$plotData = data.frame(ICA_vars)

                
    })
  })
  
  #------------------------------------------------- Plotly Chart Setup --------------------------------
  
  output$heatMapICA <- renderPlotly({
    if(is.null(ICA_dataSummary$plotData[['Fecha']]))return(NULL)
    
    df_colors = data.frame(range=c(0:15), colors=c(0:15))
    color_s <- setNames(data.frame(df_colors$range, df_colors$colors), NULL)
    for (i in 1:16) {
      ifelse(i == 1 | i == 2, color_s[[2]][[i]] <- "#c8c8c8",ifelse(i == 3 | i == 4 | i == 5 | i == 6 , color_s[[2]][[i]] <- "#851823", ifelse(i == 7 | i == 8, color_s[[2]][[i]] <- "#984796" , ifelse(i == 9 | i == 10, color_s[[2]][[i]] <- "#ff3d2b", ifelse(i == 11 | i == 12, color_s[[2]][[i]] <- "#ff8730", ifelse(i == 13 | i == 14, color_s[[2]][[i]] <- "#fffd40", ifelse(i == 15 | i == 16, color_s[[2]][[i]] <- "#00de28", "#c8c8c8")))))))
      color_s[[1]][[i]] <-  i / 16 - (i %% 2) / 16
    }
    
    p = plot_ly(
      z = ICA_dataSummary$plotData[['ICA']],
      type = "heatmap",
      x = as.POSIXct(ICA_dataSummary$plotData[['Fecha']], origin ="1970-01-01"),
      xtype = 'date',
      y = ICA_dataSummary$plotData[['Estacion']],
      colorscale = color_s,
      colorbar=list(tickmode='array', tickvals=c(0:7), ticktext=ICA_config$ICA_labels, len=0.9)
    )
    layout <- layout(p, title = paste("Indicador de Calidad del Aire (ICA) para", database$currentData))
  })
  
  output$daysOver <- renderPlotly({
    
    if(is.null(ICA_dataSummary$days_over_df_verde)) return(NULL)
    p = plot_ly(
      x = names(database[['data']])[-c(1)],
      y = c(ICA_dataSummary$days_over_df_verde),
      name = "Dias en verde",
      type = "bar"
    )
    amarillo <- add_trace(p , x = names(database[['data']])[-c(1)], y = c(ICA_dataSummary$days_over_df_amarillo), name = "Dias en amarillo", type = "bar")
    naranja <- add_trace(amarillo , x = names(database[['data']])[-c(1)], y = c(ICA_dataSummary$days_over_df_naranja), name = "Dias en naranja", type = "bar")
    rojo <- add_trace(naranja , x = names(database[['data']])[-c(1)], y = c(ICA_dataSummary$days_over_df_rojo), name = "Dias en rojo", type = "bar")
    purpura <- add_trace(rojo , x = names(database[['data']])[-c(1)], y = c(ICA_dataSummary$days_over_df_purpura), name = "Dias en purpura", type = "bar")
    marron <- add_trace(purpura , x = names(database[['data']])[-c(1)], y = c(ICA_dataSummary$days_over_df_marron), name = "Dias en marron", type = "bar")
    layout <- layout(marron, barmode = "stack", title = paste("Porcentaje de dias en cada nivel para ", database$currentData), 
                     xaxis = list(title = ""), 
                     yaxis = list(title = "Porcentaje de datos"))
  })
  
  #------------------------------------------------- Table with data for each day -------------------------------
  
}
