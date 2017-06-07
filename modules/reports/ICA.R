library(openair)
ICAUI <- function(id){
  ns <- NS(id)
  titlePanel("Indicador ICA")
  fluidPage(
    plotlyOutput(ns("heatMapICA")),
    plotlyOutput(ns("daysOver"))
  )
}
ICA <- function(input, output, session, database){
  ns <- session$ns
  #------------------------------------------------- ICA Indicator--------------------------------
  #Generate ICA dataframe
  ICA_dataSummary <- reactiveValues(plotData = list())
  calcICA <- observe({
    if(is.numeric(database[['data']]$Kennedy)) {
      ICA_vars = list()
      #Parse for plotly
      days_over_df <- vector(mode="numeric", length=0)
      for (i in 2:length(database[['data']])) {
        #i recorreo filas
        dataToAvg = data.frame(date = database[['data']][, 1], var = database[['data']][,i])
        dataAvged = timeAverage(dataToAvg, avg.time = "day", interval = "hour")
        
        ICA_vars$Fecha[length(ICA_vars$Fecha):(length(ICA_vars$Fecha) + length(dataAvged$date))] = dataAvged$date
        ICA_vars$Mean[length(ICA_vars$Mean):(length(ICA_vars$Mean) + length(dataAvged$var))] = dataAvged$var
        ICA_vars$Estacion[length(ICA_vars$Estacion):(length(ICA_vars$Estacion) + length(dataAvged$var))] = colnames(database[['data']])[i]
        
        if (database$currentData == 'pm2.5') {
          days_over_df[[i-1]] <- sum(dataAvged$var > 0 & dataAvged$var < 12 , na.rm = TRUE)/sum(!is.nan(dataAvged$var))
        }  
        
        if (database$currentData == 'pm10') {
          days_over_df[[i-1]] <- sum(dataAvged$var > 355, na.rm = TRUE)/sum(!is.nan(dataAvged$var))
        }
        
      }
      ICA_df <- data.frame(ICA_vars)
      ICA_labels <- c(0:7)
      if (database$currentData == 'pm2.5') {
        ICA_vars$ICA <- ifelse(is.na(ICA_df$Mean), 0, ifelse(ICA_df$Mean < 12, 7, ifelse(ICA_df$Mean  < 36, 6, ifelse(ICA_df$Mean  < 56, 5, ifelse(ICA_df$Mean  < 151, 4, ifelse(ICA_df$Mean  < 251, 3, ifelse(ICA_df$Mean < 351, 2, ifelse(ICA_df$Mean < 501, 1, 0))))))))
        ICA_labels <- c("", "352-501", "252-351", "152-251", "57-151", "37-56", "13-36", "0-12")
        days_over_df[[i-1]] <- sum(dataAvged$var > 152, na.rm = TRUE)/sum(!is.nan(dataAvged$var))
      }  
      
      if (database$currentData == 'pm10') {
        ICA_vars$ICA <- ifelse(is.na(ICA_df$Mean), 0, ifelse(ICA_df$Mean < 54, 7, ifelse(ICA_df$Mean  < 154, 6, ifelse(ICA_df$Mean  < 254, 5, ifelse(ICA_df$Mean  < 354, 4, ifelse(ICA_df$Mean  < 424, 3, ifelse(ICA_df$Mean < 504, 2, ifelse(ICA_df$Mean < 604, 1, 0))))))))
        ICA_labels <- c("", "505-604", "425-504", "355-424", "255-354", "155-254", "55-154", "0-54")
        
        days_over_df[[i-1]] <- sum(dataAvged$var > 355, na.rm = TRUE)/sum(!is.nan(dataAvged$var))
      }
      
      ICA_vars$Color <- ifelse(is.na(ICA_df$Mean), "#c8c8c8", ifelse(ICA_df$Mean < 54, "#00de28", ifelse(ICA_df$Mean  < 154, "#fffd40", ifelse(ICA_df$Mean  < 254, "ff8730", ifelse(ICA_df$Mean  < 354, "#ff3d2b", ifelse(ICA_df$Mean  < 424, "#984796", ifelse(ICA_df$Mean < 504, "#881924", ifelse(ICA_df$Mean < 604, "#881924", "#333333"))))))))
      df_colors = data.frame(range=c(0:15), colors=c(0:15))
      color_s <- setNames(data.frame(df_colors$range, df_colors$colors), NULL)
      for (i in 1:16) {
        ifelse(i == 1 | i == 2, color_s[[2]][[i]] <- "#c8c8c8",ifelse(i == 3 | i == 4 | i == 5 | i == 6 , color_s[[2]][[i]] <- "#851823", ifelse(i == 7 | i == 8, color_s[[2]][[i]] <- "#984796" , ifelse(i == 9 | i == 10, color_s[[2]][[i]] <- "#ff3d2b", ifelse(i == 11 | i == 12, color_s[[2]][[i]] <- "#ff8730", ifelse(i == 13 | i == 14, color_s[[2]][[i]] <- "#fffd40", ifelse(i == 15 | i == 16, color_s[[2]][[i]] <- "#00de28", "#c8c8c8")))))))
        color_s[[1]][[i]] <-  i / 16 - (i %% 2) / 16
      }
      
      #------------------------------------------------- ICA Days Over Regulation --------------------------------
      
      names(days_over_df) <- names(database[['data']])[-c(1)]
      
      # Create dataframe for plotly
      ICA_dataSummary$plotData = data.frame(ICA_vars)

      #------------------------------------------------- Plotly Chart Setup --------------------------------

      output$heatMapICA <- renderPlotly({
        p = plot_ly(
          z = ICA_dataSummary$plotData[['ICA']],
          type = "heatmap",
          x = as.POSIXct(ICA_dataSummary$plotData[['Fecha']], origin ="1970-01-01"),
          xtype = 'date',
          y = ICA_dataSummary$plotData[['Estacion']],
          colorscale = color_s,
          colorbar=list(tickmode='array', tickvals=c(0:7), ticktext=ICA_labels, len=0.9)
        )
        layout <- layout(p, title = paste("Indicador de Calidad del Aire (ICA) para", database$currentData))
      })
      
      

      #------------------------------------------------- Table with data for each day -------------------------------
       
    }
  })
}
