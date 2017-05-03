library(openair)
ICAUI <- function(id){
  ns <- NS(id)
  titlePanel("Indicador ICA")
  fluidPage(
    plotlyOutput(ns("heatMapICA"))
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
      for (i in 2:length(database[['data']])) {
        #i recorreo filas
        dataToAvg = data.frame(date = database[['data']][, 1], var = database[['data']][,i])
        dataAvged = timeAverage(dataToAvg, avg.time = "day", interval = "hour")
        ICA_vars$Fecha[length(ICA_vars$Fecha):(length(ICA_vars$Fecha) + length(dataAvged$date))] = dataAvged$date
        ICA_vars$Mean[length(ICA_vars$Mean):(length(ICA_vars$Mean) + length(dataAvged$var))] = dataAvged$var
        ICA_vars$Estacion[length(ICA_vars$Estacion):(length(ICA_vars$Estacion) + length(dataAvged$var))] = colnames(database[['data']])[i]
      }
      ICA_df <- data.frame(ICA_vars)
      if (database$currentData == 'pm2.5') {
        ICA_vars$ICA <- ifelse(is.na(ICA_df$Mean), 0, ifelse(ICA_df$Mean < 54, 7, ifelse(ICA_df$Mean  < 154, 6, ifelse(ICA_df$Mean  < 254, 5, ifelse(ICA_df$Mean  < 151, 4, ifelse(ICA_df$Mean  < 251, 3, ifelse(ICA_df$Mean < 351, 2, ifelse(ICA_df$Mean < 501, 1, 0))))))))
      }  
      
      if (database$currentData == 'pm10') {
        ICA_vars$ICA <- ifelse(is.na(ICA_df$Mean), 0, ifelse(ICA_df$Mean < 12, 7, ifelse(ICA_df$Mean  < 36, 6, ifelse(ICA_df$Mean  < 56, 5, ifelse(ICA_df$Mean  < 354, 4, ifelse(ICA_df$Mean  < 424, 3, ifelse(ICA_df$Mean < 504, 2, ifelse(ICA_df$Mean < 604, 1, 0))))))))
      }
      
      ICA_vars$Color <- ifelse(is.na(ICA_df$Mean), "#333333", ifelse(ICA_df$Mean < 54, "#00de28", ifelse(ICA_df$Mean  < 154, "#fffd40", ifelse(ICA_df$Mean  < 254, "ff8730", ifelse(ICA_df$Mean  < 354, "#ff3d2b", ifelse(ICA_df$Mean  < 424, "#984796", ifelse(ICA_df$Mean < 504, "#881924", ifelse(ICA_df$Mean < 604, "#881924", "#333333"))))))))
      df_colors = data.frame(range=c(0:6), colors=c(0:6))
      color_s <- setNames(data.frame(df_colors$range, df_colors$colors), NULL)
      
      for (i in 0:6) {
        ifelse(i == 0, color_s[[2]][[i+1]] <- "#000000",ifelse(i == 1, color_s[[2]][[i+1]] <- "#000000", ifelse(i == 2, color_s[[2]][[i+1]] <- "#984796", ifelse(i == 3, color_s[[2]][[i+1]] <- "#ff3d2b", ifelse(i == 4, color_s[[2]][[i+1]] <- "#ff8730", ifelse(i == 5, color_s[[2]][[i+1]] <- "#fffd40", ifelse(i == 6, color_s[[2]][[i+1]] <- "#00de28", "#000000")))))))
        color_s[[1]][[i+1]] <-  (1/6)*i
      }
      ICA_dataSummary$plotData = data.frame(ICA_vars)
      browser()
      output$heatMapICA <- renderPlotly({
        p = plot_ly(
          z = ICA_dataSummary$plotData[['ICA']],
          type = "heatmap",
          x = as.POSIXct(ICA_dataSummary$plotData[['Fecha']], origin ="1970-01-01"),
          xtype = 'date',
          y = ICA_dataSummary$plotData[['Estacion']],
          smoothing = 1,
          colorscale = color_s,
          colorbar=list(tickmode='array', tickvals=c(0:7), ticktext=c(0:7), len=0.6)
        )
      })
    }
  })
}
