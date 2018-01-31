library(ggplot2)
library(scales)
library(grid)
trendsUI <- function(id){
  ns <- NS(id)
  titlePanel("Análisis de tendencias")
  fluidPage(
  uiOutput(ns("stations")),
  plotlyOutput(ns("trendPlot2")),
  plotlyOutput(ns("trendPlot"))
  )
}
trends <- function(input, output, session, database){
  ns <- session$ns
  
  output$stations <- renderUI({
    options = c(colnames(database[['data']])[-1],"Todas las estaciones")
    options = setNames(options,c(colnames(database[['data']])[-1],"Todas las estaciones"))
    selectInput(ns("selectedStation"), "Seleccionar estación a mostrar", 
                choices = options,
                selected = options[1])
  })
  
  output$trendPlot <- renderPlotly({
    hname <- seq(0:23)-1
    
    if(!is.null(input$selectedStation)){
      
      if(input$selectedStation =="Todas las estaciones"){
        data <- stack(database$data, drop= F)
        data['date'] = rep(database$data[,1],length(database$data)-1)
        data = data.frame(data$date, data$values)
      }
      else{
        data <- database$data[,c(1,which(colnames(database$data) ==input$selectedStation))]
      }
      colnames(data) = c("date",input$selectedStation)
      
      data = cutData(data, type = "year")
      data = cutData(data, type = "hour")
      
      ag = aggregate(data[,2] ~ data$year + data$hour, data, 
                     FUN = function(x) c(mean = mean(x), 
                                         ub = mean(x)+qnorm(0.95)*sd(x)/sqrt(length(x)),
                                         lb = mean(x)-qnorm(0.95)*sd(x)/sqrt(length(x))))
      colnames(ag) = c("year","hour","data")
      
      ag = data.frame(year = ag$year, hour = ag$hour, mean = ag$data[,'mean'],
                      lb = ag$data[,'lb'], ub = ag$data[,'ub'])
      listDataPerYear = split.data.frame(ag,ag$year)
      dataPerYear = data.frame(hour = levels(ag$hour))
      years = c()
      for(i in levels(ag$year)){
        if(nrow(listDataPerYear[[i]]) == 24){
          years[length(years)+1] = i
          dataPerYear[paste("mean",i,sep=".")] = listDataPerYear[[i]]['mean']
          dataPerYear[paste("ub",i,sep=".")] = listDataPerYear[[i]]['ub']
          dataPerYear[paste("lb",i,sep=".")] = listDataPerYear[[i]]['lb']
        }
      }
      
      p <- ggplot(dataPerYear,aes(hname))
      p <- p + theme_bw()
      for(i in years){
        p = p+geom_line(aes_string(y=names(dataPerYear[paste("mean",i,sep=".")]), group = 1),
                        color='gray5',
                        size=0.1,linetype = 2)
        p = p+geom_ribbon(aes_string(ymin = names(dataPerYear[paste("lb",i,sep=".")]),
                                      ymax = names(dataPerYear[paste("ub",i,sep=".")]),
                                      group=1,fill = i), alpha = 0.5)
      }
      p = p +   labs(x = "Hora del dia", colour = "")+
        theme(axis.text=element_text(size=16),
              axis.title=element_blank(),
              legend.text = element_text(size = 16, face = 'bold')) + 
        geom_hline(yintercept=50,linetype="dotdash")+ 
        ggtitle(paste("Perfil diario de",database$currentData,"para",input$selectedStation))
      ggplotly(p)
    }
    
  })
  
  output$trendPlot2 <- renderPlotly({
    hname <- seq(0:23)-1
    
    if(!is.null(input$selectedStation)){
      
      if(input$selectedStation =="Todas las estaciones"){
        data <- stack(database$data, drop= F)
        data['date'] = rep(database$data[,1],length(database$data)-1)
        data = data.frame(data$date, data$values)
      }
      else{
        data <- database$data[,c(1,which(colnames(database$data) ==input$selectedStation))]
      }
      colnames(data) = c("date",input$selectedStation)
      
      data = cutData(data, type = "year")
      data = cutData(data, type = "hour")
      
      ag = aggregate(data[,2] ~ data$year + data$hour, data, 
                     FUN = function(x) c(mean = mean(x), 
                                         ub = mean(x)+qnorm(0.95)*sd(x)/sqrt(length(x)),
                                         lb = mean(x)-qnorm(0.95)*sd(x)/sqrt(length(x))))
      colnames(ag) = c("year","hour","data")
      
      ag = data.frame(year = ag$year, hour = ag$hour, mean = ag$data[,'mean'],
                      lb = ag$data[,'lb'], ub = ag$data[,'ub'])
      ag$hour = as.numeric(as.character(ag$hour))
      ag$year = as.numeric(as.character(ag$year))
      
      
      g = plot_ly(ag, x = ~hour, y = ~mean, frame = ~year,
                  type ="scatter", mode="lines",
                  line = list(color = 'rgba(7, 164, 181, 1)'),
                  name = "PM2.5")
      add_ribbons(g, ymax = ~ub, ymin = ~lb,
                  line = list(color = 'rgba(7, 164, 181, 0.05)'),
                  fillcolor = 'rgba(7, 164, 181, 0.2)')
    }
  })
}
  