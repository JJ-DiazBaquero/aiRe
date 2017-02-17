comparativeAnalysisUI <- function(id) {
  ns <- NS(id)
  navbarPage(
    "Analisis comparativo",
    tabPanel("Parametros",
             fluidRow(
               column(4,
             h3("Rango de fechas 1"),
             textInput(ns("nameRange1"), "Agregar Nombre"),
             sliderInput(ns("rangeYear1"), "Anios:",
                         min = 1998, max = 2014, value = c(1998,2014)),
             sliderInput(ns("rangeMonth1"), "Mes:",
                         min = 1, max = 12, value = c(1,12)),
             selectInput(ns("typeOfWeek1"), label = "Numeracion de semanas:", 
                         choices = list("Dias de la semana" = 0, "Numero de la semana" = 1), 
                         selected = 0),
             uiOutput(ns("weekui1")),
             sliderInput(ns("rangeHour1"), "Hora:",
                         min = 0, max = 23, value = c(0,23)),
             checkboxInput(ns("use1"),"Usar rango 1", FALSE)
              ),
             column(4,
                    h3("Rango de fechas 2"),
                    textInput(ns("nameRange2"), "Agregar Nombre"),
                    sliderInput(ns("rangeYear2"), "Anios:",
                                min = 1998, max = 2014, value = c(1998,2014)),
                    sliderInput(ns("rangeMonth2"), "Mes:",
                                min = 1, max = 12, value = c(1,12)),
                    selectInput(ns("typeOfWeek2"), label = "Numeracion de semanas:", 
                                choices = list("Dias de la semana" = 0, "Numero de la semana" = 1), 
                                selected = 0),
                    uiOutput(ns("weekui2")),
                    sliderInput(ns("rangeHour2"), "Hora:",
                                min = 0, max = 23, value = c(0,23)),
                    checkboxInput(ns("use2"),"Usar rango 2", FALSE)
             ),
             column(4,
                    h3("Rango de fechas 3"),
                    textInput(ns("nameRange3"), "Agregar Nombre"),
                    sliderInput(ns("rangeYear3"), "Anios:",
                                min = 1998, max = 2014, value = c(1998,2014)),
                    sliderInput(ns("rangeMonth3"), "Mes:",
                                min = 1, max = 12, value = c(1,12)),
                    selectInput(ns("typeOfWeek3"), label = "Numeracion de semanas:", 
                                choices = list("Dias de la semana" = 0, "Numero del dia" = 1), 
                                selected = 0),
                    uiOutput(ns("weekui3")),
                    sliderInput(ns("rangeHour3"), "Hora:",
                                min = 0, max = 23, value = c(0,23)),
                    checkboxInput(ns("use3"),"Usar rango 3", FALSE)
             )),
             fluidRow(actionButton(ns("makeGraphs"),"Enviar cambios")),
             fluidRow(uiOutput(ns("stationsUI"))),
             fluidRow(tableOutput(ns("numObsTable")))
             ),
    tabPanel("Graficos",
             plotlyOutput(ns("boxplot")),
             plotlyOutput(ns("scatterplot")))
  )
}

comparativeAnalysis <- function(input, output, session, database) {
  ns <- session$ns
  #intervalData is the informacion of the data contain in each range of dates
  reactiveData <- reactiveValues(intervalData = list())
  
  output$weekui1 <- renderUI({
    if(input$typeOfWeek1 == 0){
      checkboxGroupInput(ns("rangeDay1"), "Dias de la semana:",
                         choices = c("L" = "lunes",
                                     "M" = "martes",
                                     "M" = "miercoles",
                                     "J" = "jueves",
                                     "V" = "viernes",
                                     "S" = "sabado",
                                     "D" = "domingo"),
                         inline = TRUE
      )
    }
    else if (input$typeOfWeek1 == 1){
      sliderInput(ns("rangeDay1"), "Numero del dia:",
                  min = 1, max = 31, value = c(1,31))
    }
  })
  
  output$weekui2 <- renderUI({
    if(input$typeOfWeek2 == 0){
      checkboxGroupInput(ns("rangeDay2"), "Dias de la semana:",
                         choices = c("L" = "lunes",
                                     "M" = "martes",
                                     "M" = "miercoles",
                                     "J" = "jueves",
                                     "V" = "viernes",
                                     "S" = "sabado",
                                     "D" = "domingo"),
                         inline = TRUE
      )
    }
    else if (input$typeOfWeek2 == 1){
      sliderInput(ns("rangeDay2"), "Numero del dia:",
                  min = 1, max = 31, value = c(1,31))
    }
  })
  
  output$weekui3 <- renderUI({
    if(input$typeOfWeek3 == 0){
      checkboxGroupInput(ns("rangeDay3"), "Dias de la semana:",
                         choices = c("L" = "lunes",
                                     "M" = "martes",
                                     "M" = "miercoles",
                                     "J" = "jueves",
                                     "V" = "viernes",
                                     "S" = "sabado",
                                     "D" = "domingo"),
                         inline = TRUE
      )
    }
    else if (input$typeOfWeek3 == 1){
      sliderInput(ns("rangeDay3"), "Numero del dia:",
                  min = 1, max = 31, value = c(1,31))
    }
  })
  
  output$stationsUI <- renderUI({
    checkboxGroupInput(ns("stations"), "Estaciones a visualizar",
                       choices = colnames(database[['data']])[-1],
                       selected = colnames(database[['data']])[-1],
                       inline = TRUE
    )
  })
  
  output$numObsTable <- renderTable({
    reactiveData$intervalData
    isolate({
      obsTable = data.frame(Estacion = names(database[['data']]), row.names = "Estacion")
      row.names(obsTable)[1] = "Maximo de observaciones (maximo teorico)"
      for(i in 1:3){
        if(input[[paste("use",i,sep="")]] == TRUE){
          obsTable[paste("Rango",i)] = colSums(!is.na(reactiveData$intervalData[[i]]))
        }
      }
      #Check if the dataframe is empty, this avoid crash when there is no range dates selected (no columns in dt)
      if(is.na(colnames(obsTable)[1])){
        obsTable = NULL
      }
      obsTable
    })
  },
  caption = "Numero de observaciones en cada estacion y rango de fecha", 
  rownames = TRUE, colnames = TRUE, striped = TRUE, bordered = TRUE
  )
  
  calcAvailavility<- observe({
    input$makeGraphs
    isolate({
      data = list()
      for(i in 1:3){
        if(input[[paste("use",i,sep="")]] == TRUE){
          #The user chooses by number of the day
          if(input[[paste("typeOfWeek",i,sep="")]] == 1){
            #get Number of day
            days = seq(from = input[[paste("rangeDay",i,sep="")]][1], to = input[[paste("rangeDay",i,sep="")]][2])
            dates = data.frame(date = database[['data']][,1])
            dates$day = as.numeric(format(dates$date, "%d"))
            dates = dates[dates$day %in% days,]
          }
          #The user chooses by weekday
          if(input[[paste("typeOfWeek",i,sep="")]] == 0){
            #get weekdays selected by user
            #The functionality of this change on the language of the machine (originally done in spanish)
            dates = data.frame(date = database[['data']][,1])
            dates = cutData(dates, type = "weekday", start.day = 1)
            dates$weekday = iconv(dates$weekday,to="ASCII//TRANSLIT")
            dates = dates[dates$weekday %in% input[[paste("rangeDay",i,sep="")]],]
          }
          
          #Get the hours of interest (the +1 is for the numeric transformation of openAir factor hour)
          hours = seq(from = input[[paste("rangeHour",i,sep="")]][1]+1, to = input[[paste("rangeHour",i,sep="")]][2]+1)
          dates = cutData(dates, type = "hour")
          dates = dates[as.numeric(dates$hour) %in% hours,]
          
          #Get the months of interest
          months = seq(from = input[[paste("rangeMonth",i,sep="")]][1], to = input[[paste("rangeMonth",i,sep="")]][2])
          dates$month = as.numeric(format(dates$date, "%m"))
          dates = dates[dates$month %in% months,]
          
          #Get the years of interest
          years = seq(from = input[[paste("rangeYear",i,sep="")]][1], to = input[[paste("rangeYear",i,sep="")]][2])
          dates$year = as.numeric(format(dates$date, "%Y"))
          dates = dates[dates$year %in% years,]
          reactiveData$intervalData[[i]] = database[['data']][database[['data']][,1] %in% dates$date,]
        }
      }
    })
  })
  
  output$boxplot <- renderPlotly({
    p = plot_ly(x = input$stations, type = "box", visible = FALSE)
    for(i in 1:3){
      if(input[[paste("use",i,sep="")]] == TRUE){
        info = reactiveData$intervalData[[i]]
        info = stack(info, select = input$stations)
        p = add_trace(p, y = info$values, x = info$ind, 
                      name = input[[paste("nameRange",i,sep="")]], visible = TRUE)
      }
    }
    p = layout(p,boxmode = "group")
    p
  })
  
  #output$scatterplot <- renderPlotly({
  #  p = plot_ly(x = input$stations, type = "scatter", visible = FALSE)
  #  for(i in 1:3){
  #    if(input[[paste("use",i,sep="")]] == TRUE){
  #      info = reactiveData$intervalData[[i]]
  #      info = stack(info, select = input$stations)
  #      p = add_trace(dMeans, x = ~ind, y = ~values, 
  #                    type = "scatter", error_y = list(value = dsd$values),
  #                    visible = TRUE)
  #    }
  #  }
  #  p = layout(p,scattermode = "group")
  #  p
  #})
  
}