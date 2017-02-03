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
                                choices = list("Dias de la semana" = 0, "Numero de la semana" = 1), 
                                selected = 0),
                    uiOutput(ns("weekui3")),
                    sliderInput(ns("rangeHour3"), "Hora:",
                                min = 0, max = 23, value = c(0,23)),
                    checkboxInput(ns("use3"),"Usar rango 3", FALSE)
             )),
             fluidRow(actionButton(ns("makeGraphs"),"Enviar cambios")),
             fluidRow(uiOutput(ns("stationsUI")))
             ),
    tabPanel("Graficos",
             plotlyOutput(ns("boxplot")))
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
                                     "M" = "miércoles",
                                     "J" = "Thursday",
                                     "V" = "viernes",
                                     "S" = "sábado",
                                     "D" = "domingo"),
                         inline = TRUE
      )
    }
    else if (input$typeOfWeek1 == 1){
      sliderInput(ns("rangeDay1"), "Dias del mes:",
                  min = 1, max = 31, value = c(1,31))
    }
  })
  
  output$weekui2 <- renderUI({
    if(input$typeOfWeek2 == 0){
      checkboxGroupInput(ns("rangeDay2"), "Dias de la semana:",
                         choices = c("L" = "Monday",
                                     "M" = "Tuesday",
                                     "M" = "Wednesday",
                                     "J" = "Thursday",
                                     "V" = "Friday",
                                     "S" = "Saturday",
                                     "D" = "Sunday"),
                         inline = TRUE
      )
    }
    else if (input$typeOfWeek2 == 1){
      sliderInput(ns("rangeDay2"), "Dias del mes:",
                  min = 1, max = 31, value = c(1,31))
    }
  })
  
  output$weekui3 <- renderUI({
    if(input$typeOfWeek3 == 0){
      checkboxGroupInput(ns("rangeDay3"), "Dias de la semana:",
                         choices = c("L" = "Monday",
                                     "M" = "Tuesday",
                                     "M" = "Wednesday",
                                     "J" = "Thursday",
                                     "V" = "Friday",
                                     "S" = "Saturday",
                                     "D" = "Sunday"),
                         inline = TRUE
      )
    }
    else if (input$typeOfWeek3 == 1){
      sliderInput(ns("rangeDay3"), "Dias del mes:",
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
  
  calcAvailavility<- observe({
    input$makeGraphs
    isolate({
      data = list()
      if(input$use1 == TRUE){
        #The user chooses by number of the day
        if(input$typeOfWeek1 == 1){
          #get Number of day
          days = seq(from = input$rangeDay1[1], to = input$rangeDay1[2])
          dates = data.frame(date = database[['data']][,1])
          dates$day = as.numeric(format(dates$date, "%d"))
          dates = dates[dates$day %in% input$rangeDay1,]
        }
        #The user chooses by weekday
        if(input$typeOfWeek1 == 0){
          #get weekdays selected by user
          dates = data.frame(date = database[['data']][,1])
          dates = cutData(dates, type = "weekday", start.day = 1)
          dates = dates[dates$weekday %in% input$rangeDay1,]
        }
        
        #Get the hours of interest (the +1 is for the numeric transformation of openAir factor hour)
        hours = seq(from = input$rangeHour1[1]+1, to = input$rangeHour1[2]+1)
        dates = cutData(dates, type = "hour")
        dates = dates[as.numeric(dates$hour) %in% hours,]
        
        #Get the months of interest
        months = seq(from = input$rangeMonth1[1], to = input$rangeMonth1[2])
        dates$month = as.numeric(format(dates$date, "%m"))
        dates = dates[dates$month %in% months,]
        
        #Get the years of interest
        years = seq(from = input$rangeYear1[1], to = input$rangeYear1[2])
        dates$year = as.numeric(format(dates$date, "%Y"))
        dates = dates[dates$year %in% years,]
        reactiveData$intervalData[[1]] = database[['data']][database[['data']][,1] %in% dates$date,]
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
  
}