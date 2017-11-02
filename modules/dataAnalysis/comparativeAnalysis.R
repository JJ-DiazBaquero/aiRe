comparativeAnalysisUI <- function(id) {
  ns <- NS(id)
  navbarPage(
    "Analisis comparativo",
    tabPanel("Fechas especiales",
             fluidRow(
              column(5,selectInput(ns("selectedSpecialDate"), label = "Seleccione evento a analizar:",
                         choices = list("Dia sin carro" = 0), 
                         selected = 0)),
              column(4,sliderInput(ns("specialDateYears"), "Anios:",
                         min = 2008, max = 2016, value = c(2008,2016))),
              column(3,sliderInput(ns("specialDateHours"), "Hora:",
                         min = 0, max = 23, value = c(0,23)))
             ),
             actionButton(ns("specialDateSubmit"),"Enviar cambios"),
             fluidRow(tableOutput(ns("specialDateNumObsTable")))
    ),
    tabPanel("Fechas Personalizadas",
             fluidRow(
               column(4,
             h3("Rango de fechas 1"),
             textInput(ns("nameRange1"), "Agregar Nombre"),
             sliderInput(ns("rangeYear1"), "Anios:",
                         min = 1998, max = 2016, value = c(1998,2016)),
             sliderInput(ns("rangeMonth1"), "Mes:",
                         min = 1, max = 12, value = c(1,12)),
             selectInput(ns("typeOfWeek1"), label = "Numeracion de semanas:", 
                         choices = list("Dias de la semana" = 0, "Numero del dia" = 1), 
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
                                min = 1998, max = 2016, value = c(1998,2016)),
                    sliderInput(ns("rangeMonth2"), "Mes:",
                                min = 1, max = 12, value = c(1,12)),
                    selectInput(ns("typeOfWeek2"), label = "Numeracion de semanas:", 
                                choices = list("Dias de la semana" = 0, "Numero del dia" = 1), 
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
                                min = 1998, max = 2016, value = c(1998,2016)),
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
             hr(),
             h2("Analisis de valores atipicos"),
             uiOutput(ns("outliersUI")))

  )
}

comparativeAnalysis <- function(input, output, session, database) {
  ns <- session$ns
  #intervalData is the informacion of the data contain in each range of dates
  reactiveData <- reactiveValues(intervalData = list())
  
  output$numObsTable <- renderTable({
    reactiveData$intervalData
    database[['data']]
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
  
  #This function search for the data in each personalizeddate range
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
  
  #This function search for the data in the selected special date
  calcSpecialDate <- observe({
    input$specialDateSubmit
    if(input$specialDateSubmit == 0)return(NULL)
    isolate({
      cat("Loading Special dates")
      specialDates = read.csv("databases/SpecialDates.csv", sep=";", row.names=NULL, stringsAsFactors=FALSE, fill = FALSE)
      specialDates[,1] = as.POSIXct(as.character(specialDates[,1]), format="%d/%m/%Y")
      specialDates[,2] = as.POSIXct(as.character(specialDates[,2]), format="%d/%m/%Y")
      
      daysEval = specialDates[["BogotaDiaSinCarro"]][!is.na(specialDates[["BogotaDiaSinCarro"]])]
      daysControl = specialDates[["Control.BogotaDiaSinCarro"]][!is.na(specialDates[["Control.BogotaDiaSinCarro"]])]
      
      hours = seq(from = input[["specialDateHours"]][1], to = input[["specialDateHours"]][2])
      years = seq(from = input[["specialDateYears"]][1], to = input[["specialDateYears"]][2])
      rangeEval = vector()
      for(j in 1:length(daysEval)){
        if(as.numeric(format(daysEval[j], "%Y") %in% years)){
          rangeEval[(length(rangeEval)+1):(length(rangeEval)+length(hours))] = paste(daysEval[j]," ",hours,":00", sep = "")
        }
      }
      rangeEval = as.POSIXct(as.character(rangeEval))
      
      rangeControl = vector()
      for(j in 1:length(daysControl)){
        if(as.numeric(format(daysControl[j], "%Y") %in% years)){
          rangeControl[(length(rangeControl)+1):(length(rangeControl)+length(hours))] = paste(daysControl[j]," ",hours,":00", sep = "")
        }
      }
      rangeControl = as.POSIXct(as.character(rangeControl))
      
      
      updateTextInput(session,"nameRange1", value =  "BogotaDiaSinCarro")
      updateTextInput(session,"nameRange2", value =  "Control.BogotaDiaSinCarro")
      reactiveData$intervalData[[1]] = database[['data']][database[['data']][,1] %in% rangeEval,]
      reactiveData$intervalData[[2]] = database[['data']][database[['data']][,1] %in% rangeControl,]
      updateCheckboxInput(session, "use1", value = T)
      updateCheckboxInput(session, "use2", value = T)
      
    })
  })
  
  output$specialDateNumObsTable <- renderTable({
    reactiveData$intervalData
    browser()
    if(length(obsTable[paste("Rango",i)]) != length(colSums(!is.na(reactiveData$intervalData[[i]]))))return(NULL)
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
  #This function build the boxplot with the information in each selected range
  #The "stack" method is the lenghtiest operation in this method
  #TODO search another way to build boxplot without using "stack" and keep same behaviour
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
    p = layout(p,boxmode = "group", title = paste("Comparacion de",database$currentData),
               xaxis = list(title = "Estacion"), 
               yaxis = list(title = paste("Concentracion de",database$currentData)))
    p
  })
  
  
  output$dataTableOutliers <- renderDataTable({
    if(is.null(input$selectOutliersRange))return(NULL)
    if(is.null(input$selectStation))return(NULL)
    
    info = NULL
    #Check if we need all intervalsData
    if(input$selectOutliersRange == 4){
      for(i in 1:3){
        if(input[[paste("use",i,sep="")]] == TRUE){
          info = rbind(info, reactiveData$intervalData[[i]])
        }
      }
      dates = rep(info[colnames(info)[1]],length(input$selectStation))
      info = stack(info, select = input$selectStation)
      info$date = dates[[1]]
    }else{
      info = reactiveData$intervalData[[as.numeric(input$selectOutliersRange)]]
      dates = rep(info[colnames(info)[1]],length(input$selectStation))
      info = stack(info, select = input$selectStation)
      info$date = dates[[1]]
    }
    
    #Get outliers
    vals <- boxplot(x = info$values)
    outliers = vals$out
    fechasOuts = info[info$values %in% outliers,]
    colnames(fechasOuts) = c("Concentración", "Estacion", "Fecha")
    fechasOuts
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
  
  #--------------------------------------------------------------------
  # Functions for filling UI Components
  #--------------------------------------------------------------------
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
  
  output$outliersUI <- renderUI({
    if(is.null(input$stations)) return(NULL)

    tagList(
    radioButtons(ns("selectOutliersRange"), "Seleccione un rango:",
                 choiceValues = 1:4,
                 choiceNames = c(input[[paste("nameRange",1,sep="")]],
                                 input[[paste("nameRange",2,sep="")]], 
                                 input[[paste("nameRange",3,sep="")]], "Todos")  ,inline = TRUE),
    
    checkboxGroupInput(ns("selectStation"), "Seleccione una estacion:",
    c(input$stations), inline = TRUE),
    
    dataTableOutput(ns("dataTableOutliers"))
    )
  })
}
