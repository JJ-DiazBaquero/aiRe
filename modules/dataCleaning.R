library(shiny)
library(plotly)
library("openair")
dataCleaningUI <- function(id){
  ns <- NS(id) 
  titlePanel("Limpieza de datos")
  sidebarLayout(
    sidebarPanel(
      selectInput(ns("dataBase"), label = h3("Seleccione una base de datos"), 
                  choices = list("PM2.5" = 1, "PM10" = 2), 
                  selected = 1),
      hr(),
      checkboxGroupInput(ns("generalRules"), label = h3("Reglas generales"), 
                         choices = list("1. Restriccion de cadenas de texto" = 1, 
                                        "2. Valores negativos y ceros" = 2, 
                                        "3. Limite de deteccion del equipo" = 3)
                         ),
      checkboxGroupInput(ns("particularRules"), label = h3("Reglas particulares a PM2.5"), 
                         choices = list("4. Asegurar PM10 > PM2.5" = 4)
      ),
      checkboxGroupInput(ns("densityRules"), label = h3("Reglas de densidad de datos"), 
                         choices = list("5. Eliminar datos aislados" = 5)
      ),
      actionButton(ns("applyRulesBtn"), "Aplicar reglas"),
      uiOutput(ns("uiDateRange")),
      downloadButton(ns('downloadData'), 'Descargar los datos')
    ),
    mainPanel(
      p("El siguiente grafico indica el porcentaje de los datos que pertenecen a la categoria en la derecha"),
      plotlyOutput(ns("plot")),
      tableOutput(ns("rulesTable")),
      verbatimTextOutput(ns("summary"))
    )
  )
}

dataCleaning <- function(input, output, session, database){
  ns <- session$ns
  rulesSummarydf = isolate(data.frame(Estaciones = colnames(database[['data']])[2:12]))
  for(i in 1:6){
    rulesSummarydf[paste("Regla ",i)] = rep(0,11)
  }
  rulesSummarydf["Datos validos"] = rep(1,11)
  #data is the dataframe with the summary of the data per rule in each station 
  #rulesMatrix is the 3 dimensional matrix of 1's and 0's if the data is valid, 1st: Rule, 2nd: Station, 3rd: obs
  #rulesApplied is the historic of rules applied
  rulesSummary <- reactiveValues(data = rulesSummarydf, 
                                 rulesMatrix = array(0,dim = isolate(c(6,length(database[['data']])-1,nrow(database[['data']])))),
                                 rulesApplied = NULL)
  
  output$uiDateRange <- renderUI({
    dateRangeInput(ns("dateRange"), 
                   "Rango de fechas a visualizar", 
                   language = "es", separator = "a", format = "yyyy-mm-dd",
                   start = as.character(database[['data']][1,1]),
                   end = strsplit(as.character(database[['data']][nrow(database[['data']]),1])," ")[[1]][1], 
                   min = as.character(database[['data']][1,1]), 
                   max = strsplit(as.character(database[['data']][nrow(database[['data']]),1])," ")[[1]][1])
  })
    
  # This method change the current database when the user change it
  changeCurrentDataBase <- observe({
    if(input$dataBase == 1){
      isolate({
        database$currentData = 'pm2.5'
        database[['data']] = database[['datapm2.5']]
        # Update Tables
        rulesSummarydf = data.frame(Estaciones = colnames(database[['data']])[-1])
        for(i in 1:6){
          rulesSummarydf[paste("Regla ",i)] = rep(0,length(database[['data']])-1)
        }
        rulesSummarydf["Datos validos"] = rep(1,length(database[['data']])-1)
      
        rulesSummary$data = rulesSummarydf
        rulesSummary$rulesMatrix = array(0,dim = isolate(c(6,length(database[['data']])-1,nrow(database[['data']]))))
        rulesSummary$rulesApplied = NULL
      })
    }
    else if (input$dataBase == 2){
      isolate({
        database$currentData = 'pm10'
        database[['data']] = database[['datapm10']]
        # Update tables
        rulesSummarydf = isolate(data.frame(Estaciones = colnames(database[['data']])[-1]))
        for(i in 1:6){
          rulesSummarydf[paste("Regla ",i)] = rep(0,length(database[['data']])-1)
        }
        rulesSummarydf["Datos validos"] = rep(1,length(database[['data']])-1)
        rulesSummary$data = rulesSummarydf
        rulesSummary$rulesMatrix = array(0,dim = isolate(c(6,length(database[['data']])-1,nrow(database[['data']]))))
        rulesSummary$rulesApplied = NULL
      })
    }
  })
  #This method apply the rules that are selected when the user click on button
  applyRules <- observe({
    input$applyRulesBtn
    isolate({
      progress <- Progress$new(session, min = 0, max = length(input$generalRules))
      on.exit(progress$close())
      progress$set(message = "Aplicando reglas", value = 0)
      # Rule 1 is remove all strings, here we coerce all values to numeric
      if(1 %in% isolate(input$generalRules) && !(1 %in% rulesSummary$rulesApplied)){
        progress$set(0, detail="Regla 1")
        #Encontrar todos los valores de string diferentes en la columna para asi quedar con solo números
        cat("Applying rule 1 \n")
        rule1Array = c(rep(0,nrow(database[['data']])))
        for(i in 2:length(database[['data']])){
          lvlsStr = levels(database[['data']][,i])
          lvlsInt = as.double(gsub(",",".",lvlsStr))
          strList = lvlsStr[is.na(lvlsInt)]
          rule1Array[database[['data']][,i] %in% strList] = TRUE
          database[['data']][rule1Array == 1,i] = NA
          database[['data']][,i] = as.numeric(gsub(",",".",database[['data']][,i]))
          database[['data']][is.nan(database[['data']][,i]),i] = NA
          rulesSummary$data[i-1,2] = sum(rule1Array)/nrow(database[['data']])
          rulesSummary$data[i-1,8] = 1- sum(rulesSummary$data[i-1,2:7])
          rulesSummary$rulesMatrix[1,i-1,] = rule1Array
          rule1Array[TRUE]=0
        }
        rulesSummary$rulesApplied[1] = 1
      }
      # Rule 2 is remove all observations with 0 or negative values
      if(2 %in% isolate(input$generalRules) && !(2 %in% rulesSummary$rulesApplied)){
        progress$inc(1, detail="Regla 2")
        cat("\n Applying rule 2 \n")
        #cat(summary(database))
        #Quitar todos los 0's o negativos
        rule2Array = c(rep(0,nrow(database[['data']])))
        for(i in 2:length(database[['data']])){
          rule2Array[database[['data']][,i] <= 0] = TRUE
          database[['data']][rule2Array == 1,i] = NA
          rulesSummary$data[i-1,3] = sum(rule2Array)/nrow(database[['data']])
          rulesSummary$data[i-1,8] = 1- sum(rulesSummary$data[i-1,2:7])
          rulesSummary$rulesMatrix[2,i-1,] = rule2Array
          
          rule2Array[TRUE] = 0
        }
        rulesSummary$rulesApplied[2] = 2
      }
      # Rule 3 is remove obervations below maquine deteccion limit
      if(3 %in% isolate(input$generalRules) && !(3 %in% rulesSummary$rulesApplied)){
        progress$inc(1, detail="Rule 3")
        cat("\n Applying rule 3 \n")
        # cat(summary(database))
        # Quitar todos los valores inferiores a 1
        rule3Array = c(rep(0,nrow(database[['data']])))
        for(i in 2:length(database[['data']])){
          rule3Array[database[['data']][,i] <= 1] = TRUE
          database[['data']][rule3Array == 1,i] = NA
          rulesSummary$data[i-1,4] = sum(rule3Array)/nrow(database[['data']])
          rulesSummary$data[i-1,8] = 1- sum(rulesSummary$data[i-1,2:7])
          rulesSummary$rulesMatrix[3,i-1,] = rule3Array
          rule3Array[TRUE] = 0
        }
        rulesSummary$rulesApplied[3] = 3
      }
      # Rule 4 is ensure PM10 > PM25 and viceversa
      if(4 %in% isolate(input$particularRules) && !(4 %in% rulesSummary$rulesApplied)){
        progress$inc(1, detail="Regla 4")
        cat("\n Applying regla 4 \n")
        rule4Array = c(rep(0,nrow(database[['data']])))
        for(i in colnames(database[['data']])[-1]){
          # ensure pm25 < pm10 (selected database[['datapm2.5']])
          if(input$dataBase == 1){
            for (j in colnames(database[['datapm10']])[-1]){
              if(i == j){
                rule4Array[database[['data']][i] - database[['datapm10']][j] >= 0] = TRUE
                database[['data']][rule4Array == TRUE,i] = NA
                rulesSummary$data[which(colnames(database[['data']]) == i)-1,5] = sum(rule4Array)/nrow(database[['data']])
                rulesSummary$data[which(colnames(database[['data']]) == i)-1,8] = 1- sum(rulesSummary$data[i,2:7])
                rulesSummary$rulesMatrix[4,which(colnames(database[['data']]) == i)-1,] = rule4Array
                rule4Array[TRUE] = 0
              }
            }
          }
          # ensure pm10 > pm2.5
          else if (input$dataBase == 2){
            for (j in colnames(database[['datapm2.5']])[-1]){
              if(i == j){
                rule4Array[database[['data']][i] - database[['datapm2.5']][j] <= 0] = TRUE
                database[['data']][rule4Array == TRUE,i] = NA
                rulesSummary$data[which(colnames(database[['data']]) == i)-1,5] = sum(rule4Array)/nrow(database[['data']])
                rulesSummary$data[which(colnames(database[['data']]) == i)-1,8] = 1- sum(rulesSummary$data[i,2:7])
                rulesSummary$rulesMatrix[4,which(colnames(database[['data']]) == i)-1,] = rule4Array
                rule4Array[TRUE] = 0
              }
            }
          }
        }
        rulesSummary$rulesApplied[4] = 4
      }
      # Update changes in database
      progress$inc(1, detail="Update Database")
      if(input$dataBase == 1){
        database[['datapm2.5']] = database[['data']]
      }
      else if (input$dataBase == 2){
        database[['datapm10']] = database[['data']] 
      }
    })
  })
  
  output$rulesTable <-renderTable({
    database[['data']]
    return(rulesSummary$data)},
    striped = TRUE)
  
  output$summary = renderPrint({
    summary(database[['data']][,-1])
  })
  
  output$plot <- renderPlotly({
    progress <- Progress$new(session)
    on.exit(progress$close())
    
    progress$set(message = "Generando grafico", value = 0)
    
    input$dateRange
    if(!is.null(input$dateRange)){
      date1 = as.POSIXlt(input$dateRange[1],format="%d/%m/%Y %H:%M")
      date2 = as.POSIXlt(input$dateRange[2],format="%d/%m/%Y %H:%M")
      timeInterval = seq.POSIXt(from=date1, to=date2, by="hour")
    } else
      timeInterval = c(1,2,3)
    
    #to many obs, need to subcript by each rule
    dataSubset = rulesSummary$rulesMatrix[,,which(database[['data']][,1] %in% timeInterval)]
    database[['data']]
    #Recalculate matrix of percentage by row (each rule)
    for(i in 1:6){
      rulesSummary$data[,i+1] = rowSums(rulesSummary$rulesMatrix[i,,which(database[['data']][,1] %in% timeInterval)])/length(which(database[['data']][,1] %in% timeInterval))
    }
    rulesSummary$data[,8] = 1 - rowSums(rulesSummary$data[,2:7])
    
    plotRules = plot_ly(x = rulesSummary$data[,1], y = rulesSummary$data[,8], name = "Porcentaje validos",type = "bar")
    rule1 <- add_trace(plotRules , x = rulesSummary$data[,1], y = rulesSummary$data[,2], name = "Caracteres", type = "bar")
    rule2 <- add_trace(rule1 , x = rulesSummary$data[,1], y = rulesSummary$data[,3], name = "Valores negativos", type = "bar")
    rule3 <- add_trace(rule2 , x = rulesSummary$data[,1], y = rulesSummary$data[,4], name = "Limite deteccion", type = "bar")
    rule4 <- add_trace(rule3 , x = rulesSummary$data[,1], y = rulesSummary$data[,5], name = "Valoracion cruzada", type = "bar")
    progress$inc(1)
    layout <- layout(rule4, barmode = "stack", title = paste("Porcentaje de datos en cada regla entre",input$dateRange[1],"y",input$dateRange[2]), 
                     xaxis = list(title = ""), 
                     yaxis = list(title = "Porcentaje de datos"))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      #paste("data",input$dataBase,"-",as.POSIXlt(input$dateRange[1],format="%d/%m/%Y %H:%M"),"-",
      #      as.POSIXlt(input$dateRange[2],format="%d/%m/%Y %H:%M"),".csv", sep="")
      dataName = switch(input$dataBase, "1"="pm25", "2"="pm10","Unknown")
      paste("cleanData",dataName,"-",gsub("-","/",input$dateRange[1]),"-",
            gsub("-","/",input$dateRange[2]),".csv", sep="")
    },
    content = function(file) {
      date1 = as.POSIXlt(input$dateRange[1],format="%d/%m/%Y %H:%M")
      date2 = as.POSIXlt(input$dateRange[2],format="%d/%m/%Y %H:%M")
      timeInterval = seq.POSIXt(from=date1, to=date2, by="hour")
      dataToDownload = database[['data']][database[['data']][,1] %in% timeInterval,]
      return(write.csv(dataToDownload, file))
    }
  )
}
