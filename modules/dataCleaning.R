library(shiny)
library(plotly)
library("openair")
dataCleaningUI <- function(id){
  ns <- NS(id) 
  titlePanel("Limpieza de datos")
  sidebarLayout(
    sidebarPanel(
      selectInput(ns("dataBase"), label = h3("Seleccione una base datos"), 
                  choices = list("PM2.5" = 1, "PM10" = 2), 
                  selected = 1),
      hr(),
      checkboxGroupInput(ns("generalRules"), label = h3("Reglas generales"), 
                         choices = list("Restriccion de cadenas de texto" = 1, 
                                        "Valores negativos y ceros" = 2, 
                                        "Limite de detección del equipo" = 3)
                         ),
      checkboxGroupInput(ns("particularRules"), label = h3("Reglas particulares a PM2.5"), 
                         choices = list("Asegurar PM10 > PM2.5" = 1)
      ),
      checkboxGroupInput(ns("densityRules"), label = h3("Reglas de densidad de datos"), 
                         choices = list("Eliminar datos aislados" = 1)
      ),
      actionButton(ns("applyRulesBtn"), "Aplicar reglas"),
      dateRangeInput(ns("dateRange"), 
                     "Rango de fechas a visualizar", 
                     language = "es", separator = "a", format = "dd-mm-yyyy",
                     start = "1998-01-01", end = "2014-12-31", 
                     min = "1998-01-01", max = "2014-12-31")
    ),
    mainPanel(
      p("El siguiente grafico indica el porcentaje de los datos que pertenecen a la categoria en la derecha"),
      plotlyOutput(ns("plot")),
      tableOutput(ns("rulesTable")),
      verbatimTextOutput(ns("summary")),
      textOutput(ns("prueba"))
    )
  )
}

dataCleaning <- function(input, output, session, database){
  
  rulesSummarydf = isolate(data.frame(Estaciones = colnames(database[['data']])[2:12]))
  for(i in 1:6){
    rulesSummarydf[paste("Regla ",i)] = rep(0,11)
  }
  rulesSummarydf["Datos validos"] = rep(1,11)
  #data is the dataframe with the summary of the data per rule in each station 
  #rulesMatrix is the 3 dimensional matrix of 1's and 0's if the data is valid, 1st: Rule, 2nd: Station, 3rd: obs
  rulesSummary <- reactiveValues(data = rulesSummarydf, 
                                 rulesMatrix = array(0,dim = isolate(c(6,length(database[['data']])-1,nrow(database[['data']])))))
  output$prueba <- renderText({
    return(input$dateRange)
  })
  
  rulesData <- observe({
    input$applyRulesBtn
    isolate({
    progress <- Progress$new(session, min = 0, max = length(input$generalRules))
    on.exit(progress$close())
    progress$set(message = "Aplicando reglas", value = 0)
    if(1 %in% isolate(input$generalRules)){
      progress$set(0, detail="Regla 1")
      #Encontrar todos los valores de string diferentes en la columna para asi quedar con solo números
      cat("Aplicando regla 1 \n")
      rule1Array = c(rep(0,nrow(database[['data']])))
      for(i in 2:length(database[['data']])){
        lvlsStr = levels(database[['data']][,i])
        lvlsInt = as.double(gsub(",",".",lvlsStr))
        strList = lvlsStr[is.na(lvlsInt)]
        rule1Array[database[['data']][,i] %in% strList] = TRUE
        database[['data']][rule1Array == 1,i] = NA
        database[['data']][,i] = as.numeric(gsub(",",".",database[['data']][,i]))
        rulesSummary$data[i-1,2] = sum(rule1Array)/nrow(database[['data']])
        rulesSummary$data[i-1,8] = 1- sum(rulesSummary$data[i-1,2:7])
        rulesSummary$rulesMatrix[1,i-1,] = rule1Array
        rule1Array[TRUE]=0
        
      }
    } 
    if(2 %in% isolate(input$generalRules)){
      progress$inc(1, detail="Regla 2")
      cat("\n Aplicando regla 2 \n")
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
    }
    if(3 %in% isolate(input$generalRules)){
      progress$inc(1, detail="Regla 3")
      cat("\n Aplicando regla 3 \n")
      #cat(summary(database))
      #Quitar todos los valores inferiores a 1
      rule3Array = c(rep(0,nrow(database[['data']])))
      for(i in 2:length(database[['data']])){
        rule3Array[database[['data']][,i] <= 1] = TRUE
        database[['data']][rule3Array == 1,i] = NA
        rulesSummary$data[i-1,4] = sum(rule3Array)/nrow(database[['data']])
        rulesSummary$data[i-1,8] = 1- sum(rulesSummary$data[i-1,2:7])
        rulesSummary$rulesMatrix[3,i-1,] = rule3Array
        rule3Array[TRUE] = 0
      }
    }
    progress$inc(1, detail="Regla 3")
    })
  })
  
  output$rulesTable <-renderTable({
    database[['data']]
    return(rulesSummary$data)},
    striped = TRUE)
  
  output$summary = renderPrint({
    summary(database[['data']][,2:12])
  })
  
  output$plot <- renderPlotly({
    progress <- Progress$new(session)
    on.exit(progress$close())

    progress$set(message = "Generando grafico", value = 0)
    
    input$dateRange
    date1 = as.POSIXlt(input$dateRange[1],format="%d/%m/%Y %H:%M")
    date2 = as.POSIXlt(input$dateRange[2],format="%d/%m/%Y %H:%M")
    timeInterval = seq.POSIXt(from=date1, to=date2, by="hour")
    
    #to many obs, need to subcript by each rule
    dataSubset = rulesSummary$rulesMatrix[,,which(database[['data']][,1] %in% timeInterval)]
    cat("Length of timeInterval")
    cat(length(which(database[['data']][,1] %in% timeInterval)))
    database[['data']]
    #Recalculate matrix of percentage by row (each rule)
    for(i in 1:6){
      rulesSummary$data[,i+1] = rowSums(rulesSummary$rulesMatrix[i,,which(database[['data']][,1] %in% timeInterval)])/length(which(database[['data']][,1] %in% timeInterval))
    }
    rulesSummary$data[,8] = 1 - rowSums(rulesSummary$data[,2:7])
    
    plotRules = plot_ly(x = rulesSummary$data[,1], y = rulesSummary$data[,8], name = "Porcentaje validos",type = "bar")
    rule1 <- add_trace(plotRules , x = rulesSummary$data[,1], y = rulesSummary$data[,2], name = "Regla 1", type = "bar")
    rule2 <- add_trace(rule1 , x = rulesSummary$data[,1], y = rulesSummary$data[,3], name = "Regla 2", type = "bar")
    rule3 <- add_trace(rule2 , x = rulesSummary$data[,1], y = rulesSummary$data[,4], name = "Regla 3", type = "bar")
    rule4 <- add_trace(rule3 , x = rulesSummary$data[,1], y = rulesSummary$data[,5], name = "Regla 4", type = "bar")
    progress$inc(1)
    layout <- layout(rule4, barmode = "stack", title = "Porcentaje de datos en cada regla", 
                     xaxis = list(title = ""), 
                     yaxis = list(title = "Porcentaje de datos"))
  })
}