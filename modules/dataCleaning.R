library(shiny)
library(plotly)
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
      actionButton(ns("applyRulesBtn"), "Aplicar reglas")
    ),
    mainPanel(
      plotlyOutput(ns("plot")),
      tableOutput(ns("rulesTable")),
      verbatimTextOutput(ns("summary")),
      textOutput(ns("prueba"))
    )
  )
}

dataCleaning <- function(input, output, session, database){
  rulesSummarydf = data.frame(Estaciones = colnames(database)[2:12])
  for(i in 1:6){
    rulesSummarydf[paste("Regla ",i)] = rep(0,11)
  }
  rulesSummarydf["Datos validos"] = rep(1,11)
  rulesSummary <- reactiveValues(data = rulesSummarydf, 
                                 rulesMatrix = matrix(0,nrow = nrow(database), 
                                                ncol=length(database)-1))
  output$prueba <- renderText({
    return(1 %in% input$generalRules)
  })
  cleanData <- reactive({
    input$applyRulesBtn
    if(1 %in% input$generalRules){
      #Encontrar todos los valores de string diferentes en la columna para asi quedar con solo números
      for(i in 2:length(database)){
        lvlsStr = levels(database[,i])
        lvlsInt = as.numeric(lvlsStr)
        strList = lvlsStr[is.na(lvlsInt)]
        rulesSummary$rulesMatrix[database[,i] %in% strList] == 1
        database[database[,i] %in% strList] == 0
        database[,i] = as.numeric(database[,i])
        cat("cambiando columna ", i)
      }
      rulesSummary$data[1,] = colSums(rulesSummary$rulesMatrix)
      rulesSummary$data[,8] = rulesSummary$data[,8]- rulesSummary$data[,2]
    }
  })
  output$rulesTable <-renderTable({
    
    input$applyRulesBtn
    if(1 %in% input$generalRules){
      #Encontrar todos los valores de string diferentes en la columna para asi quedar con solo números
      rule1Array = c(rep(0,nrow(database)))
      for(i in 2:length(database)){
        lvlsStr = levels(database[,i])
        lvlsInt = as.numeric(lvlsStr)
        strList = lvlsStr[is.na(lvlsInt)]
        rule1Array[database[,i] %in% strList] = 1
        database[,i] = as.numeric(database[,i])
        database[database[,i] %in% strList,] = 0
        cat("cambiando columna ", i, "\n")
        cat("Numero de entradas con string ", sum(rule1Array), "\n")
        rulesSummary$data[i-1,2] = sum(rule1Array)/nrow(database)
        rulesSummary$rulesMatrix[,i-1] = rule1Array
        rule1Array[TRUE]=0
      }
      cat("Suma de columnas: \n")
      cat(colSums(rulesSummary$rulesMatrix))
    }
    
    return(rulesSummary$data)}, striped = TRUE)
  output$summary = renderPrint({
    summary(database[,2:12])
  })
  
  output$plot <- renderPlotly({
    plotRules = plot_ly(x = rulesSummary$data[,1], y = rulesSummary$data[,8], name = "Total validos",type = "bar")
    Positive <- add_trace(plotRules , x = rulesSummary$data[,1], y = rulesSummary$data[,2], name = "Rule 1", type = "bar")
    layout <- layout(Positive, barmode = "stack")
  })
}