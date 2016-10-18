library(shiny)
library(plyr)
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
                         choices = list("Restriccion de cadenas de texto" = 1, "Valores negativos y ceros" = 2, "Limite de detección del equipo" = 3)
                         ),
      checkboxGroupInput(ns("particularRules"), label = h3("Reglas particulares a PM2.5"), 
                         choices = list("Asegurar PM10 > PM2.5" = 1)
      ),
      checkboxGroupInput(ns("densityRules"), label = h3("Reglas de densidad de datos"), 
                         choices = list("Eliminar datos aislados" = 1)
      )
      
    ),
    mainPanel(
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
  rulesSummary <- reactiveValues(data = rulesSummarydf)
  output$prueba <- renderText({
    return(1 %in% input$generalRules)
  })
  cleanData <- reactive({
    cat(file=stderr(),"cambia valor reglas generales")
    rules = input$generalRules
    if(1 %in% rules){
      #Encontrar todos los valores de string diferentes en la columna para asi quedar con solo números
      for(i in 2:12){
        lvlsStr = levels(database[,i])
        lvlsInt = as.numeric(lvlsStr)
        strList = lvlsStr[is.na(lvlsInt)]
        database[ database %in% strList] == 0
        database[,i] = as.numeric(database[,i])
        cat("cambiando columna ", i)
        
      }
    }
  })
  output$rulesTable <-renderTable(
    rulesSummary$data, striped = TRUE)
  output$summary = renderPrint({
    summary(database[,2:12])
  })
}