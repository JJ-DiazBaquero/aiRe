# https://www.rstudio.com/resources/cheatsheets/

dataLoadingUI <- function(id, label = "Data Loading") {
  ns <- NS(id)
  navbarPage(
    "Reporte de datos",
    tabPanel("Carga de datos",
             actionButton(ns('Delete'),"Borrar Actual"),
             radioButtons(ns("desiredFormat"), "Por favor elija el formato de sus datos",
                          choices = c("Documento por contaminante" = 1,"Documento por estacion" = 2 ),
                          selected = 1, inline = FALSE),
             textInput(ns('StationName'), "Nombre de la estacion"),
             fileInput(ns('file'), 'Por favor suba un archivo CSV',
                       accept=c('text/csv','.csv')),
             selectInput(ns('database'), "Seleccione contaminante", c( "PM2.5" =2,"PM10"=1)),
             actionButton(ns('add'),"Agregar")),
    tabPanel("Visualizacion de datos",
             selectInput(ns("dataBase"), label = h3("Seleccione una base datos"), 
                         choices = list("Ninguna" = 0, "PM2.5" = 1, "PM10" = 2), 
                         selected = 1),
             hr(),
             dataTableOutput(ns("summary")))
  )
}

dataLoading <- function(input, output, session) {
  
  newDatabase <- observe({
    input$Delete
    isolate({
      if(input$Delete>=1){
    database$datapm10 = data.frame(Fecha...Hora = character())
    database$datapm2.5 = data.frame(Fecha...Hora = character())
    database$data = NULL
      }
    })
  })
  
  database <- reactiveValues(datapm10 = read.csv("databases/PM10_1998_2016_Encsv.csv", sep=";", row.names=NULL, stringsAsFactors=TRUE),
                             datapm2.5 = read.csv("databases/PM2.5_1998_2016_Encsv.csv", sep=";", row.names=NULL, stringsAsFactors=TRUE),
                             data = NULL)
  database[['data']] <- database[['datapm2.5']]
  
  changeDatabase <- observe({
    input$add
    isolate({
      #Document per contaminant
      if(input$desiredFormat == 1){
        #TODO
      }
      if(input$desiredFormat == 2){
        file <- input$file
        if (is.null(file)){
          return(NULL)
        }
        data = read.csv(file$datapath,sep = ";", stringsAsFactors = F)
        if(input$database == 1){
          newData = as.list(database$datapm10)
          newData["Fecha...Hora"] = as.character(data[,1])
          newData[input$StationName] = data['PM10']
          database$datapm10 = as.data.frame(newData)
          database$datapm10[["Fecha...Hora"]] = as.POSIXct(as.character(database$datapm10["Fecha...Hora"]), format="%m/%d/%Y %H:%M")
          database$data = database$datapm10
        }
        if(input$database == 2){
          newData = as.list(database$datapm2.5)
          newData[["Fecha...Hora"]] = as.character(data[,1])
          newData[input$StationName] = data['PM2.5']
          database$datapm2.5 = as.data.frame(newData)
          database$datapm2.5["Fecha...Hora"] = as.POSIXct(database$datapm2.5[["Fecha...Hora"]], format="%d/%m/%Y %H:%M")
          database$data = database$datapm2.5
        }
      }
    })
    
  })
  
  changeDates <- observe({
    isolate({
      database$datapm2.5[,1] = as.POSIXct(as.character(database$data[,1]), format="%d/%m/%Y %H:%M")
      database$datapm10[,1] = as.POSIXct(as.character(database$datapm10[,1]), format="%m/%d/%Y %H:%M")
      database$data = database$datapm2.5[,1]
      database$currentData = 'pm2.5'
    })  
  })
  output$summary = renderDataTable({
    if(input$dataBase == 1){
      database$data = database$datapm2.5
      database$data
    }
    else if (input$dataBase == 2){
      database$data = database$datapm10
      database$data
    }
  })
  return(database)
}
