#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source("modules/functionDescription.R")
source("modules/dataLoading.R")
source("modules/dataCleaning.R")
source("modules/dataAnalysis.R")


# Define server logic required to draw a histogram

shinyServer(function(input, output, session) {
  
  cat("lines of code \n")
  cat(length(readLines("server.R"))+length(readLines("ui.R"))+length(readLines("modules/functionDescription.R"))+
        length(readLines("modules/dataLoading.R"))+length(readLines("modules/dataCleaning.R"))+length(readLines("modules/dataAnalysis.R")))
  
  load <- observe({
    isolate({
      progress <- Progress$new(session)
      on.exit(progress$close())
      progress$set(message = "Construyendo Aplicación",
                   detail = "Cargando descripción" ,
                   value = 0)
      descriptionController <-
        callModule(description, "functionDescription")
      progress$set(message = "Construyendo Aplicación",
                   detail = "Modulo de carga de datos",
                   value = 0.25)
      loadingController <-
        callModule(dataLoading, "dataLoading")
      progress$set(message = "Construyendo Aplicación",
                   detail = "Modulo de limpieza de datos",
                   value = 0.5)
      controllerCleaning <-
        callModule(dataCleaning, "dataCleaning", database = loadingController)
      progress$set(message = "Construyendo Aplicación",
                   detail = "Modulo de análisis de datos",
                   value = 0.75)
      controllerAnalysis <-
        callModule(dataAnalysis, "dataAnalysis", database = loadingController)
      progress$set(message = "Construyendo Aplicación",
                   detail = "Modulo de análisis de datos",
                   value = 1)
    })
  })
  
  output$actionDescription <- renderUI({
    cat(paste("\nEntro a la categoria ", "hover: ", input$plot_hover))
    if(!is.null(input$dataLoading_hover)){
      taglist(
        h3("Carga de datos"),
        textInput("label", "En esta accion puede subir la base de datos como un archivo csv, puede visualizarlos y configurarlos")
      )
    }
    else if (!is.null(input$dataAvailability_hover)){
      taglist(
        h3("Limpieza de datos"),
        textInput("label", "En esta sección puede definir reglas de validacion para limpiar los datos de la base de datos")
      )
    }
    else if(!is.null(input$dataAnalysis_hover)){
      taglist(
        h3("Analysis de datos"),
        textInput("label", "En esta sección puede definir reglas de validacion para limpiar los datos de la base de datos")
      )
    }
  })
})
