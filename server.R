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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  descriptionController <- callModule(description, "functionDescription")
  loadingController <- callModule(dataLoading, "dataLoading")
  database = loadingController
  controllerCleaning <- callModule(dataCleaning, "dataCleaning", database = database)
  
  yolo <- reactive({
    cat("El hover",input$plot_hover)
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
