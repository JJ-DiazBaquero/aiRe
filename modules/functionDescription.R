library(shiny)
descriptionUI <- function(id){
  ns <- NS(id)
  
  h3("Descripción de las funciones")
  
  tabsetPanel(
    tabPanel("Carga de datos", uiOutput(ns("data_loading"))),
    tabPanel("Limpieza de datos", uiOutput(ns("data_cleaning"))),
    tabPanel("Análisis de datos", uiOutput(ns("data_analysis"))),
    tabPanel("Reportes", uiOutput(ns("reports"))),
    tabPanel("Cuadro de control", uiOutput(ns("dashboard")))
    
  )
}
description <- function(input, output, session){
  output$data_loading <- renderUI({
    tagList(
      p("Aqui va la descripcion de la funcion de carga de datos")
    )
  })
  output$data_cleaning <- renderUI({
    tagList(
      p("Aqui va la descripcion de la funcion de limpieza de datos")
    )
  })
  output$data_analysis <- renderUI({
    tagList(
      p("Aqui va la descripcion de la funcion de analisis de datos")
    )
  })
  output$reports <- renderUI({
      p("Aqui va la descripcion de la funcion de reportes")
  })
  output$dashboard <- renderUI({
    tagList(
      p("Aqui va la descripcion de la funcion de visualizar el dashboard")
    )
  })
}
