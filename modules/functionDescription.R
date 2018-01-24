library(shiny)
descriptionUI <- function(id){
  ns <- NS(id)
  
  navlistPanel(widths = c(3,9),
    tabPanel("Sobre esta herramienta",
             fluidRow(
               column(4,
                      h4("Descripción:"),
                      p("Esta herramienta de software permite realizar análisis sobre datos de calidad de aire, da facilidades de limpieza de datos, análisis de disponibilidad de datos, análisis comparativos de tiempo y análisis de indicadores de calidad de aire"),
                      hr(),
                      tags$i("Versión: 1.1")
                      
               ),
               column(4,
                      h4("Alianza:"),
                      p("Esta es una herramienta de libre acceso desarrollada por el departamento de ingeniería industrial de Universidad de los Andes (Bogotá D.C.)"),
                      p("Esta herramienta es fruto de una alianza entre la Universidad de los Andes y el Ministerio de Ambiente y Desarrollo Sostenible")
               ),
               column(4,
                      h4("Contacto:"),
                      a(href = "https:\\www.uniandes.edu.co","Universidad de los Andes" ),
                      br(),
                      a(href ="https:\\www.minambiente.gov.co", "MinAmbiente:"),
                      p("Correos de contacto:"),
                      tags$ul(
                        tags$li(a(href = "mailto:jj.diaz1067@uniandes.edu.co", "Juan Jose Díaz Baquero: jj.diaz1067@uniandes.edu.co")),
                        tags$li(a(href = "mailto:n.rojas13@uniandes.edu.co", "Nicolas Rojas: n.rojas13@uniandes.edu.co")),
                        tags$li(a(href = "mailto:jffranco@uniandes.edu.co", "Juan Felipe Franco: jffranco@uniandes.edu.co")),                        
                        tags$li(a(href = "mailto:i.mura@uniandes.edu.co", "Ivan Mura: i.mura@uniandes.edu.co"))
                      )
               )
             )),
    tabPanel("Carga de datos", uiOutput(ns("data_loading"))),
    tabPanel("Limpieza de datos", uiOutput(ns("data_cleaning"))),
    tabPanel("Análisis de datos", uiOutput(ns("data_analysis"))),
    tabPanel("Reportes", uiOutput(ns("reports")))
    
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
}
