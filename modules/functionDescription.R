library(shiny)
descriptionUI <- function(id){
  ns <- NS(id)
  
  h3("Descripción de las funciones")
  
  tabsetPanel(
    tabPanel("Sobre esta herramienta",
             fluidRow(
               column(4,
                      h4("Descripcion:"),
                      p("Esta herramienta de software permite realizar analisis sobre datos de calidad de aire, da facilidades de limpieza de datos, analisis de disponibilidad de datos, analisis comparativos de tiempo y analisis de indicadores de calidad de aire"),
                      hr(),
                      tags$i("Version: 1.1")
                      
               ),
               column(4,
                      h4("Alianza:"),
                      p("Esta es una herramienta de libre acceso desarrollada por el departamento de ingenieria industrial de Universidad de los Andes (Bogota D.C.)"),
                      p("Esta herramienta es fruto de una alianza entre la Universidad de los Andes y el Ministerio de Ambiente y Desarrollo Sostenible")
               ),
               column(4,
                      h4("Contacto:"),
                      a(href = "www.uniandes.edu.co","Universidad de los Andes" ),
                      br(),
                      a(href ="www.minambiente.gov.co", "MinAmbiente:"),
                      p("Correos de contacto:"),
                      tags$ul(
                        tags$li(a(href = "mailto:jj.diaz1067@uniandes.edu.co", "Juan Jose Diaz Baquero: jj.diaz1067@uniandes.edu.co")),
                        tags$li(a(href = "n.rojas13@uniandes.edu.co", "Nicolas Rojas: n.rojas13@uniandes.edu.co")),
                        tags$li(a(href = "jffranco@uniandes.edu.co", "Juan Felipe Franco: jffranco@uniandes.edu.co")),                        
                        tags$li(a(href = "i.mura@uniandes.edu.co", "Ivan Mura: i.mura@uniandes.edu.co"))
                      )
               )
             )),
    tabPanel("Carga de datos", uiOutput(ns("data_loading"))),
    tabPanel("Limpieza de datos", uiOutput(ns("data_cleaning"))),
    tabPanel("Analisis de datos", uiOutput(ns("data_analysis"))),
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
