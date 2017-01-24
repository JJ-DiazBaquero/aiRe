comparativeAnalysisUI <- function(id) {
  ns <- NS(id)
  navbarPage(
    "Analisis comparativo",
    tabPanel("Parametros",
             h3("Rango de fechas 1"),
             sliderInput(ns("rangeYear"), "Anios:",
                         min = 1998, max = 2014, value = c(1998,2014)),
             sliderInput(ns("rangeMonth"), "Mes:",
                         min = 1, max = 12, value = c(1,12)),
             selectInput(ns("typeOfWeek"), label = "Numeracion de semanas:", 
                         choices = list("Dias de la semana" = 0, "Numero de la semana" = 1), 
                         selected = 0),
             uiOutput(ns("weekui"))
             ),
    tabPanel("Graficos")
  )
}

comparativeAnalysis <- function(input, output, session, database) {
  
  output$weekui <- renderUI({
    if(input$typeOfWeek == 0){
      checkboxGroupInput("rangeMonth", "Dias de la semana:",
                         choices = c("L" = "Monday",
                                     "M" = "Tuesday",
                                     "M" = "Wednesday",
                                     "J" = "Thursday",
                                     "V" = "Friday",
                                     "S" = "Saturday",
                                     "D" = "Sunday"),
                         inline = TRUE
      )
    }
    else if (input$typeOfWeek == 1){
      sliderInput("rangeMonth", "Dias del mes:",
                  min = 1, max = 31, value = c(1,31))
    }
    
  })

}