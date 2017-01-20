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
                         selected = 0)
             ),
    tabPanel("Graficos")
  )
}

comparativeAnalysis <- function(input, output, session, database) {

}