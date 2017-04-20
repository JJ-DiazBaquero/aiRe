timeSeriesUI <- function(id){
  ns <- NS(id)
  titlePanel("Series de tiempo")
  fluidPage(
    radioButtons(ns("AgregationLevel"), "Selecci&oacute;n de agrupaci&oacute; n", 
                 choices = list(hora = 1, dia = 2, semana = 3, mes = 4, anio = 5),
                 selected = 2),
    plotlyOutput(ns("timeSeries"))
  )
}
timeSeries <- function(input, output, session, database){
  output$timeSeries <- renderPlotly({
    
  })
}
