comparativeAnalysisUI <- function(id) {
  ns <- NS(id)
  navbarPage(
    "Analisis comparativo",
    tabPanel("Parametros",
             fluidRow(
               column(4,
             h3("Rango de fechas 1"),
             sliderInput(ns("rangeYear1"), "Anios:",
                         min = 1998, max = 2014, value = c(1998,2014)),
             sliderInput(ns("rangeMonth1"), "Mes:",
                         min = 1, max = 12, value = c(1,12)),
             selectInput(ns("typeOfWeek1"), label = "Numeracion de semanas:", 
                         choices = list("Dias de la semana" = 0, "Numero de la semana" = 1), 
                         selected = 0),
             uiOutput(ns("weekui1")),
             sliderInput(ns("rangeHour1"), "Hora:",
                         min = 0, max = 23, value = c(0,23)),
             checkboxInput(ns("use1"),"Usar rango 1", FALSE)
              ),
             column(4,
                    h3("Rango de fechas 2"),
                    sliderInput(ns("rangeYear2"), "Anios:",
                                min = 1998, max = 2014, value = c(1998,2014)),
                    sliderInput(ns("rangeMonth2"), "Mes:",
                                min = 1, max = 12, value = c(1,12)),
                    selectInput(ns("typeOfWeek2"), label = "Numeracion de semanas:", 
                                choices = list("Dias de la semana" = 0, "Numero de la semana" = 1), 
                                selected = 0),
                    uiOutput(ns("weekui2")),
                    sliderInput(ns("rangeHour2"), "Hora:",
                                min = 0, max = 23, value = c(0,23)),
                    checkboxInput(ns("use2"),"Usar rango 2", FALSE)
             ),
             column(4,
                    h3("Rango de fechas 3"),
                    sliderInput(ns("rangeYear3"), "Anios:",
                                min = 1998, max = 2014, value = c(1998,2014)),
                    sliderInput(ns("rangeMonth3"), "Mes:",
                                min = 1, max = 12, value = c(1,12)),
                    selectInput(ns("typeOfWeek3"), label = "Numeracion de semanas:", 
                                choices = list("Dias de la semana" = 0, "Numero de la semana" = 1), 
                                selected = 0),
                    uiOutput(ns("weekui3")),
                    sliderInput(ns("rangeHour3"), "Hora:",
                                min = 0, max = 23, value = c(0,23)),
                    checkboxInput(ns("use3"),"Usar rango 3", FALSE)
             )),
             fluidRow(actionButton(ns("makeGraphs"),"Enviar cambios"))
             ),
    tabPanel("Graficos",plotlyOutput(ns("boxplot")))
  )
}

comparativeAnalysis <- function(input, output, session, database) {
  ns <- session$ns
  #intervalData is the informacion of the data contain in each range of dates
  reactiveData <- reactiveValues(intervalData = list())
  
  output$weekui1 <- renderUI({
    if(input$typeOfWeek1 == 0){
      checkboxGroupInput(ns("rangeDay1"), "Dias de la semana:",
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
    else if (input$typeOfWeek1 == 1){
      sliderInput(ns("rangeDay1"), "Dias del mes:",
                  min = 1, max = 31, value = c(1,31))
    }
  })
  
  output$weekui2 <- renderUI({
    if(input$typeOfWeek2 == 0){
      checkboxGroupInput(ns("rangeDay2"), "Dias de la semana:",
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
    else if (input$typeOfWeek2 == 1){
      sliderInput(ns("rangeDay2"), "Dias del mes:",
                  min = 1, max = 31, value = c(1,31))
    }
  })
  
  output$weekui3 <- renderUI({
    if(input$typeOfWeek3 == 0){
      checkboxGroupInput(ns("rangeDay3"), "Dias de la semana:",
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
    else if (input$typeOfWeek3 == 1){
      sliderInput(ns("rangeDay3"), "Dias del mes:",
                  min = 1, max = 31, value = c(1,31))
    }
  })
  
  calcAvailavility<- observe({
    input$makeGraphs
    isolate({
      data = list()
      if(input$use1 == TRUE){
        if(input$typeOfWeek1 == 1){
          date1 = as.POSIXlt(paste(input$rangeDay1[1],"/",input$rangeMonth1[1],"/",input$rangeYear1[1]," ",input$rangeHour1[1],":00", sep = ""),
                             format="%d/%m/%Y %H:%M")
          date2 = as.POSIXlt(paste(input$rangeDay1[2],"/",input$rangeMonth1[2],"/",input$rangeYear1[2]," ",input$rangeHour1[2],":00", sep = ""),
                             format="%d/%m/%Y %H:%M")
          timeInterval = seq.POSIXt(from=date1, to=date2, by="hour")
          reactiveData$intervalData[1] = database[['data']][which(database[['data']][,1] %in% timeInterval),]
        }
      }
      
      output$boxplot <- renderPlotly({
        p = NULL
        browser()
        for(i in reactiveData$intervalData){
          if(is.null(p)){
            p = plot_ly(y = i[,2], type = "box")
          }
          else{
            p <- add_trace(p, y = i[,2], type = "box")
          }
        }
      })
      
    })
  })
  
}