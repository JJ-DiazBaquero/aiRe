#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Proyecto de calidad de aire"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width = "10%",
      h3("Seleccione una opción"),
       actionButton("dataLoading", "Carga de datos", width ="100%"),
       actionButton("dataAvailability", "Analizar disponibilidad de datos",width="100%")
    ),
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
  