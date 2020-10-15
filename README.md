# aiRe
A Shiny R Application for analysis of urban Air Quality Data

## Getting started

### 1. Install R and RStudio

In order to run aiRe you would need both R as language compiler (https://www.r-project.org/)
 and RStudio (https://rstudio.com/) to create the server.

### 2. Clone or download aiRe's source code

### 3. Open aiRe's R project in RStudio

The easiest is to click on `AirQualityTool.Rproj` inside the root folder

### 4. Install packages

After the installation it is necessary to install all packages required to run aiRe, for this run in R console
```
install.packages("shiny")
install.packages("openair")
install.packages("RColorBrewer")
install.packages("plotly")
install.packages("htmltools")
install.packages("bsplus")
install.packages("ggplot2")
install.packages("scales")
install.packages("grid")
install.packages("shinythemes")
install.packages("readr")
```

### 5. Run the App

Open either `server.R` or `ui.R` and click on `Run App`. Or run in R's console
```
shiny::runApp()
```
By default RStudio will open a built-in browser, it is recommended to open aiRe in a wide supported browser such as Chrome or Firefox

## File structure

All the main R code is found inside the `modules` folder

All air quality data is found inside the `databases` folder,
this app comes already with some data from Bogota's air quality, you can use this
data to explore aiRe's features and to guide yourself on what kind
of data format does aiRe need in order to work

In the `www` are for external resources of a web application
