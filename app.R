require(shiny)
require(tidyverse)
require(bslib)
require(ggplot2)
require(plotly)
require(thematic)

data =  read.csv(url('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data'))
head(data)

# Define UI for application that draws a histogram
ui = fluidPage(

)

# Define server logic required to draw a histogram
server = function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
