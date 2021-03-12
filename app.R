require(shiny)
require(tidyverse)
require(bslib)
require(ggplot2)
require(plotly)
require(thematic)

data =  read.csv(url('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data'), header=FALSE)
colnames(data) = c('age','workclass','fnlwgt','education','education_num','marital_status','occupation','relationship','race','sex','cap_gain','cap_loss','hours_week','native_country','label')

# Some cleaning and selection of variables
data = data[complete.cases(data), ] %>% # Only data without NA
    select(age, workclass, education, race, sex, hours_week, label)

# Checking the presence of NAs
NAcount = function(vector){
    return(sum(is.na(vector)))
}
if (sum(sapply(data, NAcount)) != 0){cat('ERROR: NAs detected.')}

# Just for quicker processing: removal of 70% of the data. I would not do this in a real application.
library(caret)
part = createDataPartition(data$label, p=0.3, list=FALSE)
data = data[part,]
nrow(data)


# Define UI for application that draws a histogram
# Define UI
ui = navbarPage(
    theme = bs_theme(version = 4, bootswatch = "yeti"),#theme
    
    # Application title
    titlePanel("Shiny App"
    ),
    
    # First Page 
    tabPanel('Introduction',
             fluidPage(
                 h1('Shiny App'),
                 h2('By Guillermo Hernandez - Data Tidying 2021'),
                 br(),
                 p(''),
                 br(),
                 p(''),
                 a(href="https://archive.ics.uci.edu/ml/datasets/Adult", "UCI Repository - Adult Income Dataset"),
                 br(),
                 br(),
                 h2(''),
                 br(),
                 p(''),
                 br(),
                 img(src="uc3mLogo.jpg", align='right')
             )#fluidPage
             
             
    ),#tabPanel
    
    tabPanel("The Variables",
             fluidPage(
                 titlePanel(),
                 tabsetPanel(
                     tabPanel(
                         sidebarLayout(
                             sidebarPanel(
                                 p('')
                             ),#sidebarPanel
                             mainPanel(
                             )#mainPanel
                         )#sidebarLayout
                     ) #tabPanel
                 ) #titlePanel
             )#fluidPage    
    )#tabPanel
)#navbarPage

# Define server logic required to draw a histogram
server = function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
