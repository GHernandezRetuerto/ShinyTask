require(shiny)
require(tidyverse)
require(bslib)
require(ggplot2)
require(plotly)
require(thematic)
require(DT)
require(shinyWidgets)

data =  read.csv(url('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data'), header=FALSE, stringsAsFactors = TRUE)
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
                 img(src="uc3mLogo.jpg", align='right'),
                 h1('Shiny App'),
                 h4('By Guillermo Hernandez - Data Tidying 2021'),
                 br(),
                 br(),
                 br(),
                 p('This is a Shiny App made for Data Tyding, a subject of the Msc in Statistics for Data Science.\n'),
                 br(),
                 p('All the information related with the functioning of the app can be found in the next tab.')
             )#fluidPage
    ),#tabPanel
    
    tabPanel('Guide',
        fluidPage(
            includeMarkdown("guide.md")
        ) #fluidPage
    ),#tabPanel
    
    tabPanel("The Variables",
             fluidPage(
                 p('Check yourself the data in the nest interactive charts.'),
                 prettySwitch(inputId = 'label0', label='  Show <50K Observations'),
                 prettySwitch(inputId = 'label1', label='  Show >50K Observations'),
                 tabsetPanel(
                     tabPanel('Age',
                         sidebarLayout(
                             sidebarPanel(
                                 p(HTML(paste0(tags$code('Age'), 'is a continuous (or almost) variable.
                                               Therefore, it is better to display it through a kernel density distribution.')))
                             ),#sidebarPanel
                             mainPanel(
                             )#mainPanel
                         )#sidebarLayout
                     ), #Age
                     tabPanel('Workclass',
                          sidebarLayout(
                              sidebarPanel(
                                  p(HTML(paste0(tags$code('Age'), 'is a continuous (or almost) variable.
                                               Therefore, it is better to display it through a kernel density distribution.')))
                              ),#sidebarPanel
                              mainPanel(
                              )#mainPanel
                         )#sidebarLayout
                     ), #Workclass
                     tabPanel('Education',
                          sidebarLayout(
                              sidebarPanel(
                                  p(HTML(paste0(tags$code('Age'), 'is a continuous (or almost) variable.
                                               Therefore, it is better to display it through a kernel density distribution.')))
                              ),#sidebarPanel
                              mainPanel(
                              )#mainPanel
                         )#sidebarLayout
                     ), #Workclass
                     tabPanel('Race',
                          sidebarLayout(
                              sidebarPanel(
                                  p(HTML(paste0(tags$code('Age'), 'is a continuous (or almost) variable.
                                               Therefore, it is better to display it through a kernel density distribution.')))
                              ),#sidebarPanel
                              mainPanel(
                              )#mainPanel
                         )#sidebarLayout
                     ), #Race
                     tabPanel('Sex',
                          sidebarLayout(
                              sidebarPanel(
                                  p(HTML(paste0(tags$code('Age'), 'is a continuous (or almost) variable.
                                               Therefore, it is better to display it through a kernel density distribution.')))
                              ),#sidebarPanel
                              mainPanel(
                              )#mainPanel
                         )#sidebarLayout
                     ), #Race
                     tabPanel('Hours per Week',
                          sidebarLayout(
                              sidebarPanel(
                                  p(HTML(paste0(tags$code('Age'), 'is a continuous (or almost) variable.
                                               Therefore, it is better to display it through a kernel density distribution.')))
                              ),#sidebarPanel
                              mainPanel(
                              )#mainPanel
                        )#sidebarLayout
                    ) #HoursWeek
                 ) #tabsetPanel
             )#fluidPage    
    )#tabPanel
)#navbarPage

# Define server logic required to draw a histogram
server = function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
