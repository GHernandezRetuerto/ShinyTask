require(shiny)
#theming
require(bslib) 
require(thematic)
#data handling
require(tidyverse)
#plotting
require(ggplot2)
library(ggpubr)
require(plotly)
#shiny misc
require(DT)
require(shinyWidgets)

data =  read.csv(url('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data'), header=FALSE, stringsAsFactors = TRUE, sep=',')
colnames(data) = c('age','workclass','fnlwgt','education','education_num','marital_status','occupation','relationship','race','sex','cap_gain','cap_loss','hours_week','native_country','label')

# Some cleaning and selection of variables
data = data[complete.cases(data), ] %>% # Only data without NA
    select(age, workclass, education, race, sex, hours_week, label) %>%
    na.omit()

# Eliminating rows with ' ?' valuesstr
data = data[rowSums(data == " ?") == 0, , drop = FALSE]

# Checking the presence of NAs
NAcount = function(vector){
    return(sum(is.na(vector)))
}
if (sum(sapply(data, NAcount)) != 0){cat('ERROR: NAs detected.')}

# Just for quicker processing: removal of 70% of the data. I would not do this in a real application.
library(caret)
part = createDataPartition(data$label, p=0.3, list=FALSE)
data = data[part,]

#Modification of 'workclass'
levels(data$workclass) = c(levels(data$workclass), "Public", "Self Employed")
data$workclass = gsub('.*^\\ Self.*', 'Self Employed', data$workclass)
data$workclass = gsub('.*\\gov$.*', 'Public', data$workclass)
data$workclass = gsub('.*\\pay$.*', 'Without Pay', data$workclass)
data$workclass = factor(data$workclass, levels = c(' Private', 'Public', 'Self Employed', 'Without Pay'))

#Modification of 'education'
levels(data$education) = c(levels(data$education), "Elementary school", "Middle School", "High School",'Unfinnished College')
data$education = gsub(' 1st-4th', 'Elementary School', data$education)
data$education = gsub(' 5th-6th', 'Middle School', data$education)
data$education = gsub(' 7th-8th', 'Middle School', data$education)
data$education = gsub(' 9th', 'High School', data$education)
data$education = gsub(' 10th', 'High School', data$education)
data$education = gsub(' 11th', 'High School', data$education)
data$education = gsub(' 12th', 'High School', data$education)
data$education = gsub(' Some-college', 'Unfinnished College', data$education)
data$education = gsub(' Assoc-voc', 'Assoc', data$education)
data$education = gsub(' Assoc-acdm', 'Assoc', data$education)
data$education = factor(data$education)
data$education = ordered(data$education, levels = c(' Preschool','Elementary School','Middle School','High School',' HS-grad',' Prof-school','Assoc','Unfinnished College',' Bachelors',' Masters',' Doctorate'))  

# Modification of 'race'
data$race = gsub(' Amer-Indian-Eskimo', 'Ind-Eskimo', data$race)
data$race = gsub(' Asian-Pac-Islander', 'Asian-Pac', data$race)

label_choice = unique(data$label)


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
                 awesomeCheckboxGroup(
                     inputId = 'label_sel',
                     label = 'Select the populations to show',
                     choices = label_choice,
                     selected = label_choice,
                     inline = FALSE,
                     status = "primary",
                     width = NULL
                 ),
                 tabsetPanel(
                     tabPanel('Age',
                         sidebarLayout(
                             sidebarPanel(
                                 p(HTML(paste0(tags$code('Age'), 'is a continuous (or almost) variable.
                                               Therefore, it is better to display it through a kernel density distribution.')))
                             ),#sidebarPanel
                             mainPanel(
                                 br(),
                                 plotOutput('AgePlot')
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
                                  br(),
                                  plotOutput('WorkPlot')
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
                                  br(),
                                  plotOutput('EdPlot')
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
                                  br(),
                                  plotOutput('RacePlot')
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
                                  br(),
                                  plotOutput('SexPlot')
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
                                  br(),
                                  plotOutput('HoursPlot')
                              )#mainPanel
                        )#sidebarLayout
                    ) #HoursWeek
                 ) #tabsetPanel
             )#fluidPage    
    )#tabPanel
)#navbarPage

# Define server logic required to draw a histogram
server = function(input, output) {
    # Selector of populations in Variable Plots
    labelSelection = reactive({data %>%
            filter(label %in% input$label_sel)})
    
    # Variable Plot Renderers
    output$AgePlot = renderPlot(
        labelSelection() %>%
            ggplot(aes(age, fill = label)) +
            geom_density(alpha = 0.5, kernel = "gaussian") +
            geom_vline(aes(xintercept = mean(age)), linetype = 2)+
            scale_fill_discrete(name = "Label")+
            scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
            theme_minimal()+
            labs(title = "Age density plot by label", x = 'Age of Individual', y = 'Density')
    ) #AgePlot
    
    output$HoursPlot = renderPlot(
        labelSelection() %>%
            ggplot(aes(hours_week, fill = label)) +
            geom_density(alpha = 0.5, kernel = "gaussian") +
            geom_vline(aes(xintercept = mean(age)), linetype = 2)+
            scale_fill_discrete(name = "Label")+
            theme_minimal()+
            labs(title = "Hour per Week density plot by label", x = 'Hours/Week Worked', y = 'Density')
    ) #HoursPlot
    
    output$EdPlot = renderPlot(
        labelSelection() %>%
            ggplot(aes(x=education, fill=label)) +
            geom_bar(stat="count", width=0.7) +
            theme_minimal()+
            theme(axis.text.x = element_text(angle = 45, vjust=1, hjust = 1)) +
            values = c("#00AFBB", "#E7B800", "#FC4E07") +
            scale_fill_discrete(name = "Label")+
            labs(title = "Education barplot by label" , x = 'Hours/Week Worked', y = 'Density')
    ) #EdPlot
    
    output$WorkPlot = renderPlot(
        labelSelection() %>%
            ggplot(aes(x=workclass, fill=label)) +
            geom_bar(stat="count", width=0.7) +
            theme(axis.text.x = element_text(angle = 45, vjust=1, hjust = 1)) +
            scale_fill_discrete(name = "Label")+
            theme_minimal()+
            labs(title = "Workclass barplot by label" , x = 'Hours/Week Worked', y = 'Density')
    ) #WorkPlot
    
    output$SexPlot = renderPlot(
        labelSelection() %>%
            ggplot(aes(x=sex, fill=label)) +
            geom_bar(stat="count", width=0.7) +
            theme(axis.text.x = element_text(angle = 45, vjust=1, hjust = 1)) +
            scale_fill_discrete(name = "Label") +
            theme_minimal() +
            labs(title = "Sex barplot by label" , x = 'Hours/Week Worked', y = 'Density')
    ) #SexPlot
    
    output$RacePlot = renderPlot(
        labelSelection() %>%
            ggplot(aes(x=race, fill=label)) +
            geom_bar(stat="count", width=0.7) +
            theme(axis.text.x = element_text(angle = 45, vjust=1, hjust = 1)) +
            scale_fill_discrete(name = "Label")+
            theme_minimal()+
            labs(title = "Race barplot by label" , x = 'Hours/Week Worked', y = 'Density')
    ) #RacePlot
}

# Run the application 
shinyApp(ui = ui, server = server)
