require(shiny)
#theming
require(bslib) 
require(thematic)
#data handling
require(tidyverse)
require(readr)
#plotting
require(ggplot2)
library(ggpubr)
require(plotly)
#shiny misc
require(DT)
require(shinyWidgets)

data =  read.csv(url('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data'), header=FALSE, sep=',')
colnames(data) = c('age','workclass','fnlwgt','education','education_num','marital_status','occupation','relationship','race','sex','cap_gain','cap_loss','hours_week','native_country','label')

# Some cleaning and selection of variables
data = data[complete.cases(data), ] %>% # Only data without NA
    select(age, workclass, education, race, sex, hours_week, label) %>%
    na.omit()

# Eliminating rows with ' ?' values
data = data[rowSums(data == " ?") == 0, , drop = FALSE]

# Checking the presence of NAs
NAcount = function(vector){
    return(sum(is.na(vector)))
}
if (sum(sapply(data, NAcount)) != 0){cat('ERROR: NAs detected.')}

# Just for quicker processing: removal of 30% of the data. I would not do this in a real application.
library(caret)
part = createDataPartition(data$label, p=0.7, list=FALSE)
data = data[part,]

#Modification of 'workclass'
levels(data$workclass) = c(levels(data$workclass), "Public", "Self Employed")
data$workclass = gsub('.*^\\ Self.*', 'Self Employed', data$workclass)
data$workclass = gsub('.*\\gov$.*', 'Public', data$workclass)
data$workclass = gsub('.*\\pay$.*', 'Without Pay', data$workclass)
#data$workclass = factor(data$workclass, levels = c(' Private', 'Public', 'Self Employed', 'Without Pay'))

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
#data$education = factor(data$education)
#data$education = ordered(data$education, levels = c(' Preschool','Elementary School','Middle School','High School',' HS-grad',' Prof-school','Assoc','Unfinnished College',' Bachelors',' Masters',' Doctorate'))  

# Modification of 'race'
data$race = gsub(' Amer-Indian-Eskimo', 'Ind-Eskimo', data$race)
data$race = gsub(' Asian-Pac-Islander', 'Asian-Pac', data$race)

#data$sex = factor(data$sex, levels = c(' Male', ' Female'))

data = data[complete.cases(data), ]

dataPred = data %>% select(-label)
dataPred$label = ifelse(data$label == ' >50K', 1, 0)


# Failed attempt to have an external model for the label predction.
# rfModel = read_rds('rfModel.rds')

# Choice vectors
label_choice = as.vector(unique(data$label))
age_choice = c(15,25,35,45,55,65,75,90)
hours_choice = c(10,20,30,40,60,80)
ed_choice = as.vector(unique(data$education))
work_choice = as.vector(unique(data$workclass))
race_choice = as.vector(unique(data$race))
sex_choice = as.vector(unique(data$sex))

names(sex_choice) = unique(sex_choice)
names(ed_choice) = unique(ed_choice)
names(work_choice) = unique(work_choice)
names(race_choice) = unique(race_choice)


# Define UI for application that draws a histogram
# Define UI
ui = navbarPage(
    theme = bs_theme(version = 4, bootswatch = "yeti"),#theme
    
    # Application title
    titlePanel("Adult Income"
    ),
    
    # First Page 
    tabPanel('Introduction',
             fluidPage(
                 img(src="uc3mLogo.jpg", align='right', height=72.8, width=199.7),
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
                                               Therefore, it is a good option to display it through a kernel density distribution.'))),
                                 p('There is a clearly higher chance an individual gets a high anual income as it gets older.
                                    The reason why this might happen is the promotions and the improvement of the work status.')
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
                                  p(HTML(paste0(tags$code('Workclass'), 'is a factor.
                                               Therefore, it is reasonable to use barplots to show it.'))),
                                  p('Differences between groups do not seem that marked. 
                                    Further analysis show that this variable is not that useful itself to distinguish the income.')
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
                                  p(HTML(paste0(tags$code('Education'), 'is a factor.
                                               Therefore, it is reasonable to use barplots to show it.'))),
                                  p('Superior studies (specially from college up) increase the proportion of individuals with high incomes.')
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
                                  p(HTML(paste0(tags$code('Race'), 'is a factor.
                                               Therefore, it is reasonable to use barplots to show it.'))),
                                  p('There is a great imbalance in sample size between classes. 
                                    However, it can be seen that white people are more likely to have high incomes.')
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
                                  p(HTML(paste0(tags$code('Sex'), 'is a (dichotomic) factor.
                                               Therefore, it is reasonable to use barplots to show it.'))),
                                  p('The salary abysm between males and females becomes clear in this chart.
                                    Women tend to occupy lower-rank positions in companies, which might partially explain this phenomenon.')
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
                                  p(HTML(paste0(tags$code('Hours per Week'), 'is supposed to be a continuous variable. In practice, it is not entirely, as there are discrete worktimes.
                                               However, a kernel distribution allows to see just the necessary amount of information.'))),
                                  p('As expected, longer workdays are better paid and therefore chances of high incomes are greater.
                                    At least, few hours per week are very difficult to be payed over 50K.')
                                  
                              ),#sidebarPanel
                              mainPanel(
                                  br(),
                                  plotOutput('HoursPlot')
                              )#mainPanel
                        )#sidebarLayout
                    ) #HoursWeek
                 ) #tabsetPanel
             )#fluidPage    
    ),#tabPanel
    tabPanel('Predict your Income Class',
             fluidPage(
               h1('What is your income?'),
               p('In this page you can select your attributes to check if you would be a potential 50K+ earner or not!'),
               sidebarLayout(
                 sidebarPanel(
                   sliderTextInput( #Age
                     inputId = "AgeSel",
                     label = "Age",
                     choices = age_choice,
                     grid = TRUE),
                   sliderTextInput( #Hours
                     inputId = "HourSel",
                     label = "Hours per Week",
                     choices = hours_choice,
                     grid = TRUE),
                   prettyRadioButtons( #Sex
                     inputId = "SexSel",
                     label = "Sex", 
                     choices = sex_choice,
                     inline = TRUE
                   ),
                   pickerInput(
                     inputId = "WorkSel",
                     label = "Workclass", 
                     choices = work_choice,
                     multiple = FALSE
                   ),
                   pickerInput(
                     inputId = "EdSel",
                     label = "Educational Level", 
                     choices = ed_choice,
                     multiple = FALSE
                   ),
                   pickerInput(
                     inputId = "RaceSel",
                     label = "Race", 
                     choices = race_choice,
                     multiple = FALSE
                   )
                 ), #sidebar
                 mainPanel(
                   br(),
                   br(),
                   textOutput('Prediction'),
                   tags$head(tags$style("#Prediction{color: black;
                                 font-size: 40px;
                                 font-style: bold;
                                 }"))
                   # conditionalPanel(
                   #   condition = 'output.Prediction == 0',
                   #   p('It seems like you eran less than 50K...'),
                   #   img(src="nomoney.jpg", align='right', width=500)
                   # ),
                   # conditionalPanel(
                   #   condition = 'output.Prediction == 1',
                   #   p('You probably eran more than 50K a year, congratulations!'),
                   #   img(src="money.jpg", align='right', width=500)
                   # )
                 )
               )#sidebarLayout
             )#fluidPage
    ),#tabPanel
    tabPanel('Generate a Report',
            fluidPage(
              p('Click on the button below to generate a report with dynamic Plotly graphs and the prediction you obtained.'),
              br(),
              downloadButton("report", "Generate report")
            )#fluidpage
    ) #tabpanel
)#navbarPage

# Define server logic required to draw a histogram
server = function(input, output) {
    # Selector of populations in Variable Plots
    labelSelection = reactive({data %>%
            filter(label %in% input$label_sel)})
    
    predSelection = reactive({data.frame('age' = input$AgeSel,
                                         'workclass' = input$WorkSel,
                                         'education' = input$EdSel,
                                         'race' = input$RaceSel,
                                         'sex' = input$SexSel,
                                         'hours_week' = input$HoursSel)})
    
    # Variable Plot Renderers
    output$AgePlot = renderPlot(
        labelSelection() %>%
            ggplot(aes(age, fill = label)) +
            geom_density(alpha = 0.5, kernel = "gaussian") +
            geom_vline(aes(xintercept = mean(age)), linetype = 2)+
            scale_fill_manual(name = "Label", values = c("firebrick3", "dodgerblue3")) +
            theme_minimal()+
            labs(title = "Age density plot by label", x = 'Age of Individual', y = 'Density')
    ) #AgePlot
    
    output$HoursPlot = renderPlot(
        labelSelection() %>%
            ggplot(aes(hours_week, fill = label)) +
            geom_density(alpha = 0.5, kernel = "gaussian") +
            geom_vline(aes(xintercept = mean(age)), linetype = 2)+
            scale_fill_manual(name = "Label", values = c("firebrick3", "dodgerblue3")) +
            theme_minimal()+
            labs(title = "Hour per Week density plot by label", x = 'Hours/Week Worked', y = 'Density')
    ) #HoursPlot
    
    output$EdPlot = renderPlot(
        labelSelection() %>%
            ggplot(aes(x=education, fill=label)) +
            geom_bar(stat="count", width=0.7) +
            theme_minimal()+
            theme(axis.text.x = element_text(angle = 45, vjust=1, hjust = 1)) +
            scale_fill_manual(name = "Label", values = c("firebrick3", "dodgerblue3")) +
            labs(title = "Education barplot by label" , x = 'Hours/Week Worked', y = 'Density')
    ) #EdPlot
    
    output$WorkPlot = renderPlot(
        labelSelection() %>%
            ggplot(aes(x=workclass, fill=label)) +
            geom_bar(stat="count", width=0.7) +
            theme(axis.text.x = element_text(angle = 45, vjust=1, hjust = 1)) +
            scale_fill_manual(name = "Label", values = c("firebrick3", "dodgerblue3")) +
            theme_minimal()+
            labs(title = "Workclass barplot by label" , x = 'Hours/Week Worked', y = 'Density')
    ) #WorkPlot
    
    output$SexPlot = renderPlot(
        labelSelection() %>%
            ggplot(aes(x=sex, fill=label)) +
            geom_bar(stat="count", width=0.7) +
            theme(axis.text.x = element_text(angle = 45, vjust=1, hjust = 1)) +
            scale_fill_manual(name = "Label", values = c("firebrick3", "dodgerblue3")) +
            theme_minimal() +
            labs(title = "Sex barplot by label" , x = 'Hours/Week Worked', y = 'Density')
    ) #SexPlot
    
    output$RacePlot = renderPlot(
        labelSelection() %>%
            ggplot(aes(x=race, fill=label)) +
            geom_bar(stat="count", width=0.7) +
            theme(axis.text.x = element_text(angle = 45, vjust=1, hjust = 1)) +
            scale_fill_manual(name = "Label", values = c("firebrick3", "dodgerblue3")) +
            theme_minimal()+
            labs(title = "Race barplot by label" , x = 'Hours/Week Worked', y = 'Density')
    ) #RacePlot
    
    output$Prediction = renderText(
      ifelse(predict(glm(label~age+workclass+education+race+sex+hours_week, data=dataPred, family=binomial), 
                     newdata = data.frame('age'       = input$AgeSel,
                                         'workclass'  = input$WorkSel,
                                         'education'  = input$EdSel,
                                         'race'       = input$RaceSel,
                                         'sex'        = input$SexSel,
                                         'hours_week' = input$HourSel), 
                     type="response") < 0.5, 'It seems like you eran less than 50K...', 'You probably eran more than 50K a year, congratulations!') #ifelse
    ) #Prediction
    
    output$report = downloadHandler(
      filename = "report.html",
      content = function(file) {
        tempReport = file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        params = list(
          label = isolate(input$label_sel), 
          age = isolate(input$AgeSel),
          work = isolate(input$WorkSel),
          ed = isolate(input$EdSel),
          race = isolate(input$RaceSel),
          sex = isolate(input$SexSel),
          hours = isolate(input$HourSel)
        )
        
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
