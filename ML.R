require(tidyverse)
library(dplyr)
library(caret)
library(doParallel)

data =  read.csv(url('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data'), header=FALSE, stringsAsFactors = TRUE, sep=',')
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

# Just for quicker processing: removal of 70% of the data. I would not do this in a real application.
# part = createDataPartition(data$label, p=0.3, list=FALSE)
# data = data[part,]

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
data$race = factor(data$race)

data$label = factor(ifelse(data$label == ' >50K', 'Yes', 'No'))

data$age = as.numeric(data$age)
data$hours_week = as.numeric(data$hours_week)


require(randomForest)

rfModel = randomForest(label ~ . , data = data, na.action=na.roughfix)

saveRDS(rfModel, 'rfModel.rds')
