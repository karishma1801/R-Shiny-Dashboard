#importing libraries
library('shiny')
library('tidyverse')
library('stringr')
library('shinydashboard')
library('plotly')
library('shiny')
library('shinyjs')
library("DT")
library('dplyr')
library('markdown')
library('zoo')
library('tidyr')
library('plyr')
library(viridisLite)
library(cowplot)
library(treemap)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(maps)
library(countrycode)
library(purrr)
library(htmltools)


#Reading the dataset
survey <- read.csv("C:/Users/karishma/OneDrive/Documents/CPSC/CPSC_4830/Final_Exam/R_Shiny/survey.csv")
str(survey)

#summary
glimpse(survey)
#1259 rows and 27 columns

# missing values
sapply(survey, function(x) sum(is.na(x)))

#missing values are present in state, work_interfere, self_employed and comments

#looking at state the NA value are considered as missing.
survey$state <- survey$state %>% replace_na("NA")

# check for missing values
sapply(survey, function(x) sum(is.na(x)))

#dropping commments column
survey$comments <- NULL

#checking summary for work_interface and self_employed
summary(survey)

survey$work_interfere <- survey$work_interfere %>% replace_na("NA")
survey$self_employed <- survey$self_employed %>% replace_na("NA")

# check for missing values
sapply(survey, function(x) sum(is.na(x)))

#checking accuracy for data
str(survey)

#accuracy check for categorical variables
#unique value count for no_employees and gender

table(survey['no_employees'])

table(survey['Gender'])

#unique value count for gender
table(survey['Gender'])

#accuracy check for numerical variables
summary(survey)

#histogram
plot_outlier(survey)


#grouping gender column
survey$Gender <- as.character(survey$Gender)
survey$Gender <- tolower(survey$Gender)

Female <- c('female', 'cis female', 'f', 'woman', 'femake', 'female ', 'cis-female/femme', 'female (cis)', 'femail')

Male <- c('m', 'male', 'male-ish', 'maile', 'cis male', 'mal', 'male (cis)', 
          'make', 'male ', 'man', 'msle', 'mail', 'malr', 'cis man','Mle')

Others <- c('others','a little about you','agender','androgyne','queer/she/they', 'non-binary', 'nah', 'enby', 'fluid', 'genderqueer',  
            'guy (-ish) ^_^', 'male leaning androgynous', 'neuter', 'queer','p','all','trans-female', 'trans woman',
            'female (trans)','something kinda male?','ostensibly male, unsure what that really means')


Gender_new <- as.vector(survey$Gender)  

Gender_new <- sapply(as.vector(Gender_new), function(x) if(x %in% Male) "Male" else x)
Gender_new <- sapply(as.vector(Gender_new), function(x) if(x %in% Female) "Female" else x)
Gender_new <- sapply(as.vector(Gender_new), function(x) if(x %in% Others) "Others" else x)

survey$Gender<- Gender_new

#outliers are in age some values are less than 0 and greater than 100, so we will drop this values.
survey<-survey[!(survey$Age<0 | survey$Age >100),]


#histogram
plot_outlier(survey)



#checking null values
sum(duplicated(survey))

write.csv(survey,"C:/Users/karishma/OneDrive/Documents/CPSC/CPSC_4830/Final_Exam/Rshiny/survey_updated.csv")

#company size vs benefits according to age
survey$no_employees <- as.factor(survey$no_employees)

survey %>%
  filter(Age>21, Age<24) %>%
  filter(Country %in% c('United')) %>%
  filter(Gender == "Female") %>%
  ggplot(aes(x = no_employees, fill = benefits)) + geom_bar(position="fill")


#How company size and work type affect the mental health?
survey %>% 
  filter(Age>21, Age<24) %>%
  filter(Gender == "Male") %>%
  ggplot(aes(x=no_employees,y=treatment, fill=factor(tech_company)), color=factor(vs)) +  
  stat_summary(fun=mean,position=position_dodge(),geom="bar")


#People ready to seek help
survey$seek_help <- as.factor(survey$seek_help)
survey %>% 
  filter(Age>21, Age<24) %>%
  filter(Gender == "Male" & no_employees == "100-500") %>%
  ggplot(aes(x = no_employees, fill = seek_help)) + geom_bar(position = "dodge")


#Would you bring up a mental health issue with a potential employer in an interview?
survey %>%
  filter(tech_company =="Yes") %>%
  ggplot(aes(x=mental_health_consequence)) + geom_bar(fill = "darkblue")

#Would you bring up a physical health issue with a potential employer in an interview?
survey %>%
  filter(tech_company =="Yes") %>%
  ggplot(aes(x=phys_health_consequence)) + geom_bar(fill = "darkblue")