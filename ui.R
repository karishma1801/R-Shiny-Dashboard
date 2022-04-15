
library(shiny)
library(shinydashboard)
library(plotly)


# Define UI for application that draws a histogram
shinyUI(dashboardPage(skin="yellow",
                      #Application title
                      dashboardHeader(title = "Mental Health Survey",titleWidth = 600),
                      # dashboard sidebar functions will be inserted here
                      dashboardSidebar(
                        
                        sidebarMenu(
                          menuItem("For Employee",tabName = "employee"),
                          menuItem("For Employer",tabName = "employer")
                          
                        ),
                        sliderInput("age",
                                    label = "Age range",
                                    min =  15,
                                    max = 80,
                                    step = 2,
                                    value = c(15,80),
                                    sep = ""),
                        uiOutput("typeSelectOutput"),
                        radioButtons("gender",
                                     label = "Select Gender:",
                                     choices = c('Female' = 'female', 'Male'='male','Others'= 'others'),
                                     selected = "female"),
                        radioButtons("companysize",
                                     label = "Select Company Size:",
                                     choices = c("1-25" = "6-25",
                                                 "26-100" = "26-100",
                                                 "500-1000" = "500-1000",
                                                 ">1000" = "More than 1000"),
                                     selected = "26-100"),
                        radioButtons("company",
                                     label = "Type of company",
                                     choices =  c("Tech" = "Yes",
                                                  "Other" = "No"
                                                  ),
                                     selected = "Yes")
                        
                      ),
                      # functions that must go in the body of the dashboard.
                      dashboardBody(
                        tabItems(
                          tabItem(tabName = "employee",
                                  h3("Employee benefit"),
                                  plotOutput("plot1"),
                                  br(),
                                  h3("Probabilities of mental health and company size"),
                                  plotOutput("plot4"),
                                  
                          ),
                          tabItem(tabName = "employer",
                                  h3("Employees ready to seek help"),
                                  plotOutput("plot2"),
                                  h3("Would you bring up a mental health issue with a potential employer in an interview?"),
                                  plotOutput('plot3')
                          )
                          
                        )
                      )
))
