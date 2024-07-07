## Project 2
## Jason Pattison, ST 588-601 SUM I 2024

library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(scales) 
library(shinydashboard)


sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(
    menuItem("About", icon = icon("lightbulb"), tabName = "about"),
    menuItem("Data Download", icon = icon("file"), tabName = "download"),
    menuItem("Data Exploration", icon = icon("chart-simple"), tabName = "exploration")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "about",
            h2("About tab content"),
            br(),
            
            h4("Purpose:"),
            h6("The purpose of this app is to allow the user to summarize federal department spending information of the following departments as reported by `USA Spending`:"),
            h6(tags$ul(
              tags$li("Department of Defense"),
              tags$li("Department of Agriculture"),
              tags$li("General Services Administration"),
              tags$li("Department of Housing and Urban Development"),
              tags$li("Department of Veterans Affairs"),
              tags$li("Social Security Administration"),
              tags$li("Department of Health and Human Services"),
              tags$li("Federal Communications Commission"),
              tags$li("Small Business Administration"),
              tags$li("Railroad Retirement Board"))),
            br(),
  
            h4("Data Source:"),
            h6("The data being summarized can be found at 'https://api.usaspending.gov/docs/endpoints'."),
            tags$img(src = "https://files.usaspending.gov/django_static/img/logo.png"),
            br(),
            br(),
            
            h4("Tabs:"),
            p(h6("The 'Data Download' tab provides a summary graphic and data table for comparison of spending across the ten agencies `USA Spending` reported on. This tab also allows the user to download the budget and report data for the departments separately.")),
            p(h6("The 'Data Exploration' tab allows the user to select the different reports for comparison of the agencies with the three highest number of awards given. ")),
            br()
        ), # <--- End of About Tab code
    
    tabItem(tabName = "download",
            h2("Data Download"),      
        
        fluidRow(
        # Generate column plot from `agency_key` data set  
          column(width = 12,
            box(width = NULL, solidHeader = TRUE,
              plotOutput("plot1", height = 200)
            )
            ), # <- end of `agency_key` plot
          
        # Generate slider that will adust the y-axis of the `agency_key` plot
          column(width = 12, 
            box(width = NULL, height = 96, solidHeader = TRUE,
              sliderInput("slider", "# of Awards Granted:", 0, 2500000, 2500000)
            )
            ),
        
        # Generate data table associated with `agency_key` data set
          column(width = 12, 
            box(title = "Awards Granted per Agency", width = NULL, solidHeader = TRUE,
              DT::DTOutput('table1')
               )
           ), 
        
        # Generate selection parameter options for use in student generated function `govt_spending`
          # Generate parameter for `agency_name` entry
            box(
              "Select Federal Department",
              selectInput("dept", "Federal Department", choices = c("Department of Defense", "Department of Agriculture", "General Services Administration", "Department of Housing and Urban Development", "Department of Veterans Affairs", "Social Security Administration", "Department of Health and Human Services", "Federal Communications Commission", "Small Business Administration", "Railroad Retirement Board"), selected = "Department of Defense")
            ),
            
          # Generate parameter for `report` entry
            box(
              "Select Financial Area",
              selectInput("report_area", "Financial Area", choices = c("Budgetary Resources", "Federal Account", "Obligation Type", "Award Obligations", "Program Activity"), selected = "Program Activity")
            ), 
          
        # Generate data table associated with `govt_spending` created data set
          column(width = 12,
            box(title = "Department Financial Report",  width = NULL, solidHeader = TRUE,
             DT::DTOutput('table2'),
             downloadButton("downloadData", "Download")
               )
          ),
        
        # Generate downolad for data table
          
)
    ), # <--- End of Download Tab code 
  
  
    tabItem(tabName = "exploration",
            h2("Data Exploration"),
#          box(
#            "Select Federal Department",
#            selectInput("dept", "Federal Department", choices = c("Department of Defense", "Department of Agriculture", "General #Services Administration", "Department of Housing and Urban Development", "Department of Veterans Affairs", "Social Security #Administration", "Department of Health and Human Services", "Federal Communications Commission", "Small Business Administration", #"Railroad Retirement Board"), selected = "Department of Defense")
#            ),
#          
          box(
            "Select Financial Area",
            selectInput("report_area", "Financial Area", choices = c("Budgetary Resources", "Federal Account", "Obligation Type", "Award Obligations", "Program Activity"), selected = "Program Activity")
          ), 
    )  # end of Exploration Tab Code
) # <- end of tabItems code
) # <- end of dashboardBody code




# Put them together into a dashboardPage
dashboardPage(
  skin = "red",
  dashboardHeader(title = "US Fed Dept Spending", titleWidth = 250),
  sidebar,
  body
)