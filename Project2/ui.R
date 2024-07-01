library(shiny)
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
            h6("The purpose of this app is to allow the user to summarize Federal spending information from the following departments:"),
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
            h6("The data being summarized can be found at 'https://api.usaspending.gov/docs/endpoints'. The user can summarize the data for each of the departments separately, or a combination of selected "),
            br(),
            
            h4("Tabs:"),
            h6("The 'Data Download' Tab allows the user to download the budget and obligation data for the departments separately, or all of the departments collectively."),
            br(),
            
            h4("Include a picture of the data"),
            tags$img(src = "https://files.usaspending.gov/django_static/img/logo.png")
    ),
    
    tabItem(tabName = "download",
            h2("Data Download"),
            box(
              plotOutput("plot1", width = 500, height = 250)
            ),
            
            box(
              sliderInput("slider", "Number of Awards Granted:", 0, 2000000, 1000000)
            ),

            
            box(
              "Select Federal Department",
              selectInput("dept", "Federal Department", choices = c("All", "Department of Defense", "Department of Agriculture", "General Services Administration", "Department of Housing and Urban Development", "Department of Veterans Affairs", "Social Security Administration", "Department of Health and Human Services", "Federal Communications Commission", "Small Business Administration", "Railroad Retirement Board"), selected = "Department of Defense")
            ),
            
            box(
              "Select Financial Area",
              selectInput("report_area", "Financial Area", choices = c("Budgetary Resources", "Federal Account", "Obligation Type", "Award Obligations", "Program Activity"), selected = "Program Activity")
            )
    ),
    
    tabItem(tabName = "exploration",
            h2("Data Exploration"),
          box(
            "Select Federal Department",
            selectInput("dept", "Federal Department", choices = c("All", "Department of Defense", "Department of Agriculture", "General Services Administration", "Department of Housing and Urban Development", "Department of Veterans Affairs", "Social Security Administration", "Department of Health and Human Services", "Federal Communications Commission", "Small Business Administration", "Railroad Retirement Board"), selected = "Department of Defense")
            ),
          
          box(
            "Select Financial Area",
            selectInput("report_area", "Financial Area", choices = c("Budgetary Resources", "Federal Account", "Obligation Type", "Award Obligations", "Program Activity"), selected = "Program Activity")
          )
    )
  )
)


# Put them together into a dashboardPage
dashboardPage(
  skin = "red",
  dashboardHeader(title = "US Fed Dept Spending", titleWidth = 250),
  sidebar,
  body
)