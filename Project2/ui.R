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
              tags$li("General Services Administration"))),
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
              "Select Department for Data Download",
              selectInput("dept", "Federal Department", choices = c("All", "Department of Defense", "Department of Agriculture", "General Services Administration"), selected = "All")
            )
    ),
    
    tabItem(tabName = "exploration",
            h2("Data Exploration"),
          box(
            "Select Department for summary analysis",
            selectInput("dept", "Federal Department", choices = c("All", "Department of Defense", "Department of Agriculture", "General Services Administration"), selected = "All")
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