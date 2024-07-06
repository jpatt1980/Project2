## Project 2
## Jason Pattison, ST 588-601 SUM I 2024

library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(scales)  


### Generate `govt_spending` unction that will be used to generate agency financial reports by type. 

govt_spending <-function(agency_name = "Department of Defense", report = "Budgetary Resources") {
  
  #  First, we need to convert our agency name into it's associated code
  #  for use in pulling the differing data sets at the endpoints. 
  
#  if(agency_name %in% "Department of Defense") {
#    agency_code = "097"
#  } else if(agency_name %in% "Department of Agriculture") {
#    agency_code = "012"
#  } else if(agency_name %in% "General Services Administration") {
#    agency_code = "047"
#  } else if(agency_name %in% "Department of Housing and Urban Development") {
#    agency_code = "086"
#  } else if(agency_name %in% "Department of Veterans Affairs") {
#    agency_code = "036"
#  } else if(agency_name %in% "Social Security Administration") {
#    agency_code = "028"
#  } else if(agency_name %in% "Department of Health and Human Services") {
#    agency_code = "075"
#  } else if(agency_name %in% "Federal Communications Commission") {
#    agency_code = "027"
#  } else if(agency_name %in% "Small Business Administration") {
#    agency_code = "073"
#  } else if(agency_name %in% "Railroad Retirement Board") {
#    agency_code = "060"
#  } else if(agency_name %in% "All") {
#    print ("Error: Incorrect Selection")
#  }
  
  
    if_else(agency_name %in% c("Department of Defense"), agency_code <- c("097"),
      if_else(agency_name %in% c("Department of Agriculture"), agency_code <- c("012"),
        if_else(agency_name %in% c("General Services Administration"), agency_code <- c("047"),
          if_else(agency_name %in% c("Department of Housing and Urban Development"), agency_code <- c("086"),
            if_else(agency_name %in% c("Department of Veterans Affairs"), agency_code <- c("036"), 
              if_else(agency_name %in% c("Social Security Administration"), agency_code <- c("028"),
                if_else(agency_name %in% C("Department of Health and Human Services"), agency_code <- c("075"),
                  if_else(agency_name %in% c("Federal Communications Commission"), agency_code <- c("027"), 
                    if_else(agency_name %in% c("Small Business Administration"), agency_code <- c("073"), 
                      if_else(agency_name %in% c("Railroad Retirement Board"), agency_code = "060", print ("Error: Incorrect Selection")
                              ))))))))))

  
  
  #  Next we will generate conditional logic statements to determine the
  #  endpoint information that will be generated. 
  
#  if(report %in% "Budgetary Resources") {
#    endpoint <- c("budgetary_resources")
#  } else if(report %in% "Federal Account") {
#    endpoint <- c("federal_account")
#  } else if(report %in% "Obligation Type") {
#    endpoint <- c("object_class")
#  } else if(report %in% "Award Obligations") {
#    endpoint <- c("obligations_by_award_category")
#  } else if(report %in% "Program Activity") {
#    endpoint <- c("program_activity")
#  } 
  
  
  if_else(report %in% c("Budgetary Resources"), endpoint <- c("budgetary_resources"), 
    if_else(report %in% c("Federal Account"), endpoint <- c("federal_account"), 
      if_else(report %in% c("Obligation Type"), endpoint <- c("object_class"), 
        if_else(report %in% c("Award Obligations"), endpoint <- c("obligations_by_award_category"), 
          if_else(report %in% c("Program Activity"), endpoint <- c("program_activity"), print ("Error: Incorrect Selection"))
        ))))
  
  
  #  Now that we have the conditional logic statements generated, we
  #  will use them to generate the API URLs and parse the API data 
  #  we are going to use to generate our data tables and graphics.
  
  url_raw_data <- httr::GET(str_c("https://api.usaspending.gov/api/v2/agency/",agency_code, "/", endpoint, "/"))
  
  url_parsed <- jsonlite::fromJSON(rawToChar(url_raw_data$content))
  
  
  
  #  Because each endpoint has different variables associated with it,      
  #  we will be using more conditional logic to extract usable data and
  #  generate our summary graphics.
  #  
  #  Each data set had the agency code added to it for merging with the
  #  agency name. This is necessary for the `full_join` that will be 
  #  used to compare the different agencies with one another on the
  #  `Data Exploration` tab of the app. 
  #
  #  There are 6 endpoints in all that will be analyzed. We will be 
  #  using a `right_join` for each data set to merge the 
  
  
  #  The first data table we're going to read in is
  if(endpoint %in% "budgetary_resources") {
    df <- as_tibble(url_parsed$agency_data_by_year) |>
      dplyr::select(fiscal_year:total_budgetary_resources) |>
      rename("agency_budget" = agency_budgetary_resources,
             "agency_obligations" = agency_total_obligated) |>
      mutate("agency_code" = agency_code, .before = 1)
    
    df$agency_code <- as.numeric(df$agency_code)
    
    endpoint_df <- agency_key |>
      dplyr::select(agency_name, agency_code) |>
      right_join(df, join_by(agency_code))  
    
    print(endpoint_df)
    
    endpoint_df_plot <- ggplot(endpoint_df, aes(x = fiscal_year, y = agency_budget))
    
    endpoint_df_plot +
      geom_line(aes(y = agency_budget, color = "Budget")) +
      geom_line(aes(y = agency_obligations, color = "Obligations")) +
      scale_y_continuous(labels=comma) +
      scale_color_manual("$ Amount Type", 
                         breaks = c("Budget", "Obligations"), 
                         values = c("blue", "red")) +
      labs(title = "Agency Budget", x = "Budget Year", y = "$ Amount") 
    
    
  } else if(endpoint %in% "federal_account") {
    df <- as_tibble(url_parsed$results) |>
      dplyr::select(code, name, obligated_amount, gross_outlay_amount) |>
      mutate("agency_code" = agency_code, .before = 1)
    
    df$agency_code <- as.numeric(df$agency_code)
    
    endpoint_df <- agency_key |>
      dplyr::select(agency_name, agency_code) |>
      right_join(df, join_by(agency_code))  
    
    print(endpoint_df)
    
    endpoint_df_plot <- ggplot(endpoint_df, aes(code, obligated_amount, fill = code))
    
    endpoint_df_plot +
      geom_col() +
      scale_y_continuous(label = comma)+
      theme(axis.text.x = element_blank()) +
      scale_fill_discrete(name = "Program Code")+
      labs(x = "Program Code", y = "$ Amount")
    
  } else if(endpoint %in% "object_class") {
    df <- as_tibble(url_parsed$results) |>
      rename("obligation_name" = name) |>
      mutate("agency_code" = agency_code, .before = 1)
    
    df$agency_code <- as.numeric(df$agency_code)
    
    
    endpoint_df <- agency_key |>
      dplyr::select(agency_name, agency_code) |>
      right_join(df, join_by(agency_code))  
    
    print(endpoint_df)
    
    
    endpoint_df_plot <- ggplot(endpoint_df, aes(obligation_name, obligated_amount, color = obligation_name))
    
    endpoint_df_plot +
      geom_point(aes(gross_outlay_amount, shape = "Object")) +
      geom_point(aes(obligated_amount, shape = "Obligated")) +
      scale_x_continuous() +
      scale_y_continuous(labels=comma) +
      scale_color_discrete(name = "Obligation Type") +
      scale_shape_discrete(name = "Shape") +
      theme(axis.text.x = element_blank()) +
      labs(x = "Obligation", y = "$ Amount Spent") +
      coord_cartesian(ylim = c(0,120000000000))
    
  } else if(endpoint %in% "obligations_by_award_category") {
    df <- as_tibble(url_parsed$results) |>
      mutate("agency_code" = agency_code, .before = 1)  
    
    df$agency_code <- as.numeric(df$agency_code)      
    
    endpoint_df <- agency_key |>
      dplyr::select(agency_name, agency_code) |>
      right_join(df, join_by(agency_code))  
    
    print(endpoint_df)
    
    endpoint_df_plot <- ggplot(data = endpoint_df, aes(x = category, y = aggregated_amount, fill = category))
    
    endpoint_df_plot +
      geom_col() +
      scale_y_continuous(labels = comma)+
      scale_fill_discrete(name = "Obligation Award Type")+
      theme(axis.text.x = element_blank()) +
      coord_cartesian(ylim = c(0,120000000000))
    
  } else if(endpoint %in% "program_activity") {
    no_na <- c("Undisclosed") 
    
    df <- as_tibble(url_parsed$results) |>
      mutate("agency_code" = agency_code, .before = 1) |>
      mutate(name = str_replace(name, "N/A", "UNDISCLOSED"))        
    
    df$agency_code <- as.numeric(df$agency_code)      
    
    
    
    endpoint_df <- agency_key |>
      dplyr::select(agency_name, agency_code) |>
      right_join(df, join_by(agency_code))  
    
    print(endpoint_df)
    
    endpoint_df_plot <- ggplot(data = endpoint_df, aes(x = name, y = gross_outlay_amount))
    
    endpoint_df_plot +
      geom_point(aes(gross_outlay_amount, color = "Budgeted")) +
      geom_point(aes(obligated_amount, color = "Obligated")) +
      scale_y_continuous(labels = comma)+
      scale_fill_discrete(name = "Program Activity")+
      labs(x = "Program Activity", y = "$ Amount Spent") +
      theme(axis.text.x = element_blank())+
      theme(strip.text = element_text(size = 6)) +
      scale_color_manual("", 
                         breaks = c("Budgeted", "Obligated"), 
                         values = c("blue", "red")) +
      facet_wrap(~name, labeller = labeller(name = label_wrap_gen(width = 15)))
    
  } 
  
}

### End of `govt_spending` function. 


  server <- function(input, output) {
  
#  Create the background code for the data download and exploration tabs.
    
##  Data download    
###  Create the `agency_key` data set that will be used to 
###  generate the ggplot and slider which will control the 
###  y-axis on the graphic on the download tab. 
    
    agency_key_raw <- httr::GET("https://api.usaspending.gov/api/v2/agency/awards/count")
    
    agency_key_parsed <- jsonlite::fromJSON(rawToChar(agency_key_raw$content))
    
    agency_key_df <- as_tibble(agency_key_parsed$results[[1]]) |>
      rename("agency_name" = awarding_toptier_agency_name,
             "agency_code" = awarding_toptier_agency_code,
             "indef_deliv_contract" = idvs) |>
      mutate(total_awards = contracts + direct_payments + grants + indef_deliv_contract + loans + other) |>
      dplyr::arrange(desc(total_awards))
    
    agency_key_df$agency_code <- as.numeric(agency_key_df$agency_code)
    
    agency_key <- agency_key_df
    
    agency_key
    


    
## Output the plot and data table to the "Download" tab.     
    
#    output$table1 <- renderDataTable(agency_key, options = #list(pageLength = 5, lengthMenu = c(5, 10), scrollX = TRUE)
#                                    )
 
   
    
    output$plot1 <- renderPlot({
    
      agency_key_plot <- ggplot(agency_key, aes(x = agency_name, y = total_awards, fill = agency_name))
      
      agency_key_plot +
        geom_col() +
        scale_fill_discrete(name = "Agency") +
        theme(axis.text.x = element_blank()) +  
        labs(title = "Awards Granted per Agency", x = "Agency", y = "# of Awards Granted") +
        ylim(0, input$slider)
      
    })
    
    
    ## Output the data table and plot based on user selections of "Federal Department" and "Financial Area"

#    agency_name <- reactive(input$dept)
#     
#    report <- reactive(input$report_area)
#    
#    govt_spending(paste0(agency_name), paste0(report))    
    
    
    agency_name <- reactive ({
          input$dept
    })
    
    report <- reactive ({
      input$report_area
    })
    
    govt_spending(agency_name, report)
    

    

    
    
    
#  Generate code for the "Explore" tab. 

  ##  Generate the combined data set of all 10 Departments that will be used to cross-analyze their financials. 

##  Generate graph/table of the data.      

    
# Server closing bracket   
  }
