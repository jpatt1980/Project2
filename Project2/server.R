#---
#title: "Project 2 - server.R"
#author: "Jason M. Pattison, ST 588-651 Summer 1 2024"
#---

library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(scales)
library(treemapify)
library(shinydashboard)


############### Create the background code for use inside of the server function ###############


########## Create the `agency_key` data set ##########

#  This data set will be used to generate a bar plot with a slider that will control the y-axis on  
#  the "Awards Granted per Agency" graphic on the download tab. `agency_key` also ties into user
#  created functions `govt_spending` and `combined_govt_spending` that are used for generating 
#  summary analysis graphics and data tables. 
  
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
  
  
########## Generate `govt_spending` function ##########
  
#  This function will be used to generate department financial reports by type to be used on the
#  "Data Download" page
  
  govt_spending <-function(agency_name, report) {
  
  #  First, we need to convert our agency name into it's associated agency code. The agency codes 
  #  are required inputs for the URLs
  

########## 
#
#  Running the following "else if" conditional logic continues to flag an error involving the "%in%" #####
#  language The error occurs when placing input$dept and input$report_area into reactive 
#  functions. I have not been able to figure out how to resolve this. 
#    
#  Error text code is: 
#     "Error in match(x, table, nonmatch = 0L): 'match' requires vector arguments"
#
##########
    
    if(agency_name %in% "Department of Defense") {
      agency_code = "097"
    } else if(agency_name %in% "Department of Agriculture") {
      agency_code = "012"
    } else if(agency_name %in% "General Services Administration") {
      agency_code = "047"
    } else if(agency_name %in% "Department of Housing and Urban Development") {
      agency_code = "086"
    } else if(agency_name %in% "Department of Veterans Affairs") {
      agency_code = "036"
    } else if(agency_name %in% "Social Security Administration") {
      agency_code = "028"
    } else if(agency_name %in% "Department of Health and Human Services") {
      agency_code = "075"
    } else if(agency_name %in% "Federal Communications Commission") {
      agency_code = "027"
    } else if(agency_name %in% "Small Business Administration") {
      agency_code = "073"
    } else if(agency_name %in% "Railroad Retirement Board") {
      agency_code = "060"
    } else {print ("Error: Incorrect Agency Selection")}
  
  
  #  With the agency's name associated with its code, we will now use conditional logic to 
  #  generate the remaining portion of the report URL endpoints. 

    if(report %in% "Budgetary Resources") {
      endpoint <- "budgetary_resources"
    } else if(report %in% "Federal Account") {
      endpoint <- "federal_account"
    } else if(report %in% "Obligation Type") {
      endpoint <- "object_class"
    } else if(report %in% "Award Obligations") {
      endpoint <- "obligations_by_award_category"
    } else if(report %in% "Program Activity") {
      endpoint <- "program_activity"
    } else {print("Error: Incorrect Report selection")}


  #  Now that we have the conditional logic statements generated, we will use them to generate
  #  the API URLs and parse the API data that will be used to generate our data tables and
  #  graphics.
  
    url_raw_data <- httr::GET(str_c("https://api.usaspending.gov/api/v2/agency/",agency_code, "/", endpoint, "/"))
  
    url_parsed <- jsonlite::fromJSON(rawToChar(url_raw_data$content))
  
  
  #  Because each endpoint has different variables associated with it, we will be using more     
  #  conditional logic to extract usable data and generate our summary graphics.
  # 
  #  Each data set had the agency code added to it for merging with the agency name. 
  #  This is necessary for the `full_join` that will be used to compare the
  #  different agencies with one another on the `Data Exploration` tab of the app. 
  #
  #  There are 5 endpoints in all that will be analyzed. We will be using a `right_join` 
  #  for each data set to merge the `agency_key` information to each of the report data tables. 

  #  The first financial report we're going to read in is "budgetary_resources"
    if(endpoint %in% "budgetary_resources") {
      df <- as_tibble(url_parsed$agency_data_by_year) |>
        dplyr::select(fiscal_year:total_budgetary_resources) |>
        rename("agency_budget" = agency_budgetary_resources,
               "agency_obligations" = agency_total_obligated) |>
        mutate("agency_code" = agency_code, .before = 1)
    
      df$agency_code <- as.numeric(df$agency_code)
    
      endpoint_df <<- agency_key |>
      dplyr::select(agency_name, agency_code) |>
      right_join(df, join_by(agency_code))  
    
      endpoint_df_plot <- ggplot(endpoint_df, aes(x = fiscal_year, y = agency_budget))
    
      endpoint_df_plot +
        geom_line(aes(y = agency_budget, color = "Budget")) +
        geom_line(aes(y = agency_obligations, color = "Obligations")) +
        scale_y_continuous(labels=comma) +
        scale_color_manual("$ Amount Type", 
                           breaks = c("Budget", "Obligations"), 
                           values = c("blue", "red")) +
        labs(title = "Agency Budget", x = "Budget Year", y = "$ Amount") 
    
    } ##### <- end of "budgetary_resources"

    #  The next financial report we're going to read in is "federal_account"   
    else if(endpoint %in% "federal_account") {
      df <- as_tibble(url_parsed$results) |>
        dplyr::select(code, name, obligated_amount, gross_outlay_amount) |>
        mutate("agency_code" = agency_code, .before = 1)
    
      df$agency_code <- as.numeric(df$agency_code)
    
      endpoint_df <<- agency_key |>
        dplyr::select(agency_name, agency_code) |>
        right_join(df, join_by(agency_code))  
    
      endpoint_df_plot <- ggplot(endpoint_df, aes(code, obligated_amount, fill = code))
    
      endpoint_df_plot +
        geom_col() +
        scale_y_continuous(label = comma)+
        theme(axis.text.x = element_blank()) +
        scale_fill_discrete(name = "Program Code")+
        labs(x = "Program Code", y = "$ Amount")
    
    } ##### <- end of "federal_account"

    #  The next financial report we're going to read in is "object_class"     
    else if(endpoint %in% "object_class") {
      df <- as_tibble(url_parsed$results) |>
        rename("obligation_name" = name) |>
        mutate("agency_code" = agency_code, .before = 1)
    
      df$agency_code <- as.numeric(df$agency_code)
    
      endpoint_df <<- agency_key |>
        dplyr::select(agency_name, agency_code) |>
        right_join(df, join_by(agency_code))  
    
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
    
    } ##### <- end of "object_class"
  
    #  The next financial report we're going to read in is "obligations_by_award_category"     
    else if(endpoint %in% "obligations_by_award_category") {
      df <- as_tibble(url_parsed$results) |>
        mutate("agency_code" = agency_code, .before = 1)  
    
      df$agency_code <- as.numeric(df$agency_code)      
    
      endpoint_df <<- agency_key |>
        dplyr::select(agency_name, agency_code) |>
        right_join(df, join_by(agency_code))  
    
      endpoint_df_plot <- ggplot(data = endpoint_df, aes(x = category, y = aggregated_amount, fill = category))
    
      endpoint_df_plot +
        geom_col() +
        scale_y_continuous(labels = comma)+
        scale_fill_discrete(name = "Obligation Award Type")+
        theme(axis.text.x = element_blank()) +
        coord_cartesian(ylim = c(0,120000000000))
    
    } ##### <- end of "obligations_by_award_category"
  
    #  The final financial report we're going to read in is "program_activity"     
    else if(endpoint %in% "program_activity") {
      no_na <- c("Undisclosed") 
    
      df <- as_tibble(url_parsed$results) |>
        mutate("agency_code" = agency_code, .before = 1) |>
        mutate(name = str_replace(name, "N/A", "UNDISCLOSED"))        
    
      df$agency_code <- as.numeric(df$agency_code)      
    
      endpoint_df <<- agency_key |>
        dplyr::select(agency_name, agency_code) |>
        right_join(df, join_by(agency_code))  
    
      endpoint_df_plot <<- ggplot(data = endpoint_df, aes(x = name, y = gross_outlay_amount))
    
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
    
    } ##### <- end of "program_activity"
  
  } ########## End of `govt_spending` function ##########


  # Now that the `govt_spending` function is complete, we will generate combined data sets
  # for comparison analysis of the agencies across each of the report types
  
  
##########  Generate function `combined_agency_spending` ##########

  combined_agency_spending <- function(report2) {

    # Here we are going to use the same conditional logic to generate the endpoints for the URL.
    # We are not using the conditional logic for the departments because the data set will have
    # combined data for the top 3 agencies with the highest number of award obligations. 
    
    if(report2 %in% "Budgetary Resources") {
      endpoint <- c("budgetary_resources")
    } else if(report2 %in% "Federal Account") {
      endpoint <- c("federal_account")
    } else if(report2 %in% "Obligation Type") {
      endpoint <- c("object_class")
    } else if(report2 %in% "Award Obligations") {
      endpoint <- c("obligations_by_award_category")
    } else if(report2 %in% "Program Activity") {
      endpoint <- c("program_activity")
    } else {
      print ("Error: Incorrect Report Selection")
    }
  
    # With the endpoints established, we are going to use them to generate the raw data for
    # each of the agencies based on the report the app user selects. 
    
    dod_url_raw_data <- httr::GET(str_c("https://api.usaspending.gov/api/v2/agency/097/", endpoint, "/"))
    doa_url_raw_data <- httr::GET(str_c("https://api.usaspending.gov/api/v2/agency/012/", endpoint, "/"))
    gsa_url_raw_data <- httr::GET(str_c("https://api.usaspending.gov/api/v2/agency/047/", endpoint, "/"))
  
  
    # Next we will parse the raw data for each of the agencies to create separate data frames 
    # that will be used to generate tibbles for use in merging the usable portions of the 
    #data frames together. 
    
    dod_url_parsed <- jsonlite::fromJSON(rawToChar(dod_url_raw_data$content))
    doa_url_parsed <- jsonlite::fromJSON(rawToChar(doa_url_raw_data$content))
    gsa_url_parsed <- jsonlite::fromJSON(rawToChar(gsa_url_raw_data$content))
  
  
  # Use conditional logic to convert the parsed data to tibble frames for merging and generating 
  # graphic summaries that make the most sense for the different data tables. 
  
    
    #  The first financial report we're going to read in is "budgetary_resources"     
    if(endpoint %in% "budgetary_resources") {
      df <- as_tibble(dod_url_parsed$agency_data_by_year) |>
        dplyr::select(fiscal_year:total_budgetary_resources) |>
        rename("agency_budget" = agency_budgetary_resources,
               "agency_obligations" = agency_total_obligated) |>
        mutate("agency_code" = "097", .before = 1)       
    
      df$agency_code <- as.numeric(df$agency_code)      
    
      dod_df <- agency_key |>
        dplyr::select(agency_name, agency_code) |>
        right_join(df, join_by(agency_code))  
    
      dod_df
    
      df <- as_tibble(doa_url_parsed$agency_data_by_year) |>
        dplyr::select(fiscal_year:total_budgetary_resources) |>
        rename("agency_budget" = agency_budgetary_resources,
               "agency_obligations" = agency_total_obligated) |>
        mutate("agency_code" = "012", .before = 1)  
    
      df$agency_code <- as.numeric(df$agency_code)      
    
      doa_df <- agency_key |>
        dplyr::select(agency_name, agency_code) |>
        right_join(df, join_by(agency_code))  
    
      doa_df
    
      df <- as_tibble(gsa_url_parsed$agency_data_by_year) |>
        dplyr::select(fiscal_year:total_budgetary_resources) |>
        rename("agency_budget" = agency_budgetary_resources,
               "agency_obligations" = agency_total_obligated) |>
        mutate("agency_code" = "047", .before = 1)  
    
      df$agency_code <- as.numeric(df$agency_code)      
    
      gsa_df <- agency_key |>
        dplyr::select(agency_name, agency_code) |>
        right_join(df, join_by(agency_code))  
    
      gsa_df
    
      combined_1 <- dod_df |>
        full_join(doa_df)
    
      combined_df <<- combined_1 |>
        full_join(gsa_df)
    
      print(combined_df)
    
      combined_df_point <- ggplot(data = combined_df, aes(x = fiscal_year, y = agency_budget, color = agency_name))
      
      combined_plot1 <<- combined_df_point +
        geom_point(aes(y = agency_budget, shape = "Budget")) +
        geom_point(aes(y = agency_obligations, shape = "Obligations")) +
        scale_color_discrete(name = "Agency") +
        scale_shape_discrete(name = "Budget / Obligation") +
        scale_y_continuous(labels=dollar) +
        labs(title="Agency Budgets vs Obligations", x = "Fiscal Year", y = "$ Spent by Agency") 
    
      print(combined_plot1)
    
      combined_df_box <- ggplot(data = combined_df, aes(x = fiscal_year, y = agency_budget))
    
      combined_plot2 <<- combined_df_box +
        geom_boxplot(aes(group=fiscal_year, fill="#CC0000")) +
        scale_y_continuous(name="$ Amount Spent", label=dollar) +
        scale_x_continuous(name="Budget Year") +
        labs(title="$ Amount Spent per Budget Year") +
        guides(fill = "none") 
    
      print(combined_plot2)
    
    
    } #####  <- end of "budgetary_resources" 
  
    #  The next financial report we're going to read in is "federal_account"
    else if(endpoint %in% "federal_account") {
      df <- as_tibble(dod_url_parsed$results) |>
        dplyr::select(code, name, obligated_amount, gross_outlay_amount) |>
        mutate("agency_code" = "097", .before = 1)
    
      df$agency_code <- as.numeric(df$agency_code)      
    
      dod_df <- agency_key |>
        dplyr::select(agency_name, agency_code) |>
        right_join(df, join_by(agency_code))  
    
      dod_df
    
      df <- as_tibble(doa_url_parsed$results) |>
        dplyr::select(code, name, obligated_amount, gross_outlay_amount) |>
        mutate("agency_code" = "012", .before = 1)
    
      df$agency_code <- as.numeric(df$agency_code)      
    
      doa_df <- agency_key |>
        dplyr::select(agency_name, agency_code) |>
        right_join(df, join_by(agency_code))  
    
      doa_df
    
      df <- as_tibble(gsa_url_parsed$results) |>
        dplyr::select(code, name, obligated_amount, gross_outlay_amount) |>
        mutate("agency_code" = "047", .before = 1)
    
      df$agency_code <- as.numeric(df$agency_code)      
    
      gsa_df <- agency_key |>
        dplyr::select(agency_name, agency_code) |>
        right_join(df, join_by(agency_code))  
    
      gsa_df
    
      combined_1 <- dod_df |>
        full_join(doa_df)
    
      combined_df <<- combined_1 |>
        full_join(gsa_df)
    
      print(combined_df)
    
      combined_df_col <- ggplot(combined_df, aes(x=agency_name, y=obligated_amount))
    
      combined_plot1 <<- combined_df_col +
        geom_col(aes(fill=agency_name)) +
        theme(axis.text.x = element_blank()) +
        scale_y_continuous(labels=dollar) +
        scale_fill_discrete(name="Agency") +
        labs(title="$ Amount Obligated by Program Code", x="Program", y="$ Amount Obligated")+
        facet_wrap(~code, labeller = labeller(name = label_wrap_gen(width = 15))) 
    
      print(combined_plot1)
    
      combined_df_plot <- (ggplot(combined_df, aes(shape=agency_name))) #, aes(x=obligated_amount, y=agency_name)))
    
      combined_plot2 <<- combined_df_plot +
        geom_qq(aes(sample=obligated_amount)) +
        scale_y_continuous(labels=dollar) +
        scale_shape_discrete(name="Agency") +
        labs(title="QQ Plot of $ Amount Obligated by Agency", x="Theoretical Data", y="$ Amount Spent") 
    
      print(combined_plot2)
    
    } #####  <- end of "federal_account" 
  
    #  The next financial report we're going to read in is "object_class"
    else if(endpoint %in% "object_class") {
      df <- as_tibble(dod_url_parsed$results) |>
        rename("obligation_name" = name) |>
        mutate("agency_code" = "097", .before = 1)
    
      df$agency_code <- as.numeric(df$agency_code)
    
      dod_df <- agency_key |>
        dplyr::select(agency_name, agency_code) |>
        right_join(df, join_by(agency_code)) 
    
      dod_df
    
      df <- as_tibble(doa_url_parsed$results) |>
        rename("obligation_name" = name) |>
        mutate("agency_code" = "012", .before = 1)
    
      df$agency_code <- as.numeric(df$agency_code)
    
      doa_df <- agency_key |>
        dplyr::select(agency_name, agency_code) |>
        right_join(df, join_by(agency_code))
    
      doa_df
    
      df <- as_tibble(gsa_url_parsed$results) |>
        rename("obligation_name" = name) |>
        mutate("agency_code" = "047", .before = 1)
    
      df$agency_code <- as.numeric(df$agency_code)
    
      gsa_df <- agency_key |>
        dplyr::select(agency_name, agency_code) |>
        right_join(df, join_by(agency_code))
    
      gsa_df
    
      combined_1 <- dod_df |>
        full_join(doa_df)
    
      combined_df <<- combined_1 |>
        full_join(gsa_df)
    
      print(combined_df)
    
      combined_df_tile <- ggplot(combined_df, aes(x=agency_name, y=obligation_name, fill=obligated_amount))
    
      combined_plot1 <<- combined_df_tile +
        geom_tile() +
        scale_fill_distiller(labels=comma) +
        labs(title="Award Recepients by Agency and Amount", x="Agency", y="Award Obligation", fill="$ Obligated") +
        theme(axis.text.x = element_blank()) 
    
      print(combined_plot1)
    
      combined_df_tree<- ggplot(combined_df, aes(area=obligated_amount, fill = agency_name, label=obligation_name, subgroup=obligation_name)) 
    
      combined_plot2 <<- combined_df_tree+
        geom_treemap(layout="squarified") +
        geom_treemap_text(place="center", size=12) +
        labs(title="Award Recepients by Agency") +
        scale_fill_discrete(name="Agency")
    
      print(combined_plot2)
    
    
    } #####  <- end of "object_class" 
  
    #  The next financial report we're going to read in is "obligations_by_award_category"
    else if(endpoint %in% "obligations_by_award_category") {
      df <- as_tibble(dod_url_parsed$results) |>
        mutate("agency_code" = "097", .before = 1)  
    
      df$agency_code <- as.numeric(df$agency_code)
    
      dod_df <- agency_key |>
        dplyr::select(agency_name, agency_code) |>
        right_join(df, join_by(agency_code)) 
  
      dod_df
    
      df <- as_tibble(doa_url_parsed$results) |>
        mutate("agency_code" = "012", .before = 1)  
    
      df$agency_code <- as.numeric(df$agency_code)
    
      doa_df <- agency_key |>
        dplyr::select(agency_name, agency_code) |>
        right_join(df, join_by(agency_code)) 
    
      doa_df
    
      df <- as_tibble(gsa_url_parsed$results) |>
        mutate("agency_code" = "047", .before = 1)  
    
      df$agency_code <- as.numeric(df$agency_code)
    
      gsa_df <- agency_key |>
        dplyr::select(agency_name, agency_code) |>
        right_join(df, join_by(agency_code)) 
    
      gsa_df
    
      combined_1 <- dod_df |>
        full_join(doa_df)
    
      combined_df <<- combined_1 |>
        full_join(gsa_df)
    
      print(combined_df)
    
      combined_df_col <- ggplot(combined_df, aes(x=category , y=aggregated_amount))
    
      combined_plot1 <<- combined_df_col +
        geom_col(aes(fill=agency_name)) +
        scale_x_discrete(guide=guide_axis(n.dodge=2)) +
        scale_y_continuous(labels=dollar) +
        scale_fill_discrete(name="Agency") +
        labs(title="Awards by Type and Agency", x="Award Type", y="$ Amount Awarded") 
    
      print(combined_plot1)
    
      combined_df_box <- ggplot(combined_df, aes(x=category, y=aggregated_amount))
    
      combined_plot2 <<- combined_df_box +
        geom_boxplot(fill="#CC0000") +
        scale_x_discrete(guide=guide_axis(n.dodge=2)) +
        scale_y_continuous(labels=dollar) +
        labs(title="Agency Awards by Type and Range", x="Award Type", y="$ Amount Awarded")
    
      print(combined_plot2)
    
    } #####  <- end of "obligations_by_award_category"
  
    #  The final financial report we're going to read in is "program_activity"
    else if(endpoint %in% "program_activity") {
      df <- as_tibble(dod_url_parsed$results) |>
        mutate("agency_code" = "097", .before = 1) |>
        mutate(name = str_replace(name, "N/A", "UNDISCLOSED"))        
    
      df$agency_code <- as.numeric(df$agency_code)      
    
      dod_df <- agency_key |>
        dplyr::select(agency_name, agency_code) |>
        right_join(df, join_by(agency_code))  
    
      dod_df
    
      df <- as_tibble(doa_url_parsed$results) |>
        mutate("agency_code" = "012", .before = 1) |>
        mutate(name = str_replace(name, "N/A", "UNDISCLOSED"))        
    
      df$agency_code <- as.numeric(df$agency_code)      
    
      doa_df <- agency_key |>
        dplyr::select(agency_name, agency_code) |>
        right_join(df, join_by(agency_code))  
    
      doa_df
    
      df <- as_tibble(gsa_url_parsed$results) |>
        mutate("agency_code" = "047", .before = 1) |>
        mutate(name = str_replace(name, "N/A", "UNDISCLOSED"))        
    
      df$agency_code <- as.numeric(df$agency_code)      
    
      gsa_df <- agency_key |>
        dplyr::select(agency_name, agency_code) |>
        right_join(df, join_by(agency_code))  
    
      gsa_df
    
      combined_1 <- dod_df |>
        full_join(doa_df)
    
      combined_df <<- combined_1 |>
        full_join(gsa_df)
    
      print(combined_df)
    
      combined_df_facet <- ggplot(data = combined_df, aes(x = name, y = gross_outlay_amount))
    
      combined_plot1 <<- combined_df_facet +
        geom_point(aes(gross_outlay_amount, color = "Budgeted")) +
        geom_point(aes(obligated_amount, color = "Obligated")) +
        scale_y_continuous(labels = dollar, guide=guide_axis(check.overlap=TRUE))+
        labs(title="Program Budgets vs Obligations", x = "Program", y = "$ Amount Spent") +
        theme(axis.text.x = element_blank())+
        theme(strip.text = element_text(size = 5)) +
        scale_color_manual("Budgeted / Obligated", 
                           breaks = c("Budgeted", "Obligated"), 
                           values = c("blue", "#CC0000")) +
        facet_wrap(~name, labeller = labeller(name = label_wrap_gen(width = 15)))
    
      print(combined_plot1)
    
      combined_df_point <- ggplot(data = combined_df, aes(x = gross_outlay_amount, y =name , shape = agency_name))
    
      combined_plot2 <<- combined_df_point +
        geom_point() +
        scale_shape_discrete(name = "Agency") +
        scale_x_continuous(labels=dollar) +
        theme(axis.text.y = element_blank()) +  
        labs(title="$ Amount Spent on Programs per Agency", x ="$ Amount Spent" , y ="Program" ) +
        coord_cartesian(xlim = c(0, 175000000000))
    
      print(combined_plot2)
    
    } #####  <- end of "obligations_by_award_category"

  } ########## End of `combined_agency_spending` function ##########


############### Start of ShinyDashboard server function ###############


    server <- function(input, output, session) {


########## "Data Download" tab ##########
  
  #  Output the "Awards Granted per Agency" plot, slider, and data table for display on
  #  the "Download" tab.     
    
    output$table1 <- DT::renderDT(agency_key, options = list(pageLength = 3, lengthMenu = c(3, 6, 10), scrollX = TRUE)
                                    )
    output$plot1 <- renderPlot({
      agency_key_plot <- ggplot(agency_key, aes(x = agency_name, y = total_awards, fill = agency_name))
      
      agency_key_plot +
        geom_col() +
        scale_fill_discrete(name = "Agency") +
        theme(axis.text.x = element_blank()) +  
        labs(title = "Awards Granted per Agency", x = "Agency", y = "# of Awards Granted") +
        ylim(0, input$slider)
      
    })
    
    
  # Output the data table and plot based on user selections of "Federal Department" and "Financial Area"
  # user selected inputs
  # Use the `govt_spending` function to generate data table `endpoint_df` based on the user selected input, 
  # for `agency_name` and `report`, then download the data table to a ".csv" file. 
    
    
    
    update <- observe({
      
      dept <- c("Department of Defense", "Department of Agriculture", "General Services Administration", "Department of Housing and Urban Development", "Department of Veterans Affairs", "Social Security Administration", "Department of Health and Human Services", "Federal Communications Commission", "Small Business Administration", "Railroad Retirement Board")
      
      report <- c("Budgetary Resources", "Federal Account", "Obligation Type", "Award Obligations", "Program Activity")
      
      updateSelectInput(session, "dept", choices = dept, selected = "Department of Defense")
      
      updateSelectInput(session, "report_area1", choices = report, selected = "Program Activity")
      
      govt_spending(input$dept, input$report_area1)
      })
    
    output$table2 <- DT::renderDT(endpoint_df, options = list(pageLength = 5, lengthMenu = c(2, 5, 10), scrollX = TRUE))        

    output$downloadData1 <- downloadHandler(
      filename = "Department Financial Report.csv",
      content = function(file) {
        write.csv(endpoint_df, file, row.names = FALSE)
      }
    )

########## End of "Data Download" tab ##########
    
    
########## "Exploration" tab ##########

  # Generate the report data set that combines the information from the three agencies with the highest total number of 
  # awards rendered. Each report type has pre-selected data summaries based on the data available. . 

    
#    "Budgetary Resources", "Federal Account", "Obligation Type", "Award Obligations", "Program Activity"
    
    update2 <- observe({
      
      report <- c("Budgetary Resources", "Federal Account", "Obligation Type", "Award Obligations", "Program Activity")
      
      updateSelectizeInput(session, "report_area2", choices = report, selected = "Obligation Type")
      
      combined_agency_spending(input$report_area2)
    })    
    
    output$table3 <- DT::renderDT(combined_df, options=list(pageLenght=5, lengthMenu=c(3, 5, 10), scrollX=TRUE))

    output$downloadData2 <- downloadHandler(
      filename = "Combined Financial Report.csv",
      content = function(file) {
        write.csv(endpoint_df, file, row.names = FALSE)
      }
    )
  
    output$plot2 <- renderPlot(combined_plot1)
    
    output$plot3 <- renderPlot(combined_plot2)

########## End of "Exploration" tab ##########

    

  } ########## <- End of ShinyDashboard server function 
