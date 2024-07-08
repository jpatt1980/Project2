## Project 2
## Jason Pattison, ST 588-601 SUM I 2024

library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(scales)
library(treemap)
library(RColorBrewer)


#####     Create the background code for the data download and exploration tabs     #####

###  Create the `agency_key` data set
###  This is will be used to generate a bar plot with a slider that will 
###  control the y-axis on the graphic on the download tab. `agency_key`
###  also ties into user created functions `govt_spending` and `combined_govt_spending`. 
  
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
  
  
### Generate `govt_spending` unction that will be used to generate department financial reports by type. 
  
  
govt_spending <-function(agency_name = "Department of Defense", report = "Program Activity") {
  
  #  First, we need to convert our agency name into it's associated code
  #  for use in pulling the differing data sets at the endpoints. 
  

###  VVV  Manually inputting agency_name parameter to make the below code work due to      VVV  ###
###  vvv  Error in conditional logic generating error associated with the usage of `%in%`  vvv  ###
###  vvv  "Error in match(x, table, nonmatch = 0L): 'match' requires vector arguments"     vvv  ###
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
###  ^^^  Error: conditional logic generates error associated with the usage of `%in%`  ^^^  ###
###  ^^^  "Error in match(x, table, nonmatch = 0L): 'match' requires vector arguments"  ^^^  ###
  
  
###  vvv  Error: conditional logic generates error associated with the usage of `%in%`  vvv  ###
###  vvv  "Error in match(x, table, nonmatch = 0L): 'match' requires vector arguments"  vvv  ###
#    if_else(agency_name %in% "Department of Defense", agency_code <- "097",
#      if_else(agency_name %in% "Department of Agriculture", agency_code <- "012",
#        if_else(agency_name %in% "General Services Administration", agency_code <- "047",
#          if_else(agency_name %in% "Department of Housing and Urban Development", agency_code <- "086",
#            if_else(agency_name %in% "Department of Veterans Affairs", agency_code <- "036", 
#              if_else(agency_name %in% "Social Security Administration", agency_code <- "028",
#                if_else(agency_name %in% "Department of Health and Human Services", agency_code <- "075",
#                  if_else(agency_name %in% "Federal Communications Commission", agency_code <- "027", 
#                    if_else(agency_name %in% "Small Business Administration", agency_code <- "073", 
#                      if_else(agency_name %in% "Railroad Retirement Board", agency_code = "060", print ("Error: Incorrect Agency #Selection")
#                              ))))))))))
###  ^^^  Error: conditional logic generates error associated with the usage of `%in%`  ^^^  ###
###  ^^^  "Error in match(x, table, nonmatch = 0L): 'match' requires vector arguments"  ^^^  ###
  
  
  #  Next we will generate conditional logic statements to determine the
  #  endpoint information that will be generated. 
  
  
###  VVV  Manually inputting report parameter to make the below code work due to           VVV  ###
###  vvv  Error in conditional logic generating error associated with the usage of `%in%`  vvv  ###
###  vvv  "Error in match(x, table, nonmatch = 0L): 'match' requires vector arguments"     vvv  ###
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
###  ^^^  Error: conditional logic generates error associated with the usage of `%in%`  ^^^  ###
###  ^^^  "Error in match(x, table, nonmatch = 0L): 'match' requires vector arguments"  ^^^  ###
  
###  vvv  Error: conditional logic generates error associated with the usage of `%in%`  vvv  ###
###  vvv  "Error in match(x, table, nonmatch = 0L): 'match' requires vector arguments"  vvv  ###
#  if_else(report %in% "Budgetary Resources", endpoint <- "budgetary_resources", 
#   if_else(report %in% "Federal Account", endpoint <- "federal_account", 
#      if_else(report %in% "Obligation Type", endpoint <- "object_class", 
#        if_else(report %in% "Award Obligations", endpoint <- "obligations_by_award_category", 
#          if_else(report %in% "Program Activity", endpoint <- "program_activity", print ("Error: Incorrect Report Selection"))
#        )))) 
###  ^^^  Error: conditional logic generates error associated with the usage of `%in%`  ^^^  ###
###  ^^^  "Error in match(x, table, nonmatch = 0L): 'match' requires vector arguments"  ^^^  ###
  
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
  #  using a `right_join` for each data set to merge the `agency_key` information
  #  to each of the report data tables. 
  
  
  #  The first data table we're going to read in is
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
    
  } else if(endpoint %in% "federal_account") {
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
    
  } else if(endpoint %in% "object_class") {
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
    
  } else if(endpoint %in% "obligations_by_award_category") {
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
    

  } else if(endpoint %in% "program_activity") {
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
    
  } 
  
} ### <-- End of `govt_spending` function. 


###  Generate combined data sets for report analysis across the top 3 award generating agencies

combined_agency_spending <- function(report = "Obligation Type") {
  
  if(report %in% "Budgetary Resources") {
    endpoint <- c("budgetary_resources")
  } else if(report %in% "Federal Account") {
    endpoint <- c("federal_account")
  } else if(report %in% "Obligation Type") {
    endpoint <- c("object_class")
  } else if(report %in% "Award Obligations") {
    endpoint <- c("obligations_by_award_category")
  } else if(report %in% "Program Activity") {
    endpoint <- c("program_activity")
  } else {
    print ("Error: Incorrect Report Selection")
  }
  
  # Generate raw data for the reports for each of the departments
  dod_url_raw_data <- httr::GET(str_c("https://api.usaspending.gov/api/v2/agency/097/", endpoint, "/"))
  doa_url_raw_data <- httr::GET(str_c("https://api.usaspending.gov/api/v2/agency/012/", endpoint, "/"))
  gsa_url_raw_data <- httr::GET(str_c("https://api.usaspending.gov/api/v2/agency/047/", endpoint, "/"))
  
  
  # Parse the raw data for the reports for each of the departments
  dod_url_parsed <- jsonlite::fromJSON(rawToChar(dod_url_raw_data$content))
  doa_url_parsed <- jsonlite::fromJSON(rawToChar(doa_url_raw_data$content))
  gsa_url_parsed <- jsonlite::fromJSON(rawToChar(gsa_url_raw_data$content))
  
  
  # Use conditional logic to convert the parsed data to tibble frames for
  # merging and generating graphic summaries
  
  
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
      guides(fill = FALSE) 
    
    print(combined_plot2)
    
    
  }  ###  <-- end of `Budgetary Resources` code
  
  
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
    
    
  }  ###  <-- end of `Federal Account` code
  
  
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
    
    combined_plot2 <<- treemap(combined_df, 
                              title="Award Recepients by Agency", 
                              index= c("agency_name", "obligation_name"),
                              vSize="obligated_amount",
                              type="index",
                              palette = "BuPu",
                              fontsize.labels = c(15, 12),
                              fontcolor.labels = c("#CC0000", "#000000"),
                              bg.labels = c("transparent"),
                              align.labels = list(
                                c("center", "center"),
                                c("center", "bottom")
                              ),
                              overlap.labels = 0.5,
                              inflate.labels = FALSE)
    
    print(combined_plot2)
    
    
  }  ###  <-- end of `Obligation Type` code
  
  
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
    
    
  }  ###  <-- end of `Award Obligations` code
  
  
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
    
    
  }  ###  <-- end of `Program Activity` code
}  ###  <-- end of `combined_agency_spending` function


##########     Start of ShinyDashboard server function     ##########


  server <- function(input, output, session) {
  
    
##  Data download    
  
## Output the plot and data table to the "Download" tab.     
    
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
    
    
    ## Output the data table and plot based on user selections of "Federal Department" and "Financial Area"
    
    gov_spend_params <- eventReactive(input$submit1, {
      input$dept
      input$report_area1
    })
    
    

    output$table2 <- DT::renderDT(endpoint_df, options = list(pageLength = 2, lengthMenu = c(2, 4, 10), scrollX = TRUE))

############################################################################################################
#   Attempted to model this off of an example from "mastering-shiny.org", received the error that follows it:  
#
#    output$table2 <- DT::renderDT({
#      agency_name <- reactive(input$dept)
#      report1 <- reactive(input$report_area1)
#      govt_spending(agency_name, report1)
#    }, options = list(pageLength = 2, lengthMenu = c(2, 4, 10), scrollX = TRUE))
#    
#      Warning: Error in match: 'match' requires vector arguments
#        106: %in%
#        105: govt_spending [C:\Users\jpatt\OneDrive\Desktop\558-601\Project2\Project2/server.R#50]
#        104: exprFunc [C:\Users\jpatt\OneDrive\Desktop\558-601\Project2\Project2/server.R#719]
#    
#    I've received this error multiple times, cannot seem to find a "fix" to it online. The Shiny website is
#    less than helpful when troubleshooting this. 
#    
###############################################################################################################
        
    output$downloadData1 <- downloadHandler(
      filename = "Department Financial Report.csv",
      content = function(file) {
        write.csv(endpoint_df, file, row.names = FALSE)
      }
    )
    
###  Generate code for the "Explore" tab. 

##  Generate the combined data set of all 10 Departments that will be used to cross-analyze their financials. 
    
    combined_agency_spending()  ### Using default values for the function while testing ways to get `input$` to work
    
    output$table3 <- DT::renderDT(combined_df, options=list(pageLenght=2, lengthMenu=c(3, 6, 10), scrollX=TRUE)
                                  )

    output$downloadData2 <- downloadHandler(
      filename = "Combined Financial Report.csv",
      content = function(file) {
        write.csv(endpoint_df, file, row.names = FALSE)
      }
    )
  
    output$plot2 <- renderPlot(combined_plot1)
    
    output$plot3 <- renderPlot(combined_plot2)

### <-- End of "Exploration" tab code

    
##########     End of ShinyDashboard server function     ##########   
  }
