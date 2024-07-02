## Project 2
## Jason Pattison, ST 588-601 SUM I 2024

library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(scales)  

  server <- function(input, output) {
  
#  Create the background code for the data download tab. 
##  Create the `agency_key` data set that will be used to 
##  generate the ggplot and slider which will control the 
##  y-axis on the graphic on the download tab. 
    
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
    
    output$table1 <- renderDataTable(agency_key, options = list(pageLength = 5, lengthMenu = c(5, 10), scrollX = TRUE)
                                     )
    
    output$plot1 <- renderPlot({
      agency_key_plot <- ggplot(agency_key, aes(x = agency_name, y = total_awards, fill = agency_name))
      
    agency_key_plot +
      geom_col() +
      scale_fill_discrete(name = "Agency") +
      theme(axis.text.x = element_blank()) +  
      labs(title = "Awards Granted per Agency", x = "Agency", y = "# of Awards Granted")
    

    data_filter = reactive(agency_key[, 1:2])
    
    output$table2 <- renderDataTable({
      my_data = data_filter()
      DT::datatable(my_data, 
               extensions = "Buttons",
               options = list(paging = TRUE, 
                              scrollX = TRUE, 
                              searching = TRUE, 
                              ordering = TRUE, 
                              dom = 'l<"sep">Bfrtip',
                              buttons = c('copy', 'csv', 'excel', 'pdf'),
                              pageLength = 5,
                              lengthMenu = c(5, 10))
      )
      })
      
      output$filtered_row <- renderPrint({
        input[["table2_rows_all"]]
      })
      
      output$download_filtered <- downloadHandler(
        filename = "Filtered Data.csv",
        content = function(file) {
          write.csv(my_data[input[["table2_rows_all"]], ],
                    file)
        }
      )
    })
    
    
    

#  Generate the combined data set of all 10 Departments that will be used to cross-analyze their financials. 
##      
      
    }
