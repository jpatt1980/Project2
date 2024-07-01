## Project 2
## Jason Pattison, ST 588-601 SUM I 2024

library(shiny)
library(shinydashboard)

  server <- function(input, output) {
    agency_key_raw <- httr::GET("https://api.usaspending.gov/api/v2/agency/awards/count")
    
    agency_key_parsed <- jsonlite::fromJSON(rawToChar(agency_key_raw$content))
    
    agency_key_df <- as_tibble(agency_key_parsed$results[[1]]) |>
      rename("agency_name" = awarding_toptier_agency_name,
             "agency_code" = awarding_toptier_agency_code,
             "indef_deliv_contract" = idvs) |>
      mutate(total_awards = contracts + direct_payments + grants + indef_deliv_contract + loans + other) |>
      arrange(desc(total_awards))
    
    
    agency_key_df$agency_code <- as.numeric(agency_key_df$agency_code)
    
    agency_key <- agency_key_df
    
    agency_key
    
    output$plot1 <- renderPlot({
      agency_key_plot <- ggplot(agency_key, aes(x = agency_name, y = total_awards, fill = agency_name))
      
      agency_key_plot +
        geom_col() +
        scale_fill_discrete(name = "Agency") +
        theme(axis.text.x = element_blank()) +  
        labs(x = "Agency", y = "# of Awards Granted")
    })
  }
