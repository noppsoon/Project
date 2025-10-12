library(readr)
library(tidyverse)
library(dplyr)
library(shiny)
library(bslib)
library(leaflet)
library(ggplot2)


data <- read_csv("AusStage_S12025PE2v2.csv")

glimpse(data)

colnames(data) <- c("event_identifier", "event_name", "first_year", "last_year", "primary_genre", "venue_identifier", "venue_name", "suburb", "latitude", "longitude")

min_year <- min(data$first_year)
max_year <- max(data$first_year)

# Define UI for application
ui <- fixedPage(
    
  # Create layout of each section
  ## https://shiny.posit.co/r/layouts/arrange/
  ## https://yards.albert-rapp.de/shiny-applications
  ## https://seankross.com/developing-data-products/shiny.html
  ## https://educationshinyappteam.github.io/Style_Guide/htmlCSS.html
  ## https://collinn.github.io/teaching/2023/labs/shiny_style.html 
  
  h2("Spotlight on Melbourne: Events Through Time",
     style = "font-weight: 600; font-family: 'Helvetica'; font-size: 32px; margin-bottom: 25px;"),
  
    fixedRow(
      column(3,
             # Create numberic slider
             sliderInput(
               inputId = "year_range", 
               label = "Select Year Range:",
               min = min_year,
               max = max_year,
               value = c(min_year, max_year),
               sep = ""  
             )
      ),
    
     column(9,
            
      # Show Map
      
      leafletOutput("map", height = "320px", width = "840px"), 
      br(),
      strong("Map Description: "),
      p("This map visualizes the distribution of event venues across Melbourne, filtered by year and categorized by primary genre. Each circle marker displays a genre-specific event at a venue. The circle’s size reflects the frequency of events, while color corresponds to the event’s genre. When a year range between 1995 and 2025 is selected, the map updates to display the events that occurred around Melbournewithin that time frame. During this period, the most frequent event’s genre was Theatre - Spoken Word, illustrated as orange circles. The largest orange circle marks the La Mama venue, which hosted 713 events of this genre. Detailed information shows when clicking on the circle marker."),
     )
    ),
    
    fixedRow(
      br(),
      br(),
      column(7,
             
        h4("Top 10 most commonly used venues of events",
          style = "font-weight: 600; font-family: 'Helvetica'; font-size: 22px; margin-bottom: 10px;"),
            
        # Show Graph
        plotOutput("plot", height = "450px", width = "640px"),
    
      ),
      
      column(5,
      
        strong(p("Graph Description:", style ="margin-bottom: 10px")),
        p("The graph demonstrates the 10 venues with the highest number of events, categorized by their primary genre. The horizontal axis illustates the name of vanues hosting the events, while the vertical axis illustrates the number of events.Each bar is segmented by color to represent different genres of events. La Mama is the most frequently used venue, largely hosting Theatre - Spoken Word events. This genre dominates the programming in most venues. However, venues such as Comedy Theatre, Her Majesty’s theater and the State Theatre display greater genre diversity, with Music theatre forming a largest share of their events."),
        tags$style(HTML("
          #data_source {
            font-family: 'Helvetica';
            font-size: 12px;
            }
          ")),
            
        strong("Data source information"),
        verbatimTextOutput("data_source")
      )
  )
)


