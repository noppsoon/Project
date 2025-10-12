# Color palette for categorized genre
## https://rstudio.github.io/leaflet/articles/markers.html
genre_palette <- colorFactor(
  palette = "Set1",                 
  domain = unique(data$primary_genre)  
)

# Define server for application
server <- function(input, output, session) {
  
  # Create layout of each section
  ## https://shiny.posit.co/r/layouts/arrange/
  ## https://yards.albert-rapp.de/shiny-applications
  ## https://jtr13.github.io/cc19/shiny.html
  observe({
    updateSelectInput(session, "year_range", choices = unique(data$first_year))
  })
  
  # Prepare data for graph visualization
  ## https://bookdown.org/yih_huynh/Guide-to-R-Book/basic-data-management.html
  ## https://dplyr.tidyverse.org/reference/group_by.html 
  ## https://dplyr.tidyverse.org/reference/summarise.html
  top_venues <- reactive({
    data %>%
      group_by(venue_name) %>%
      summarise(event_count = n_distinct(event_identifier)) %>%
      arrange(desc(event_count)) %>%
      top_n(10, event_count)
  })
  
  # Prepare data for breakdown in graph visualization
  ## https://bookdown.org/yih_huynh/Guide-to-R-Book/basic-data-management.html
  ## https://dplyr.tidyverse.org/reference/group_by.html 
  ## https://dplyr.tidyverse.org/reference/summarise.html
  genre_breakdown <- reactive({
    data %>%
      filter(venue_name %in% top_venues()$venue_name) %>%
      group_by(venue_name, primary_genre) %>%
      summarise(event_count = n_distinct(event_identifier))
  })
  
  # Prepare data for map visulization (filter, grouping, summarise, join)
  ## https://bookdown.org/yih_huynh/Guide-to-R-Book/basic-data-management.html
  ## https://dplyr.tidyverse.org/reference/group_by.html 
  ## https://dplyr.tidyverse.org/reference/summarise.html
  ## https://r4ds.hadley.nz/joins.html 
  filtered_data <- reactive({
    req(input$year_range)
    
    data %>%
      filter(first_year >= input$year_range[1],
             first_year <= input$year_range[2]) %>%
      group_by(venue_name, primary_genre) %>%
      summarise(event_count = n_distinct(event_identifier), .groups = "drop") %>%
      inner_join(
        data %>%
          filter(first_year >= input$year_range[1],
                 first_year <= input$year_range[2]) %>%
          select(venue_name, primary_genre, latitude, longitude, suburb) %>%
          distinct(),
        by = c("venue_name", "primary_genre")
      )
  })
  
  # Show the map
  ## https://rstudio.github.io/leaflet/articles/shiny.html
  ## https://rstudio.github.io/leaflet/articles/markers.html
  ## https://gis.stackexchange.com/questions/474407/r-leaflet-map-set-minimum-radius-size
  output$map <- renderLeaflet({
  leaflet(data = filtered_data()) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 144.962, lat = -37.8162, zoom = 12) %>%
      addCircleMarkers(~longitude, 
               ~latitude,
               ~pmax(sqrt(event_count) * 2, 2),
               color = ~genre_palette(primary_genre),
               stroke = FALSE,
               fillOpacity = 0.4,
               weight = 1,
               popup = ~paste(
                 "Venue: ", venue_name, "<br>",
                 "Suburb: ", suburb, "<br>",
                 "Primary Genre: ", primary_genre, "<br>",
                 "Event Count: ", event_count, "<br>"
               )) %>%
      #https://rstudio.github.io/leaflet/articles/legends.html
      addLegend(
        "bottomleft",
        pal = genre_palette,
        values = ~primary_genre,
        title = "Primary Genre",
        opacity = 1
      )
      })
  
  # Show graph
  output$plot <- renderPlot({
    
    # Compute prepared breakdown data for each venue to arrange data
    ## https://bookdown.org/yih_huynh/Guide-to-R-Book/basic-data-management.html
    ## https://dplyr.tidyverse.org/reference/group_by.html 
    ## https://dplyr.tidyverse.org/reference/summarise.html
    breakdown <- genre_breakdown()
    venue_order <- breakdown %>%
      group_by(venue_name) %>%
      summarise(total_event = sum(event_count), .groups = "drop") %>%
      arrange(desc(total_event)) %>%
      pull(venue_name)
    
    # Change to factor and order venue_name
    ## https://r-graph-gallery.com/267-reorder-a-variable-in-ggplot2.html
    breakdown$venue_name <- factor(breakdown$venue_name, levels = venue_order)
    
    # Plot stacked bar chart from prepared data
    ## https://ggplot2.tidyverse.org/reference/scale_brewer.html
    ## https://bookdown.org/aschmi11/RESMHandbook/data-visualization-with-ggplot.html
    ggplot(breakdown, aes(x = venue_name, y = event_count, fill = primary_genre)) +
      geom_bar(stat = "identity") +
      labs(x = "Venue Name", 
           y = "Number of Events") +
      theme(axis.text.x = element_text(angle = 35, hjust = 0.9)) +
      scale_fill_brewer(palette = "Set1")
  })
  
  # Display about datasource
  ## https://shiny.posit.co/r/components/outputs/text/
  output$data_source <- renderText({
    paste(
      "a) Name of the data: AusStage online resource",
      "b) URL to the data: https://www.ausstage.edu.au/pages/learn/about",
      "c) Licensor: AusStage",
      "d) Date of version: February 27, 2025",
      sep = "\n"
    )
  })
  
}


# Calls apps
shinyApp(ui = ui, server = server)

