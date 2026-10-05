install.packages("shiny")
install.packages("tidyverse")
install.packages("bslib")
install.packages("leaflet")
install.packages("plotly")
install.packages("sf")
install.packages("terra")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")

library(shiny)
library(tidyverse)
library(dplyr)
library(leaflet)
library(plotly)
library(bslib)
library(ggplot2)
library(sf)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)

data_for_plot <- read_csv("data_for_plot_adjusted.csv")
world <- ne_countries(scale = "medium", returnclass = "sf")
# https://cran.r-project.org/web/packages/rnaturalearth/vignettes/rnaturalearth.html

ui <- page_fluid(
  # https://shiny.posit.co/r/reference/shiny/0.14/fluidpage.html
  
  # Application title
  titlePanel("The Economic and Environment Impacts of Deforestation"),
  
  
  tags$style(HTML("
  # https://shiny.posit.co/r/articles/build/css/
  h2 {
    margin-top: 0px !important;
  }
  .container-fluid {
    padding-top: 0px !important;
  }
  
  .card-body {
    overflow-y: visible !important;
  }
  
   .tab-content {
    overflow: visible !important;
  }
  
   .selectize-dropdown {
    z-index: 99999 !important;
   }
  
  .selectize-dropdown-content {
    max-height: 100px !important;
    overflow-y: auto !important;
  }

  .selectize-input {
    z-index: 9999 !important;
    position: relative !important;
  }

  .form-select {
    z-index: 999 !important;
  }

  .col-sm-4 {
    overflow: visible !important;
    position: relative !important;
  }
  
  .card, .card-body, .card-header {
    overflow: visible !important;
  }

  .col-sm-4 {
    overflow-y: visible !important;
  }
  
  .desc-text {
    font-size: 13px;
    color: #555;
  }
")),
  
  
  card(
    
    # Generate template with the page navigation bar
    navset_card_pill(
    # https://shiny.posit.co/r/layouts/tabs/#card-with-a-pill-tabset
    # https://shiny.posit.co/r/components/inputs/select-single/
      # The first page template
      nav_panel("Tutorial",
                card(
                  card_header("Tutorial for the visualization"),
                  card(
                    card_body(
                      # Description text
                      
                      tags$div(
                        style = "font-size: 15px; line-height: 1.6;",
                        HTML("
          <p><b>Welcome to the <span style='color:#2a6ebb;'>The Economic and Environment Impacts of Deforestation Explorer</span>.</b><br>
          This interactive dashboard allows you to explore how deforestation affects both the <b>environment</b> and the <b>economy</b> across countries, regions, and income groups from <b>2000 to 2020</b>.</p>

          <ul>
            <li><b>Track trends over time</b> using interactive line graphs</li>
            <li><b>Compare areas globally</b> with choropleth maps and bar charts</li>
            <li><b>Examine relationships</b> between variables with scatter and bubble plots</li>
            <li><b>Filter data</b> by year, region, income group, and selected variables</li>
          </ul>

          <p>Start by selecting a topic from the tabs above, then customise your view using dropdowns and sliders.<br>
          Use this tool to discover key patterns and inequalities in forest-related impacts.</p>
                          ")
                      )
                    ),
                    # Containing tutorial image
                    card(imageOutput("image"))
                    
                  )
                ),
                
                id = "tab"
      ),
      
      # The second page template
      nav_panel("Feature Trends Over Time",
                fluidRow(
                  column(
                    width = 8,
                    card(plotlyOutput("linePlotFactors", height = "300px"),
                         card(plotlyOutput("linePlotAll", height = "300px")
                         ))
                    # https://shiny.posit.co/r/components/outputs/plot-plotly/
                  ),
                  column(
                    width = 4,
                    card(
                      # Dropdown to Select Aggregated Method
                      selectInput("agg_level_b", "Aggregate by:",
                                  choices = c("Region" = "region_wb", 
                                              "Income Group" = "income_grp") ),
                      
                      # Dropdown to Select Feature
                      selectInput("feature_b", "Feature to show:",
                                  choices = c("Forest Area" = "forest_area",
                                              "CO2 from Deforestation" = "co2_flux_deforest",
                                              "GDP Agri/Forestry" = "gdp_agri_forestry_year")),
                      
                      # Slider for Selecting Year Range
                      # https://shiny.posit.co/r/components/inputs/slider-range/
                      sliderInput("year_range_b", "Select Year Range:",
                                  min = 2000, max = 2020,
                                  value = c(2000, 2020),
                                  step = 1,
                                  sep = "",
                                  ticks = TRUE,
                                  timeFormat = "%Y"),
                    ),
                    card(
                      card_header("Description"),
                      card_body(tags$div(class = "desc-text",
                                         "The top chart presents a comparative view of four global factors over two decades. Each line represents a key indicator, helping readers explore how forest area, carbon emissions, agricultural GDP share, and temperature anomalies have varied year by year. The visual allows for cross-factor comparisons and supports interpretation of long-term patterns without needing to rely on raw values."
                      )),
                      card_body(tags$div(class = "desc-text",
                                         "The bottom chart compares the selected feature across world regions over time. It helps identify which regions have consistently higher or lower values, and how regional patterns have evolved.
You can change the feature using the dropdown to explore other environmental or economic indicators."
                      ))
                    )
                    
                  )
                )),
      
      # The third page template
      nav_panel("Global Overview", 
                
                fluidRow(
                  
                  # Left Section
                  column(
                    width = 8,
                  # Containing Choropleth Map
                    card(
                      card_header("Choropleth Map of Selected Variable"),  
                      leafletOutput("mapPlot", height = "400px"),
                      verbatimTextOutput("click_info")
                    ),
                    
                    # Containing Description
                    card(
                      fluidRow(
                        column(
                          width = 6,
                          card_body(tags$div(class = "desc-text",
                                             "This page offers a global overview of how a selected feature, such as forest area, CO₂ emissions, or GDP from agriculture and forestry, varies across the world in a specific year. The choropleth map visualises this feature geographically using colour intensity, helping users identify which countries, regions, or income groups have relatively high or low values. Beneath the map, a bar chart ranks the top five aggregated groups, using the same colour scale with the choropleth map for consistency and intuitive comparison. To the right, a line chart shows the long-term global trend of the chosen feature, offering historical context for observed patterns. Users can adjust the feature, aggregation level, and year using the interactive controls to explore how environmental and economic indicators differ across time and place."
                          ))
                        ),
                        
                        # Containing Bar Chart
                        column(
                          width = 6,
                          plotlyOutput("barPlot", height = "350px")
                        )
                      )
                    )
                    
                  ),
                  
                  # Right Section
                  column(
                    width = 4,
                  # Containing Line chart
                    card(plotlyOutput("linePlot", height = "250px")),
                  
                  # Containing filter features
                    card(
                      
                      # Dropdown to Select Aggregated method
                      selectInput("agg_level_a", "Aggregate by:",
                                  choices = c( 
                                    "Country" = "country",
                                    "Region" = "region_wb", 
                                    "Income Group" = "income_grp"
                                  ) ),
                      
                      # Dropdown to Select Feature
                      selectInput("feature_a", "Feature to show:",
                                  choices = c("Forest Area" = "forest_area",
                                              "CO2 from Deforestation" = "co2_flux_deforest",
                                              "GDP Agri/Forestry" = "gdp_agri_forestry_year")),
                      
                      # Slider for Selecting Year
                      # https://shiny.posit.co/r/components/inputs/slider/
                      sliderInput("year_a", "Year:", 
                                  min = 2000, 
                                  max = 2020, 
                                  value = 2020)
                    )
                  )
                )
      ),
      
      # The fourth page template
      nav_panel("Variable Relationship Explorer", 
                
                # Left section
                fluidRow(
                  column(
                    width = 8,
                    
                    # Containing Scatter Plot 
                    card(plotlyOutput("scatterPlot", height = "400px")),
                    fluidRow(
                      
                      # Contain left description
                      column(
                        width = 6,
                        card_body(tags$div(class = "desc-text",
                                           "This page enables users to explore the relationships between any two variables across countries. The main chart displays either a scatter or bubble plot, depending on whether a bubble size variable is selected. Users can examine how environmental and economic indicators—such as forest area, CO₂ emissions, or agriculture-related GDP—correlate with one another."
                        ))),
                      # Contain right description
                      column(
                        width = 6,
                        card_body(tags$div(class = "desc-text",
                                           "Colour represents either region or income group, while bubble size adds an extra dimension, such as land area or economic magnitude. Filters on the right allow users to adjust the year, choose variables for each axis, and control how the data is grouped and displayed. This view helps reveal clusters, outliers, and possible associations across different country types."
                        ))
                      ))
                  ),
                  
                  # Right Section
                  column(
                    width = 4,
                    # Containing filter features
                    card(
                      conditionalPanel(
                        # Hide the year slider when x or y variable is year
                        # https://tomsing1.github.io/blog/posts/conditional_shiny_widgets/index.html
                        condition = "input.x_var != 'year' && input.y_var != 'year'",
                        # Slider for Selecting Year
                        # https://shiny.posit.co/r/components/inputs/slider/
                        card(sliderInput("year_c", "Select Year:", min = 2000, max = 2020, value = 2020))),
                      
                      card(
                        # Dropdown to Select X-axis Variable
                        selectInput("x_var", "Select X-axis Variable:",
                                    choices = c("Forest Area" = "forest_area_adjusted",
                                                "CO2 from Deforestation" = "co2_flux_deforest_adjusted",
                                                "GDP Agri/Forestry" = "gdp_agri_forestry_year_adjusted",
                                                "Year" = "year")),
                        
                        # Dropdown to Select Y-axis Variable
                        selectInput("y_var", "Select Y-axis Variable:",
                                    choices = c("Forest Area" = "forest_area_adjusted",
                                                "CO2 from Deforestation" = "co2_flux_deforest_adjusted",
                                                "GDP Agri/Forestry" = "gdp_agri_forestry_year_adjusted",
                                                "Year" = "year"),
                                    selected = "co2_flux_deforest_adjusted"),
                        
                        # Dropdown to Select Size of Bubble 
                        selectInput("size_var", "Select Bubble Size Variable:",
                                    choices = c("None" = "none",
                                                "Forest Area" = "forest_area_adjusted",
                                                "CO2 from Deforestation" = "co2_flux_deforest_adjusted",
                                                "GDP Agri/Forestry" = "gdp_agri_forestry_year_adjusted"))
                      ),
                      
                      
                      card(
                        
                        # Dropdown to Select Aggregated method
                        selectInput("color_by", "Aggregate by:",
                                    choices = c("Region" = "region_wb", "Income Group" = "income_grp"),
                                    selected = "region_wb"),
                        
                        # Filter by Region
                        selectInput("filter_region", "Filter by Region:",
                                    choices = c("All", sort(unique(na.omit(world$region_wb)))),
                                    selected = "All"),
                        
                        # Filter by Income Group
                        selectInput("filter_income", "Filter by Income Group:",
                                    choices = c("All", sort(unique(na.omit(world$income_grp)))),
                                    selected = "All")
                      )
                    )
                  )
                )
      ),
      
      
    )
  )
)

# Server logic
server <- function(input, output) {
  
  # Render a static image
  output$image <- renderImage(
    #https://shiny.posit.co/r/reference/shiny/0.14/renderimage.html
    { 
      list(
        # Image file
        src = "tutorial_img.png",  
        contentType = "image/png",
        width = "100%",               
        alt = "Tutorial Summary"
      )
    }, 
    deleteFile = FALSE 
  ) 
  
  # Prepare data to long pattern
  long_data <- reactive({
    data_for_plot %>%
      pivot_longer(
        # https://tidyr.tidyverse.org/reference/pivot_longer.html
        cols = c(forest_area, gdp_agri_forestry_year, 
                 co2_flux_deforest,
                 temperature_anomaly, 
                 forest_area_adjusted,
                 gdp_agri_forestry_year_adjusted, 
                 co2_flux_deforest_adjusted),
        names_to = "feature",
        values_to = "value"
      ) %>%
      # Join with the geographic data to plot chorophleth map
      left_join(
        world %>% st_drop_geometry() %>% select(name, region_wb, income_grp),
        by = c("country" = "name")
      )
  })
  
  
  # Page 1 -------------------------------------------------------------------------------------------------
  
  # Render line chart showing trends of key factors over selected years
  output$linePlotFactors <- renderPlotly({
    req(input$year_range_b)
    
    # Set the text to present on the label
    feature_label_map <- c(
      "forest_area_adjusted" = "Forest Area (million sq. km)",
      "co2_flux_deforest_adjusted" = "CO₂ Net Flux from Deforestation (×100 tonnes CO₂e)",
      "gdp_agri_forestry_year_adjusted" = "Agriculture, forestry, and fishing, value added (×10% of GDP)",
      "temperature_anomaly" = "Temperature Anomaly (°C)"
    )
    
    
    df <- long_data() %>%
      
      # Filter the received input value and remove NA
      filter(
        year >= input$year_range_b[1],
        year <= input$year_range_b[2],
        feature %in% c("forest_area_adjusted", "co2_flux_deforest_adjusted", "gdp_agri_forestry_year_adjusted", "temperature_anomaly"),
        !is.na(value),
        !is.na(income_grp),
        income_grp != "NA"
      ) %>%
      
      # Compute global average per year and feature
      group_by(year, feature) %>%
      summarise(global_avg = mean(value, na.rm = TRUE), .groups = "drop" ) %>%
      # Add the appropriate labels and tooltip platform for plotly
      mutate(
        feature_label = feature_label_map[feature],
        tooltip_text = paste0(
          "Feature: ", feature_label_map[feature], 
          "<br>Year: ", year, 
          "<br>Value: ", round(global_avg, 2))
      ) %>%
      # Set the order of label
      mutate(
        feature_label = factor(
          feature_label,
          levels = unname(feature_label_map)
        )
      )
    
    # Create a line chart, setting the X-axis, Y-axis, color grouping, and custom tooltip
    p <- ggplot(df, 
                aes(x = year, y = global_avg, 
                    color = feature_label, 
                    group = feature_label, 
                    text = tooltip_text)) +
      
      # Draw line chart 
      geom_line(linewidth = 0.6) +
      
      # Add points for each data value on the line
      geom_point(size = 1.0) +
      
      # Assign colors to each feature, and name the legend
      scale_color_manual(
        values = c(
          "Forest Area (million sq. km)" = "forestgreen",
          "CO₂ Net Flux from Deforestation (×100 tonnes CO₂e)" = "darkorange",
          "Agriculture, forestry, and fishing, value added (×10% of GDP)" = "gold",
          "Temperature Anomaly (°C)" = "firebrick"
        ),
        name = "Global Factor"
      ) +
      
      # Set name of chart and axis
      labs(
        title = "Global Trends of All Factors Over Time",
        x = "Year",
        y = "Units"
      ) +
      
      # Apply minimal theme and center the plot title
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12, hjust = 0.5))
    
    # Convert to interactive plotly chart with custom tooltips and hover mode
    # https://plotly-r.com/controlling-tooltips
    ggplotly(p, tooltip = "text") %>%
      layout(hovermode = "closest")
  })
  
  # Render line chart showing trends of selected factors aggregated by region, income group over selected year range
  # https://www.rdocumentation.org/packages/plotly/versions/4.10.4/topics/plotly-shiny
  output$linePlotAll <- renderPlotly({
    
    
    df <- long_data() %>%
      
      # Filter the received input value and remove NA
      filter(
        feature == input$feature_b,
        year >= input$year_range_b[1],
        year <= input$year_range_b[2],
        !is.na(value),
        !is.na(.data[[input$agg_level_b]]),
        income_grp != "NA"
      ) %>%
      # Compute global average per year and feature
      group_by(year, !!sym(input$agg_level_b)) %>%
      summarise(value_avg = mean(value, na.rm = TRUE), .groups = "drop") %>%
      # Add the appropriate labels and tooltip platform for plotly
      mutate(
        group_label = .data[[input$agg_level_b]],
        tooltip_text = paste0(group_label, "<br>Year: ", year, "<br>Value: ", round(value_avg, 2))
      )
    
    # Define custom colors for each region
    custom_region_colors <- c(
      "East Asia & Pacific" = "#f65353",
      "Europe & Central Asia" = "#ff9797",
      "Latin America & Caribbean" = "#ffd300",
      "Middle East & North Africa" = "#4cb04c",
      "North America" = "#007aff",
      "South Asia" = "#03468f",
      "Sub-Saharan Africa" = "#9c59fd"
    )
    
    # Convert feature names to readable labels
    feature_label <- function(var) {
      label_map <- c(
        "forest_area" = "Forest Area",
        "co2_flux_deforest" = "CO2 from Deforestation",
        "gdp_agri_forestry_year" = "GDP Agri/Forestry",
        "temperature_anomaly" = "Temperature Anomaly",
        "forest_area_adjusted" = "Forest Area",
        "co2_flux_deforest_adjusted" = "CO2 from Deforestation",
        "gdp_agri_forestry_year_adjusted" = "GDP Agri/Forestry",
        "year" = "Year"
      )
      # If not found, return the original variable name
      return(label_map[[var]] %||% var)  # 
    }
    
    
    # Convert variable names to readable labels for display
    aggregate_group_label <- function(var) {
      label_map <- c(
        "region_wb" = "Region",
        "income_grp" = "Income Group",
        "country" = "Country"
      )
      # If not found, return the original variable name
      return(label_map[[var]] %||% var)
    }
    
    # Initialize an empty Plotly object
    # https://community.plotly.com/t/incorporate-a-plotly-graph-into-a-shiny-app/5329/2
    # https://plotly.com/r/hover-text-and-formatting/
    # https://r-graph-gallery.com/customize-plotly-tooltip.html
    p <- plot_ly()
    # Loop through each unique group label in the dataset
    for (grp in unique(df$group_label)) {
      # Filter the dataset to include only the current group
      subdf <- df %>% filter(group_label == grp)
      
      # Add a trace for the current group with custom appearance and tooltip
      p <- add_trace(
        p,
        data = subdf,
        x = ~year,
        y = ~value_avg,
        type = 'scatter',
        mode = 'lines+markers',
        name = grp,
        text = ~tooltip_text,
        hoverinfo = 'text',
        line = list(color = custom_region_colors[grp]),
        marker = list(color = custom_region_colors[grp]),
        key = grp
      )
    }
    
    p %>%
      
      # Generate the plot title based on selected feature and aggregation level
      layout(
        
        title = list(
          text = paste("Trend of", feature_label(input$feature_b), 
                       "by", aggregate_group_label(input$agg_level_b)),
          # Set the appearance of title
          x = 0.05,
          xanchor = "left",
          font = list(size = 16)
        ),
        
        # Set X-axis and Y-axis label
        yaxis = list(title = feature_label(input$feature_b)), 
        xaxis = list(title = "Year"),
        
        # Enable hover interaction (nearest data point)
        hovermode = "closest",
        
        # Double-click toggles individual trace visibility
        legend = list(itemclick = "toggleothers", 
                      itemdoubleclick = "toggle")
      ) %>%
      
      highlight(
        on = "plotly_hover",
        off = "plotly_doubleclick",
        opacityDim = 0.1,
        persistent = FALSE
      )
  })
  
  # Page 2 --------------------------------------------------------------------------------------------------------
  
  # Render the Leaflet map for spatial data display
  output$mapPlot <- renderLeaflet({
    
    # Filter the dataset for the selected year and feature
    df <- long_data() %>%
      filter(year == input$year_a, feature == input$feature_a)
    
    # Aggregate data and join with map geometry based on the selected aggregation level
    # https://shiny.posit.co/r/reference/shiny/1.7.0/conditionalpanel.html
    if (input$agg_level_a == "country") {
      
      # Group by country and calculate average value
      summary_df <- df %>%
        group_by(country) %>%
        summarise(value_avg = mean(value, na.rm = TRUE), .groups = "drop")
      
      # Join summarized data with spatial geometry using country name
      plot_data <- world %>%
        left_join(summary_df, by = c("name" = "country")) %>%
        mutate(agg_label = name)
      
    } else if (input$agg_level_a == "region_wb") {
      
      # Group by region and calculate average value
      summary_df <- df %>%
        group_by(region_wb) %>%
        summarise(value_avg = mean(value, na.rm = TRUE), .groups = "drop")
      
      # Join with spatial geometry using region
      plot_data <- world %>%
        left_join(summary_df, by = "region_wb") %>%
        mutate(agg_label = region_wb)
      
    } else {
      
      # Group by income group and calculate average value
      summary_df <- df %>%
        group_by(income_grp) %>%
        summarise(value_avg = mean(value, na.rm = TRUE), .groups = "drop")
      
      # Join with spatial geometry using income group
      plot_data <- world %>%
        left_join(summary_df, by = "income_grp") %>%
        mutate(agg_label = income_grp)
    }
    
    # Define tooltip message
    plot_data <- plot_data %>%
      mutate(hover_label = paste0(
        "<strong>", agg_label, "</strong><br/>",
        "Feature: ", input$feature_a, "<br/>",
        "Value: ", round(value_avg, 2), "<br/>",
        "Year: ", input$year_a
      ))
    
    # Define a color palette for the choropleth map based on the selected feature
    pal <- switch(
      # https://www.rdocumentation.org/packages/dichromat/versions/1.1/topics/colorRampPalette
      # https://rstudio.github.io/leaflet/reference/colorNumeric.html
      input$feature_a,
      "forest_area" = colorNumeric(colorRampPalette(c("#d9f0d3", "#006400"))(100), domain = plot_data$value_avg, na.color = "gray"),
      "gdp_agri_forestry_year" = colorNumeric(colorRampPalette(c("#fff7bc", "#f7b801"))(100), domain = plot_data$value_avg, na.color = "gray"),
      "co2_flux_deforest" = colorNumeric(colorRampPalette(c("#ffd9af", "#dd5900"))(100), domain = plot_data$value_avg, na.color = "gray")
    )
    
    # Convert feature names appropriate labels
    feature_label <- function(var) {
      label_map <- c(
        "forest_area" = "Forest Area",
        "co2_flux_deforest" = "CO2 from Deforestation",
        "gdp_agri_forestry_year" = "GDP Agri/Forestry"
      )
      return(label_map[[var]] %||% var) 
    }
    
    # Custom tooltip text for each polygon on the map
    plot_data <- plot_data %>%
      mutate(hover_label = paste0(
        "<strong>", agg_label, "</strong><br/>",
        "Feature: ", feature_label(input$feature_a), "<br/>",
        "Value: ", round(value_avg, 2), "<br/>",
        "Year: ", input$year_a
      ))
    
    # Initialize a Leaflet map using the spatial dataset (plot_data)
    leaflet(plot_data) %>%
      # https://rstudio.github.io/leaflet/articles/shiny.html
      addProviderTiles("CartoDB.Positron") %>%
      # Set the initial view of the map
      setView(lng = 20, lat = 10, zoom = 1.5) %>%
      
      # Add polygons to the map using spatial geometry and colored by value
      addPolygons(
        fillColor = ~pal(value_avg),
        weight = 1,
        opacity = 1,
        color = "black",
        dashArray = "1",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(weight = 2, color = "#dd5900", fillOpacity = 0.9),
        label = lapply(plot_data$hover_label, HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          direction = "auto"
        )
      ) %>%
      
      # Add a color legend to explain the data range
      addLegend(
        pal = pal, 
        values = ~value_avg,
        opacity = 0.7, 
        title = "value_avg",
        position = "bottomright",
        na.label = "No data"
      )
  })
  
  
  # Render supporting line plot
  output$linePlot <- renderPlotly({
    
    # Create a trend dataset from the main data_for_plot table
    trend_df <- data_for_plot %>%
      # Compute global average per year
      group_by(year) %>%
      summarise(value_avg = mean(.data[[input$feature_a]], na.rm = TRUE), 
                .groups = "drop") %>%
      # Add the appropriate labels and tooltip platform for plotly
      mutate(
        tooltip_text = paste0(
          "Year: ", year,
          "<br>Value: ", format(value_avg, big.mark = ",")
        )
      )
    
    
    line_color <- switch(
      input$feature_a,
      "forest_area" = "#006400",
      "forest_area_adjusted" = "#006400",
      
      "gdp_agri_forestry_year" = "#f9b801",
      "gdp_agri_forestry_year_adjusted" = "#f9b801",
      
      "co2_flux_deforest" = "#ff5900",
      "co2_flux_deforest_adjusted" = "#ff5900",
      
      "temperature_anomaly" = "#990000",
      
      "gray"  # fallback
    )
    
    # Convert feature names to readable labels 
    feature_label <- function(var) {
      label_map <- c(
        "forest_area" = "Forest Area",
        "co2_flux_deforest" = "CO2 from Deforestation",
        "gdp_agri_forestry_year" = "GDP Agri/Forestry",
        "temperature_anomaly" = "Temperature Anomaly",
        "forest_area_adjusted" = "Forest Area",
        "co2_flux_deforest_adjusted" = "CO2 from Deforestation",
        "gdp_agri_forestry_year_adjusted" = "GDP Agri/Forestry",
        "year" = "Year"
      )
      return(label_map[[var]] %||% var) 
    }
    
    # Create a line chart showing the global average of the selected feature per year
    p <- ggplot(trend_df, aes(x = year, y = value_avg, text = tooltip_text, group = 1)) +
      # Draw the trend line with specified color and thickness
      geom_line(linewidth = 1, color = line_color) +
      # Add points for each year
      geom_point(color = line_color) +
      # Set chart title and axis labels using dynamic feature label
      labs(title = paste("Global Average of", feature_label(input$feature_a), "per Year"),
           x = "Year", y = feature_label(input$feature_a)) +
      
      # Apply theme styling 
      theme(
        text = element_text(family = "system-ui"),
        axis.text.x = element_text(size = 10),  
        axis.text.y = element_text(size = 10),                     
        axis.title = element_text(size = 11),                     
        plot.title = element_text(size = 12, hjust = 0.5)
      )
    
    # Convert the ggplot chart to an interactive Plotly chart with custom tooltips
    ggplotly(p, tooltip = "text")
  })
  
 # Render supporting bar chart 
  output$barPlot <- renderPlotly({
    
    # Filter the dataset for the selected year and feature
    df <- long_data() %>%
      filter(year == input$year_a, feature == input$feature_a)
    
    # Compute summary statistics based on user-selected aggregation level
    summary_df <- switch(
      input$agg_level_a,
      "country" = df %>% group_by(country) %>% summarise(value_avg = mean(value, na.rm = TRUE), .groups = "drop") %>% rename(agg_label = country),
      "region_wb" = df %>% group_by(region_wb) %>% summarise(value_avg = mean(value, na.rm = TRUE), .groups = "drop") %>% rename(agg_label = region_wb),
      "income_grp" = df %>% group_by(income_grp) %>% summarise(value_avg = mean(value, na.rm = TRUE), .groups = "drop") %>% rename(agg_label = income_grp)
    )
    
    # Extract the top 5 groups with the highest average values
    top5 <- summary_df %>%
      filter(!is.na(agg_label)) %>%
      arrange(desc(value_avg)) %>%
      slice_head(n = 5) %>%
      # Generate tooltip labels
      mutate(
        label_text = paste0(agg_label, "<br>Value: ", format(value_avg, big.mark = ","))
      )
    
    # Convert feature names to readable labels
    aggregate_group_label <- function(var) {
      label_map <- c(
        "region_wb" = "Region",
        "income_grp" = "Income Group",
        "country" = "Country"
      )
      return(label_map[[var]] %||% var)
    }
    
    # Define a custom color palette based on the selected feature 
    pal <- switch(
      input$feature_a,
      "forest_area" = colorNumeric(colorRampPalette(c("#d9f0d3", "#006400"))(100), domain = top5$value_avg, na.color = "gray"),
      "forest_area_adjusted" = colorNumeric(colorRampPalette(c("#d9f0d3", "#006400"))(100), domain = top5$value_avg, na.color = "gray"),
      "gdp_agri_forestry_year" = colorNumeric(colorRampPalette(c("#fff7bc", "#f7b801"))(100), domain = top5$value_avg, na.color = "gray"),
      "gdp_agri_forestry_year_adjusted" = colorNumeric(colorRampPalette(c("#fff7bc", "#f7b801"))(100), domain = top5$value_avg, na.color = "gray"),
      "co2_flux_deforest" = colorNumeric(colorRampPalette(c("#ffd9af", "#dd5900"))(100), domain = top5$value_avg, na.color = "gray"),
      "co2_flux_deforest_adjusted" = colorNumeric(colorRampPalette(c("#ffd9af", "#dd5900"))(100), domain = top5$value_avg, na.color = "gray"),
      colorNumeric(c("gray"), domain = top5$value_avg)  # fallback
    )
    
    # Create a bar chart showing the top 5 groups with the highest average values
    # https://plotly.com/ggplot2/bar-charts/
    p <- ggplot(top5, aes(x = reorder(agg_label, value_avg), y = value_avg,
                          fill = value_avg,
                          text = label_text)) +
      geom_col() +
      # Apply a custom color gradient based on value range
      scale_fill_gradientn(
        colours = pal(seq(min(top5$value_avg, na.rm = TRUE),
                          max(top5$value_avg, na.rm = TRUE),
                          length.out = 100))) +
      # Set axis labels and chart title
      labs(
        x = NULL,
        y = "Value",
        title = paste("Top 10", aggregate_group_label(input$agg_level_a), "by", input$feature_a)
      ) +
      # Apply minimal theme and adjust text styling
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 12, hjust = 0.5)
      )
    
    # Convert to interactive plotly chart with custom tooltips
    ggplotly(p, tooltip = "text")
  })
  

  # Page 3 ------------------------------------------------------------------------------------------
  
  output$scatterPlot <- renderPlotly({
    req(input$x_var, input$y_var)
    
    # Aggregate and reshape data to enable multivariate comparison by country and year
    df <- long_data() %>%
      group_by(country, year, feature, region_wb, income_grp) %>%  
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = feature, values_from = value)
    
    
    # Filter by year only if year is not used as X or Y
    if (!(input$x_var == "year" || input$y_var == "year")) {
      df <- df %>% filter(year == input$year_c)
    }
    
    # Apply region filter if selected
    if (!is.null(input$filter_region) && input$filter_region != "All") {
      df <- df %>% filter(region_wb == input$filter_region)
    }
    
    # Apply income group filter if selected
    if (!is.null(input$filter_income) && input$filter_income != "All") {
      df <- df %>% filter(income_grp == input$filter_income)
    }
    
    # Drop missing value of selected X and Y values
    df <- df %>%
      filter(!is.na(.data[[input$x_var]]),
             !is.na(.data[[input$y_var]]))
    
    # Define color to depend on region or income group
    color_var <- input$color_by
    
    # Convert the selected color variable to character 
    df[[color_var]] <- as.character(df[[color_var]])
    
    # Remove rows with missing values
    df <- df %>% filter(!is.na(.data[[color_var]]))
    
    
    # Define custom colors for each region
    custom_region_colors <- c(
      "East Asia & Pacific" = "#f65353",
      "Europe & Central Asia" = "#ff9797",
      "Latin America & Caribbean" = "#ffd300",
      "Middle East & North Africa" = "#4cb04c",
      "North America" = "#007aff",
      "South Asia" = "#03468f",
      "Sub-Saharan Africa" = "#9c59fd"
    )
    
    # Define custom colors for each income group
    custom_income_colors <- c(
      "1. High income: OECD" = "#377EB8",
      "2. High income: nonOECD" = "#FF7F00",
      "3. Upper middle income" = "#4DAF4A",
      "4. Lower middle income" = "#E41A1C",
      "5. Low income" = "#984EA3"
    )
    
    # Convert feature names to readable labels
    feature_label <- function(var) {
      label_map <- c(
        "forest_area_adjusted" = "Forest Area",
        "co2_flux_deforest_adjusted" = "CO2 from Deforestation",
        "gdp_agri_forestry_year_adjusted" = "GDP Agri/Forestry",
        "year" = "Year"
      )
      return(label_map[[var]])
    }
    
    # Compute the size of each point based on the selected variable
    if (!is.null(input$size_var) && input$size_var != "none") {
      
      # Extract raw size values from the selected variable
      raw_size <- df[[input$size_var]]
      
      # Normalize size values to the range [0, 1] to avoid distortion
      size_norm <- (raw_size - min(raw_size, na.rm = TRUE)) /
        (max(raw_size, na.rm = TRUE) - min(raw_size, na.rm = TRUE) + 1e-9)
      
      # Scale normalized values to the appropriate size range
      size_vector <- 6 + size_norm * 60 
    } else {
      # Use a constant default size if no size variable is selected
      size_vector <- rep(8, nrow(df)) 
    }
    
    # Render an interactive scatter plot/bubble chart
    plot_ly(
      # https://plotly.com/r/bubble-charts/
      # https://plotly-r.com/controlling-tooltips
      # https://mastering-shiny.org/action-graphics.html
      # https://r-graph-gallery.com/customize-plotly-tooltip.html
      
      data = df,
      x = ~.data[[input$x_var]],
      y = ~.data[[input$y_var]],
      type = "scatter",
      mode = "markers",
      
      # Color points by selected grouping variable
      color = ~.data[[color_var]],
      colors = if (color_var == "region_wb") {
        custom_region_colors
      } else if (color_var == "income_grp") {
        custom_income_colors
      } else {
        "Set2"
      },
      
      # Tooltip content with dynamic grouping variable
      text = ~paste0("Country: ", country,
                     "<br>Year: ", year,
                     "<br>", tools::toTitleCase(gsub("_", " ", color_var)), ": ", .data[[color_var]]),
      
      # Set bubble size 
      marker = list(
        size = size_vector,
        sizemode = "diameter",
        sizemin = 2,
        opacity = 0.6
      ),
      hoverinfo = "text"
    ) %>%
      
      # Set chart title and axis labels
      layout(
        title = paste("Relationship Between Selected Variables"),
        xaxis = list(title = feature_label(input$x_var)),
        yaxis = list(title = feature_label(input$y_var))
      )
  })
  
}

# Run app
shinyApp(ui = ui, server = server)

