library(shiny)
library(leaflet)
library(leaflet.extras)
library(httr)
library(lubridate)
library(jsonlite)
library(dplyr)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(ggplot2)
library(ggiraph)
library(plotly)
library(stringr)
library(DT)
library(scales)
library(bslib)  # For theming the interface

source('tableau-in-shiny-v1.2.R')

# Pedestrian
# General function to fetch paginated data
fetch_paginated_data <- function(base_url, query_params, required_cols, id_col = NULL) {
  all_data <- list()
  offset <- 0
  limit <- 100
  
  repeat {
    # Update pagination parameters
    current_query <- c(query_params, list(limit = limit, offset = offset))
    
    # Build the complete URL
    url <- modify_url(base_url, query = current_query)
    
    # Send GET request
    response <- GET(url)
    
    # Check response status
    if (status_code(response) != 200) {
      stop(paste("Error fetching data from", base_url, "Status code:", status_code(response)))
    }
    
    # Parse JSON content
    data <- fromJSON(content(response, "text"), flatten = TRUE)
    
    # If no data, terminate the loop
    if (length(data) == 0) break
    
    # Convert to data frame
    data_df <- as.data.frame(data, stringsAsFactors = FALSE)
    
    # Ensure required columns exist
    missing_cols <- setdiff(required_cols, names(data_df))
    if (length(missing_cols) > 0) {
      data_df[missing_cols] <- NA
    }
    data_df <- data_df[required_cols]
    
    # If there is an ID column, ensure it is character type
    if (!is.null(id_col)) {
      data_df[[id_col]] <- as.character(data_df[[id_col]])
    }
    
    # Add to data list
    all_data <- append(all_data, list(data_df))
    
    # Update offset
    offset <- offset + limit
  }
  
  # Combine all paginated data
  if (length(all_data) == 0) {
    return(data.frame())
  }
  
  combined_data <- do.call(rbind, all_data)
  
  return(combined_data)
}

# Function to get map data
get_map_data <- function() {
  # Get current time and one hour ago
  current_time <- Sys.time()
  one_hour_ago <- current_time - hours(1)
  
  # Format to ISO 8601 format (UTC)
  one_hour_ago_str <- format(one_hour_ago, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  current_time_str <- format(current_time, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  
  # Define API and query parameters for traffic data
  traffic_base_url <- "https://data.melbourne.vic.gov.au/api/v2/catalog/datasets/pedestrian-counting-system-past-hour-counts-per-minute/exports/json"
  traffic_query <- list(
    where = paste0("sensing_datetime >= '", one_hour_ago_str, "' AND sensing_datetime <= '", current_time_str, "'"),
    select = "location_id,sensing_datetime,total_of_directions",
    order_by = "sensing_datetime DESC",
    timezone = "Australia/Melbourne"
  )
  traffic_required_cols <- c("location_id", "sensing_datetime", "total_of_directions")
  
  # Fetch traffic data
  traffic_data <- fetch_paginated_data(
    base_url = traffic_base_url,
    query_params = traffic_query,
    required_cols = traffic_required_cols,
    id_col = "location_id"
  )
  
  # Convert total_of_directions to numeric and handle possible NA values
  traffic_data$total_of_directions <- as.numeric(traffic_data$total_of_directions)
  
  # Define API and query parameters for sensor location data
  sensor_base_url <- "https://data.melbourne.vic.gov.au/api/v2/catalog/datasets/pedestrian-counting-system-sensor-locations/exports/json"
  sensor_query <- list()
  sensor_required_cols <- c("location_id", "latitude", "longitude")
  
  # Fetch sensor location data
  sensor_data <- fetch_paginated_data(
    base_url = sensor_base_url,
    query_params = sensor_query,
    required_cols = sensor_required_cols,
    id_col = "location_id"
  )
  
  # Rename columns and ensure location IDs are consistent
  sensor_data <- sensor_data %>%
    dplyr::rename(lat = latitude, lng = longitude)
  
  # Clean location_id (remove spaces, convert to lowercase)
  sensor_data$location_id <- trimws(tolower(sensor_data$location_id))
  traffic_data$location_id <- trimws(tolower(traffic_data$location_id))
  
  # Merge sensor location data with traffic data
  map_data <- merge(traffic_data, sensor_data, by = "location_id", all.x = TRUE)
  
  # Exclude records with missing location information
  map_data <- map_data[!is.na(map_data$lat) & !is.na(map_data$lng), ]
  
  return(map_data)
}

# Parking
# Data Loading and Processing Module
load_and_process_data <- function() {
  parking_data <- read.csv('off_street.csv', stringsAsFactors = FALSE)
  
  parking_data <- parking_data %>%
    rename(
      area = CLUE.small.area,
      parking_spaces = Parking.spaces,
      parking_type = Parking.type,
      property_id = Property.ID
    )
  
  total_parking_spaces <- sum(parking_data$parking_spaces, na.rm = TRUE)
  total_areas <- n_distinct(parking_data$area)
  total_parking_locations <- n_distinct(parking_data$property_id)
  
  list(
    parking_data = parking_data,
    total_parking_spaces = total_parking_spaces,
    total_areas = total_areas,
    total_parking_locations = total_parking_locations
  )
}

# UI Component Module: Filter Panel
ui_filter_panel <- function(areas) {
  div(class = "floating-filter-panel",
      h4("Filters"),
      pickerInput(
        inputId = "selected_areas",
        label = "Select Areas",
        choices = areas,
        selected = areas,
        multiple = TRUE,
        options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)
      )
  )
}

# UI Component Module: Introduction
ui_introduction <- function() {
  fluidRow(
    column(12,
           wellPanel(
             style = "background-color:#f7f7f7; border-left: 5px solid #428bca; margin-bottom: 20px;",
             h3("Introduction"),
             htmlOutput("introduction_text")
           )
    )
  )
}

# UI Component Module: Overview Metrics
ui_overview_metrics <- function() {
  fluidRow(
    column(width = 4, uiOutput("total_parking_spaces")),
    column(width = 4, uiOutput("total_parking_locations")),
    column(width = 4, uiOutput("total_areas"))
  )
}

# UI Component Module: Map Visualization
ui_map <- function() {
  fluidRow(
    column(12,
           wellPanel(
             style = "background-color:#fff; border: 1px solid #ddd; padding: 15px; margin-bottom: 20px;",
             h3("Parking Area Map"),
             tags$iframe(
               src = "https://public.tableau.com/views/Book1_17298376125330/parkingareamap?:showVizHome=no&:embed=true",
               style = "width:100%; height:600px; border:none;",
               frameborder = "0",
               scrolling = "no"
             )
           )
    )
  )
}

# UI Component Module: Charts
ui_plots <- function() {
  fluidRow(
    column(6,
           wellPanel(
             style = "background-color:#fff; border: 1px solid #ddd; padding: 15px; margin-bottom: 20px;",
             plotlyOutput("parking_type_pie", height = "400px")
           )
    ),
    column(6,
           wellPanel(
             style = "background-color:#fff; border: 1px solid #ddd; padding: 15px; margin-bottom: 20px;",
             girafeOutput('plot_parking', height = "400px")
           )
    )
  )
}

# Load and process data
data_list <- load_and_process_data()
parking_data <- data_list$parking_data
total_parking_spaces <- data_list$total_parking_spaces
total_areas <- data_list$total_areas
total_parking_locations <- data_list$total_parking_locations

pedestrian_tab <- tabPanel(
  title = "Melbourne Pedestrian Traffic",
  
  useShinyjs(), 
  
  titlePanel("Melbourne Pedestrian Traffic Heatmap - Last Hour"),
  
  # Add customisable content in the head
  tags$head(
    setUpTableauInShiny()
  ),
  
  # Introduction and Map
  fluidRow(
    column(4,
           div(class = "card", style = "padding: 30px; border: 1px solid #ddd; border-radius: 8px; background-color: #f8f9fa; font-size: 1.2em;",
               h3("Introduction"),
               p("This application visualizes the pedestrian traffic in Melbourne over the past hour."),
               p("The map displays various sensor locations as blue markers. Users need to find the Location ID from the map and input it into the Tableau dashboard below to view the pedestrian traffic distribution for the past 24 hours at the selected location."),
               
               h3("Usage Instructions"),
               tags$ol(
                 tags$li("Explore the heatmap to understand pedestrian traffic density."),
                 tags$li("Click on any blue marker to identify the corresponding Location ID."),
                 tags$li("The Tableau dashboard below the map will automatically update to reflect the selected sensor's data for the past 24 hours.")
               )
           )
    ),
    
    column(8,
           leafletOutput("hotspotMap", width = "100%", height = "600px") # 渲染 Leaflet 地图
    )
  ),
  
  # Embed tableau dashboard
  fluidRow(
    column(12,
           style = "height: 900px;",
           br(),
           # Use tableauPublicViz to embed Tableau
           tableauPublicViz(
             id = "tableauViz_pedestrian",
             url = "https://public.tableau.com/views/PedestrianCounting_17299110472950/Dashboard1?:showVizHome=no&:embed=true",
             height = "900px"
           )
    )
  )
)

# Navbar: Define UI
ui <- navbarPage(
  "Discover Melbourne: Your Guide to the City’s Best",
  
  # Add custom styles
  header = tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
    
      /* Navbar custom styling */
      .navbar-default {
        background-color: rgba(0, 0, 0, 0.8); /* Semi-transparent background */
        border-color: transparent;
      }
      
      /* Navbar title styling */
      .navbar-header .navbar-brand {
        display: none; /* Hide default brand placement */
      }
      
      /* Navbar link styling */
      .navbar-default .navbar-nav > li > a {
        color: #ddd;
        font-size: 18px;
        font-weight: 500;
        transition: color 0.3s;
      }
      
      /* Hover effect for navbar links */
      .navbar-default .navbar-nav > li > a:hover {
        color: #FFFFFF; /* Text-only hover effect */
        background-color: transparent;
      }
      
      /* Active link styling */
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:hover,
      .navbar-default .navbar-nav > .active > a:focus {
        color: #FFFFFF;
        background-color: transparent;
      }
      
      /* Adjust padding and margin in navbar */
      .navbar-nav > li > a {
        padding: 15px 20px;
        margin: 0 5px;
      }
      
      /* Full-page background styling without white borders */
      html, body{ 
        margin: 0;
        padding: 0;
      /* overflow: hidden;*/
      }
      
      .container-fluid, .navbarPage, .tab-content {
        overflow: visible;
      }
      
      /* Body padding to remove extra space */
      body {
        padding: 0 !important;
      }
      
      /* Navbar margin-bottom adjustment */
      .navbar {
        margin-bottom: 0 !important;
      }
      
      #home-page {
        display: flex;
        justify-content: center;
        align-items: center;
        height: 100vh;
        width: 100vw;
        background-image: url('https://upload.wikimedia.org/wikipedia/commons/9/97/CBD_Melbourne.jpg');
        background-size: cover;
        background-position: center;
        color: #fff;
        font-family: Arial, sans-serif;
        margin: 0;
      }
      
      #pedestrian-content {
        overflow-y: auto;
        height: calc(100vh - 70px); 
      }
      
      #parking-content {
        overflow-y: auto;
        height: calc(100vh - 70px); 
        padding-right: 270px; 
      }
      
      /* Fixed floating filter panel styles */
      .floating-filter-panel {
        position: fixed;
        top: 70px; 
        right: 20px;
        width: 250px;
        background-color: rgba(255, 255, 255, 0.9);
        padding: 15px;
        z-index: 1000;
        box-shadow: -2px 0 5px rgba(0,0,0,0.1);
        border-radius: 5px;
      }
      
      

      /* Overlay styling */
      #overlay {
        background-color: rgba(0, 0, 0, 0.6);
        padding: 40px;
        border-radius: 10px;
        text-align: center;
        max-width: 800px;
      }
      
      /* Title and description styling */
      #home-page h1 {
        font-size: 36px;
        margin-bottom: 20px;
        text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.7);
      }
      #home-page h2 {
        font-size: 24px;
        margin-bottom: 10px;
        text-shadow: 1px 1px 2px rgba(0, 0, 0, 0.7);
      }
      #home-page p {
        font-size: 18px;
        margin-bottom: 30px;
        text-shadow: 1px 1px 2px rgba(0, 0, 0, 0.7);
      }
      
      @media (max-width: 767px) {
        .navbar-default .navbar-nav > li > a {
        font-size: 14px;
        padding: 10px;
      }
  
      .navbar-header .navbar-brand {
        display: block;
      }
  
      .navbar-toggle {
        margin-right: 15px;
      }
      
      
    ")),
    
    tags$style(HTML("
      /* Fixed floating filter panel styles */
      .floating-filter-panel {
        position: fixed;
        top: 80px;
        right: 20px;
        width: 250px;
        background-color: #fff;
        padding: 15px;
        z-index: 1000;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        border: 1px solid #ddd;
        border-radius: 4px;
      }
      /* Beautify value boxes */
      .value-box {
        padding: 20px;
        color: #fff;
        border-radius: 4px;
        margin-bottom: 20px;
      }
      .value-box h3 {
        font-size: 36px;
        margin: 0;
        padding: 0;
      }
      .value-box p {
        font-size: 18px;
        margin: 0;
        padding: 0;
      }
    ")),
    
  ),
  
  
  # Home Page Tab
  tabPanel("Home",
           fluidPage(
             div(id = "home-page",
                 div(id = "overlay",
                     h1("Welcome to Melbourne"),
                     h2("Discover Melbourne: Your Guide to the City’s Best")
                 )
             )
           )
  ),
  
  # Define each content tab with matching names for `selected`
  pedestrian_tab,
  
  tabPanel("Parking Space", fluidPage(
    useShinyjs(),
    div(id = "parking-content",
        
        # Application Title
        titlePanel("Parking Spaces Analysis"),
        # Filter Panel
        ui_filter_panel(sort(unique(load_and_process_data()$parking_data$area))),
        # Main Content Area
        div(class = "content",
            ui_introduction(),
            ui_overview_metrics(),
            ui_map(),
            ui_plots()
        )
    ))),
  
  
  tabPanel("Free Support & Service", uiOutput("tableau_free_support_service")),
  
  tabPanel("Weather Overview", uiOutput("tableau_weather_overview")),
  tabPanel("Real-Time Weather", 
           fluidPage(
             fluidRow(
               column(12,
                      wellPanel(
                        style="margin-top: 50px",
                        h3("Introduction"),
                        p("In terms of the part of Weather Overview, it focuses on rainfall and temperature of Melbourne Weather."),
                        p("The Weather Overview section provides an interactive display of Melbourne's rainfall and temperature trends. 
                          Tourists can explore historical data on monthly average rainfall and temperature variations, view recent years' data trends, and gain insights into seasonal patterns. 
                          This interactive tool helps tourists better understand the impact of weather changes over time in Melbourne."),
                        p("On this page, the widget uses the OpenWeather API to display real-time weather data for Melbourne, including temperature, weather conditions (cloudy, rainy, sunny, etc.), wind speed, and forecasts for the upcoming week.")
                      )
               )
             ),
             uiOutput("weather_widget"),
             uiOutput("weather_data"),  # Add this to display weather data
             leafletOutput("realtime_map", height = "400px")  # Add Leaflet map output
           )
  )
  
)




# Define Server
server <- function(input, output, session) {
  
  # Pedestrian Counting
  # Fetch and prepare map data
  map_data <- tryCatch({
    withProgress(message = 'Fetching and processing data...', value = 0, {
      incProgress(0.3)
      data <- get_map_data()
      incProgress(0.7)
      data
    })
  }, error = function(e) {
    showNotification(paste("Error:", e$message), type = "error")
    NULL
  })
  
  # Render Leaflet map
  output$hotspotMap <- renderLeaflet({
    if (is.null(map_data)) {
      leaflet() %>% 
        addTiles() %>%
        setView(lng = 144.9631, lat = -37.8136, zoom = 15)
    } else {
      leaflet(map_data) %>% 
        addTiles() %>%
        addHeatmap(
          lng = ~lng, 
          lat = ~lat, 
          intensity = ~total_of_directions,
          blur = 20, 
          max = max(map_data$total_of_directions, na.rm = TRUE), 
          radius = 15
        ) %>%
        addCircleMarkers(
          lng = ~lng,
          lat = ~lat,
          radius = 5,
          color = "blue",
          stroke = FALSE,
          fillOpacity = 0.6,
          layerId = ~location_id,
          label = ~paste("Location ID:", location_id),
          clusterOptions = markerClusterOptions()  # Use marker clustering
        ) %>%
        setView(lng = 144.9631, lat = -37.8136, zoom = 15)
    }
  })
  
  
  # Parking
  # Update filter selection
  observe({
    updatePickerInput(session, "selected_areas",
                      choices = sort(unique(parking_data$area)),
                      selected = sort(unique(parking_data$area)))
  })
  
  # Overview Metrics
  output$total_parking_spaces <- renderUI({
    div(class = "value-box", style = "background-color: #1abc9c;",
        h3(formatC(total_parking_spaces, format = "d", big.mark = ",")),
        p("Total Parking Spaces")
    )
  })
  
  output$total_parking_locations <- renderUI({
    div(class = "value-box", style = "background-color: #3498db;",
        h3(formatC(total_parking_locations, format = "d", big.mark = ",")),
        p("Total Parking Locations")
    )
  })
  
  output$total_areas <- renderUI({
    div(class = "value-box", style = "background-color: #9b59b6;",
        h3(total_areas),
        p("Total Areas")
    )
  })
  
  # Render Introduction Text
  output$introduction_text <- renderUI({
    HTML("
      <p>Welcome to the Parking Spaces Analysis platform. Use the filters on the right to select specific areas. The dashboard will update accordingly to display parking space information for the selected areas.</p>
      <ul>
        <li><strong>Parking Area Map:</strong> Visualizes the locations of parking areas on a map.</li>
        <li><strong>Parking Type Distribution:</strong> Shows the distribution of different parking types in a pie chart.</li>
        <li><strong>Parking Spaces Histogram:</strong> Displays the total parking spaces by area in a bar chart.</li>
      </ul>
      <p>Clicking on the bar chart can also filter the map area. Interact with the charts and map for more detailed insights.</p>
    ")
  })
  
  # Filter data based on selected areas
  filtered_data <- reactive({
    parking_data %>%
      filter(area %in% input$selected_areas)
  })
  
  # Render Pie Chart
  output$parking_type_pie <- renderPlotly({
    parking_type_distribution <- filtered_data() %>%
      group_by(parking_type) %>%
      summarise(total_spaces = sum(parking_spaces, na.rm = TRUE))
    
    plot_ly(parking_type_distribution, labels = ~parking_type, values = ~total_spaces, type = 'pie') %>%
      layout(title = "Parking Types Distribution",
             margin = list(t = 50))
  })
  
  # Render Bar Chart
  output$plot_parking <- renderGirafe({
    parking_data_clean <- filtered_data() %>%
      group_by(area) %>%
      summarise(total_parking_spaces = sum(parking_spaces, na.rm = TRUE)) %>%
      arrange(desc(total_parking_spaces))
    
    p <- ggplot(parking_data_clean) +
      aes(
        x = area,
        y = total_parking_spaces,
        data_id = area,
        tooltip = paste0(
          "Area: ", area,
          "\nTotal Parking Spaces: ", total_parking_spaces
        )
      ) +
      geom_bar_interactive(
        stat = 'identity',
        width = 0.8,
        fill = '#2c3e50'
      ) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      labs(x = 'Area', y = 'Parking Spaces') +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.y = element_line(color = '#e2e2e2'),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 12)
      ) +
      ggtitle("Parking Spaces by Area")
    
    girafe(ggobj = p, height_svg = 5, width_svg = 7,
           options = list(
             opts_selection(
               type = "single", css = "stroke:red;stroke-width:2px;"
             ),
             opts_tooltip(css = "background-color:white;color:black;font-style:italic;padding:5px;border-radius:5px;")
           ))
  })
  
  # Initialize Tableau Visualization
  session$onFlushed(function() {
    runjs(sprintf('
      var vizDiv = document.getElementById("tableauViz");
      var vizUrl = "%s";
      var options = {
        hideTabs: true,
        onFirstInteractive: function () {
          window.viz = viz;
          Shiny.setInputValue("tableauReady", true);
        }
      };
      var viz = new tableau.Viz(vizDiv, vizUrl, options);
    ', 'https://public.tableau.com/views/Book1_17298376125330/parkingareamap'))
  })
  
  # Function to Update Filters in Tableau
  updateTableauFilters <- function() {
    selected_areas <- input$selected_areas
    selected_bar_area <- input$plot_parking_selected
    
    areas_js_array <- jsonlite::toJSON(selected_areas)
    
    # Apply Area Filters
    if (!is.null(selected_areas) && length(selected_areas) > 0) {
      runjs(sprintf('
        if (window.viz) {
          let sheet = window.viz.getWorkbook().getActiveSheet();
          sheet.applyFilterAsync("CLUE small area", %s, tableau.FilterUpdateType.REPLACE);
        }
      ', areas_js_array))
    } else {
      runjs('
        if (window.viz) {
          let sheet = window.viz.getWorkbook().getActiveSheet();
          sheet.clearFilterAsync("CLUE small area");
        }
      ')
    }
    
    # Further Filter if an Area is Selected in the Bar Chart
    if (!is.null(selected_bar_area) && length(selected_bar_area) > 0) {
      runjs(sprintf('
        if (window.viz) {
          let sheet = window.viz.getWorkbook().getActiveSheet();
          sheet.applyFilterAsync("CLUE small area", ["%s"], tableau.FilterUpdateType.REPLACE);
        }
      ', selected_bar_area))
    }
  }
  
  # Apply Initial Filters When Tableau Visualization is Ready
  observeEvent(input$tableauReady, {
    updateTableauFilters()
  })
  
  # Update Tableau Filters When Area Selection Changes
  observeEvent(input$selected_areas, {
    updateTableauFilters()
  })
  
  # Update Tableau Filters When Bar Chart Selection Changes
  observeEvent(input$plot_parking_selected, {
    updateTableauFilters()
  })
  
  observeEvent(input$hotspotMap_marker_click, {
    click <- input$hotspotMap_marker_click
    if (!is.null(click)) {
      location_id <- click$id
      runjs(sprintf('let viz = document.getElementById("tableauViz_pedestrian");
        let sheet = viz.workbook.activeSheet;
        sheet.applyFilterAsync("Location ID", ["%s"], FilterUpdateType.Replace);', location_id))
    }
  })
  
  # Navibar
  # Tableau embedding for service and weather sections
  
  output$tableau_free_support_service <- renderUI({
    HTML("<div class='tableauPlaceholder' id='viz1730025160819' style='position: relative'>
         <noscript>
         <a href='#'><img alt='Free Support and Service ' src='https:&#47;&#47;public.tableau.com&#47;static&#47;images&#47;1_&#47;1_17300251245350&#47;Sheet1&#47;1_rss.png' style='border: none' /></a>
         </noscript>
         <object class='tableauViz'  style='display:none;'><param name='host_url' value='https%3A%2F%2Fpublic.tableau.com%2F' /> <param name='embed_code_version' value='3' /> <param name='site_root' value='' /><param name='name' value='1_17300251245350&#47;Sheet1' />
         <param name='tabs' value='no' /><param name='toolbar' value='yes' /><param name='static_image' value='https:&#47;&#47;public.tableau.com&#47;static&#47;images&#47;1_&#47;1_17300251245350&#47;Sheet1&#47;1.png' /> <param name='animate_transition' value='yes' />
         <param name='display_static_image' value='yes' /><param name='display_spinner' value='yes' /><param name='display_overlay' value='yes' /><param name='display_count' value='yes' /><param name='language' value='en-US' /><param name='filter' value='publish=yes' />
         </object></div>                
         <script type='text/javascript'>                    
         var divElement = document.getElementById('viz1730025160819');                    
         var vizElement = divElement.getElementsByTagName('object')[0];                    
         vizElement.style.width='100%';vizElement.style.height=(divElement.offsetWidth*0.75)+'px';                    
         var scriptElement = document.createElement('script');                    
         scriptElement.src = 'https://public.tableau.com/javascripts/api/viz_v1.js';                    
         vizElement.parentNode.insertBefore(scriptElement, vizElement);                
         </script>")
  })
  
  output$tableau_weather_overview <- renderUI({
    HTML("
    <div style='display: flex; justify-content: center; align-items: flex-start; height: 80vh; overflow-y: auto; padding-top: 60px;'>
      <div class='tableauPlaceholder' id='viz1729997687220' style='position: relative; width: 100%; max-width: 1200px;'>
        <noscript>
          <a href='#'><img alt='Dashboard_Rainfall' src='https://public.tableau.com/static/images/IV/IV_A3_Weather_Percy_Yang/Dashboard_Rainfall/1_rss.png' style='border: none' /></a>
        </noscript>
        <object class='tableauViz' style='display: block; width: 100%; height: 100%;'>
          <param name='host_url' value='https%3A%2F%2Fpublic.tableau.com%2F' />
          <param name='embed_code_version' value='3' />
          <param name='site_root' value='' />
          <param name='name' value='IV_A3_Weather_Percy_Yang/Dashboard_Rainfall' />
          <param name='tabs' value='no' />
          <param name='toolbar' value='yes' />
          <param name='static_image' value='https://public.tableau.com/static/images/IV/IV_A3_Weather_Percy_Yang/Dashboard_Rainfall/1.png' />
          <param name='animate_transition' value='yes' />
          <param name='display_static_image' value='yes' />
          <param name='display_spinner' value='yes' />
          <param name='display_overlay' value='yes' />
          <param name='display_count' value='yes' />
          <param name='language' value='en-US' />
          <param name='filter' value='publish=yes' />
        </object>
      </div>
      <script type='text/javascript'>
        var divElement = document.getElementById('viz1729997687220');
        var vizElement = divElement.getElementsByTagName('object')[0];
        if ( divElement.offsetWidth > 800 ) { 
          vizElement.style.minWidth='1000px'; vizElement.style.maxWidth='1200px';
          vizElement.style.width='100%'; vizElement.style.minHeight='600px'; vizElement.style.maxHeight='800px';
          vizElement.style.height=(divElement.offsetWidth*0.75)+'px';
        } else if ( divElement.offsetWidth > 500 ) {
          vizElement.style.minWidth='1000px'; vizElement.style.maxWidth='1200px';
          vizElement.style.width='100%'; vizElement.style.minHeight='600px'; vizElement.style.maxHeight='800px';
          vizElement.style.height=(divElement.offsetWidth*0.75)+'px';
        } else {
          vizElement.style.width='100%'; vizElement.style.height='800px';
        }
        var scriptElement = document.createElement('script');
        scriptElement.src = 'https://public.tableau.com/javascripts/api/viz_v1.js';
        vizElement.parentNode.insertBefore(scriptElement, vizElement);
      </script>
    </div>
    ")
  })
  
  
  # OpenWeather Widget Embedding
  output$weather_widget <- renderUI({
    HTML("
    <div style='display: flex; justify-content: center; align-items: center; height: 100%; margin-top: 50px; margin-bottom: 20px'>
      <div id='openweathermap-widget-11'></div>
      <script src='//openweathermap.org/themes/openweathermap/assets/vendor/owm/js/d3.min.js'></script>
      <script>
        window.myWidgetParam ? window.myWidgetParam : window.myWidgetParam = [];  
        window.myWidgetParam.push({
          id: 11,
          cityid: '2158177',
          appid: '3e3e05ba3eace3597cb57ceda9413619',
          units: 'metric',
          containerid: 'openweathermap-widget-11'
        });
        (function() {
          var script = document.createElement('script');
          script.async = true;
          script.charset = 'utf-8';
          script.src = '//openweathermap.org/themes/openweathermap/assets/vendor/owm/js/weather-widget-generator.js';
          var s = document.getElementsByTagName('script')[0];
          s.parentNode.insertBefore(script, s);  
        })();
      </script>
    </div>
    ")
  })
  
  # Leaflet Map Rendering for Real-Time Weather Tab
  
  #output$realtime_map <- renderLeaflet({
  # leaflet() %>%
  #  addTiles() %>%  # Add OpenStreetMap
  # setView(lng = 144.9631, lat = -37.8136, zoom = 10)  # Set Melbourne as the center of the map
  #})
  
  
  
}


# Run Shiny App
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE)) 
