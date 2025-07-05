library(shiny)
library(tidyverse)
library(lubridate)
library(stringr)
library(plotly)

# Load and preprocess data
#folder_path <- "/Users/ritupatel09/Documents/Wildfire data"
file1 <- read_csv("data/2003.csv")
file2 <- read_csv("data/2013.csv")
file3 <- read_csv("data/2023.csv")

merged_df <- bind_rows(file1, file2, file3) %>%
  mutate(
    acq_date = as.Date(acq_date),
    frp = as.numeric(frp),
    confidence = as.numeric(confidence),
    daynight = factor(daynight),
    satellite = factor(satellite),
    year = year(acq_date),
    month = floor_date(acq_date, "month"),
    dayofweek = wday(acq_date, label = TRUE, week_start = 1),
    hour = as.integer(substr(str_pad(as.character(acq_time), 4, pad = "0"), 1, 2))
  ) %>%
  drop_na(latitude, longitude, frp)

# UI
ui <- navbarPage(
  title = actionLink("title_click", "🔥 Wildfire Dashboard"),
  
  tabPanel("\ud83d\udcca EDA & Trends",
           sidebarLayout(
             sidebarPanel(
               selectInput("satelliteFilter", "Select Satellite:",
                           choices = c("All", levels(merged_df$satellite)),
                           selected = "All"),
               selectInput("daynightFilter", "Day or Night:",
                           choices = c("All", levels(merged_df$daynight)),
                           selected = "All"),
               sliderInput("frpThreshold", "Minimum FRP:", min = 0, max = 500, value = 0, step = 10)
             ),
             mainPanel(
               plotlyOutput("boxplotBrightness"),
               plotlyOutput("wildfireTimeSeries"),
               plotlyOutput("confidenceBar"),
               plotlyOutput("satelliteBar")
             )
           )
  ),
  
  tabPanel("\ud83d\udcc8 FRP & Satellite Analysis",
           sidebarLayout(
             sidebarPanel(
               selectInput("satelliteFilter2", "Select Satellite:",
                           choices = c("All", levels(merged_df$satellite)),
                           selected = "All"),
               selectInput("daynightFilter2", "Day or Night:",
                           choices = c("All", levels(merged_df$daynight)),
                           selected = "All"),
               sliderInput("frpThreshold2", "Minimum FRP:", min = 0, max = 500, value = 0, step = 10)
             ),
             mainPanel(
               plotlyOutput("frpHist"),
               plotlyOutput("frpDensity"),
               plotlyOutput("satelliteFrpBar")
             )
           )
  ),
  
  tabPanel("\u23f0 Temporal Patterns",
           sidebarLayout(
             sidebarPanel(
               selectInput("satelliteFilter3", "Select Satellite:",
                           choices = c("All", levels(merged_df$satellite)),
                           selected = "All"),
               selectInput("daynightFilter3", "Day or Night:",
                           choices = c("All", levels(merged_df$daynight)),
                           selected = "All"),
               sliderInput("frpThreshold3", "Minimum FRP:", min = 0, max = 500, value = 0, step = 10)
             ),
             mainPanel(
               plotlyOutput("heatmapWeekHour"),
               plotlyOutput("frpByMonthBoxplot")
             )
           )
  )
)

# Server
server <- function(input, output, session) {
  
  observeEvent(input$title_click, {
    showModal(modalDialog(
      title = "Welcome to Wildfire Dashboard",
      "This dashboard explores satellite-based wildfire data through trends, distributions, and temporal patterns. Use the filters to interactively explore fire activity, satellite performance, and fire radiative power (FRP).",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  filter_data <- function(df, satelliteFilter, daynightFilter, frpThresh) {
    df %>%
      filter((satellite == satelliteFilter) | (satelliteFilter == "All")) %>%
      filter((daynight == daynightFilter) | (daynightFilter == "All")) %>%
      filter(frp >= frpThresh)
  }
  
  filtered_data1 <- reactive({ filter_data(merged_df, input$satelliteFilter, input$daynightFilter, input$frpThreshold) })
  filtered_data2 <- reactive({ filter_data(merged_df, input$satelliteFilter2, input$daynightFilter2, input$frpThreshold2) })
  filtered_data3 <- reactive({ filter_data(merged_df, input$satelliteFilter3, input$daynightFilter3, input$frpThreshold3) })
  
  output$boxplotBrightness <- renderPlotly({
    ggplotly({
      ggplot(filtered_data1(), aes(x = "", y = brightness)) +
        geom_boxplot(fill = "steelblue", alpha = 0.7) +
        labs(title = "Brightness Boxplot", y = "Brightness") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))
    })
  })
  
  output$wildfireTimeSeries <- renderPlotly({
    ggplotly({
      filtered_data1() %>%
        count(year) %>%
        filter(year %in% c(2003, 2013, 2023)) %>%
        ggplot(aes(x = factor(year), y = n)) +
        geom_col(fill = "firebrick") +
        labs(title = "Wildfires in 2003, 2013, and 2023", x = "Year", y = "Count") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))
    })
  })
  
  output$confidenceBar <- renderPlotly({
    ggplotly({
      ggplot(filtered_data1(), aes(x = factor(confidence))) +
        geom_bar(fill = "orange") +
        labs(title = "Confidence Level Distribution", x = "Confidence", y = "Count") +
        scale_x_discrete(breaks = scales::pretty_breaks(n = 10)) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))
    })
  })
  
  output$satelliteBar <- renderPlotly({
    ggplotly({
      ggplot(filtered_data1(), aes(x = satellite)) +
        geom_bar(fill = "steelblue") +
        labs(title = "Satellite Observations", x = "Satellite", y = "Count") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))
    })
  })
  
  output$frpHist <- renderPlotly({
    ggplotly({
      ggplot(filtered_data2(), aes(frp)) +
        geom_histogram(binwidth = 5, fill = "tomato", color = "black", alpha = 0.7) +
        coord_cartesian(xlim = c(0, 200)) +
        labs(title = "FRP Histogram", x = "Fire Radiative Power (FRP)", y = "Count") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))
    })
  })
  
  output$frpDensity <- renderPlotly({
    ggplotly({
      ggplot(filtered_data2(), aes(frp, fill = satellite)) +
        geom_density(alpha = 0.5) +
        coord_cartesian(xlim = c(0, 200)) +
        labs(title = "FRP Density by Satellite", x = "FRP", y = "Density") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))
    })
  })
  
  output$satelliteFrpBar <- renderPlotly({
    ggplotly({
      filtered_data2() %>%
        group_by(satellite) %>%
        summarise(mean_frp = mean(frp, na.rm = TRUE), .groups = "drop") %>%
        ggplot(aes(reorder(satellite, mean_frp), mean_frp)) +
        geom_col(fill = "darkgreen", alpha = 0.7) +
        coord_flip() +
        labs(title = "Average FRP by Satellite", x = "Satellite", y = "Average FRP") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))
    })
  })
  
  output$heatmapWeekHour <- renderPlotly({
    ggplotly({
      filtered_data3() %>%
        count(dayofweek, hour) %>%
        ggplot(aes(x = hour, y = dayofweek, fill = n)) +
        geom_tile() +
        scale_fill_viridis_c() +
        labs(title = "Wildfires Heatmap by Weekday & Hour", x = "Hour of Day", y = "Day of Week") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))
    })
  })
  
  output$frpByMonthBoxplot <- renderPlotly({
    ggplotly({
      filtered_data3() %>%
        mutate(
          year = year(acq_date),
          month = factor(month(acq_date, label = TRUE, abbr = TRUE), levels = month.abb)
        ) %>%
        group_by(month, year) %>%
        summarise(mean_frp = mean(frp, na.rm = TRUE), .groups = "drop") %>%
        ggplot(aes(x = month, y = mean_frp, fill = factor(year))) +
        geom_col(position = "stack") +
        labs(title = "Monthly FRP by Year", x = "Month", y = "Avg FRP", fill = "Year") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    })
  })
}


# Run the app
shinyApp(ui, server)
