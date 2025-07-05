# 🔥 Wildfire Dashboard

An interactive R Shiny application that visualizes wildfire data from NASA satellite observations over the years **2003, 2013, and 2023**. The app allows users to explore patterns in **Fire Radiative Power (FRP)**, satellite-specific trends, temporal activity, and brightness levels across different regions and time periods.

## 🚀 Features

- 📊 **Exploratory Data Analysis (EDA)** tab with:
  - Brightness distribution (boxplot)
  - Wildfire count across selected years
  - Confidence level breakdown
  - Satellite-specific fire detections

- 📈 **FRP & Satellite Analysis** tab:
  - Histogram and density plot of FRP values
  - Mean FRP by satellite

- ⏰ **Temporal Patterns** tab:
  - Heatmap of wildfire activity by weekday and hour
  - Stacked bar chart of average FRP by month and year

- 📍 Clickable dashboard title to display a summary modal
- 🎯 Filters for satellite, day/night classification, and FRP threshold
- 🖱️ Tooltips (via `plotly`) for detailed plot interactivity

## 📂 Data Source

The app uses **MODIS satellite wildfire detection data** from:
- 2003
- 2013
- 2023

Data was preprocessed and merged into a unified dataset containing:
- Acquisition date/time
- FRP (Fire Radiative Power)
- Brightness
- Satellite used (Aqua or Terra)
- Confidence levels
- Latitude and longitude

## 📦 Dependencies

Main R packages used:

- `shiny`
- `plotly`
- `tidyverse`
- `lubridate`
- `viridis`

Install dependencies using:

```r
install.packages(c("shiny", "plotly", "tidyverse", "lubridate", "viridis"))
