# R Shiny Air Quality Index (AQI) Dashboard

This repository contains the source code for an interactive web application built in R Shiny to visualize and analyze time-series Air Quality Index (AQI) data. This tool was developed to support an environmental research project at LUMS.

## 🎯 Objective
To create a unified, interactive dashboard that automates the process of ingesting, cleaning, and visualizing complex time-series data from multiple, disparate air quality sensors. The goal is to empower researchers to easily perform comparative analysis without manual data wrangling.

## ⚙️ Key Features & Methodology
The application is a single, self-contained `app.R` script that performs all necessary operations.

1.  **Automated Data Pipeline**: The script recursively scans a directory for raw `.csv` data files from various sensors. It uses `stringr` and `lubridate` to parse each filename, extracting crucial metadata like the sensor's unique ID and the date of data collection.

2.  **Data Wrangling & Unification**: Using `dplyr` and `plyr`, the pipeline intelligently handles inconsistencies in column names across different files. It then merges hundreds of individual daily data files into a set of master time-series dataframes, one for each of the 10 monitored pollutants (e.g., PM2.5, Ozone, CO).

3.  **Interactive Visualization**: The front-end is built with **R Shiny**, featuring a simple dropdown menu for pollutant selection. The **`highcharter`** library is used to render beautiful and responsive multi-series line charts, allowing for a direct visual comparison of readings from all sensors over the entire time period.

## 📈 Outcome
The result is a powerful, web-based dashboard that significantly streamlines the research workflow. It transforms a complex and time-consuming data aggregation task into an intuitive, interactive experience, enabling researchers to quickly identify trends, spot anomalies, and compare sensor data with just a few clicks.

## 🛠️ Tech Stack
* **Language**: `R`
* **Core Libraries**: `shiny`, `highcharter`, `dplyr`, `plyr`, `lubridate`, `stringr`
