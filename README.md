<img width="628" height="311" alt="Screenshot 2026-11-01 175933" src="https://github.com/user-attachments/assets/c634c102-a0ab-4bec-afeb-7b9879b74093" />

# Pacific Salmon Abundance Explorer App

An interactive R Shiny dashboard for exploring Pacific salmon abundance data through temporal trend analysis, management-area comparisons, statistical summaries, and data quality assessment.

## Overview

The Pacific Salmon Abundance Explorer is a single-file R Shiny application designed to demonstrate an environmental data analysis and visualization workflow. Users can interactively filter salmon abundance records by species, management area, population, year range, and abundance metric.

The dashboard transforms filtered records into interactive visualizations, descriptive statistics, and data quality reports. Users can also export filtered datasets, summary statistics, QA/QC results, and visualizations for further analysis.

The application currently uses simulated salmon abundance records to demonstrate its functionality. It is structured to support loading an external CSV dataset.

## Features

### Interactive Data Exploration
- Filter records by salmon species, management area, population, and abundance metric.
- Select custom year ranges using interactive controls.
- Dynamically update dashboard outputs based on selected filters.
- View key summary indicators for the filtered dataset.

### Data Visualization
- Explore annual abundance patterns through time-series visualizations.
- Compare abundance across management areas and populations.
- Examine abundance distributions and variation.
- Visualize year-over-year changes.
- Interact with charts using Plotly.

### Statistical Analysis
- Calculate descriptive statistics, including mean, median, minimum, maximum, and standard deviation.
- Summarize abundance by year, species, and management area.
- Examine year-over-year percentage changes.
- Display descriptive linear trend estimates.

### Data Quality Assurance & Quality Control (QA/QC)
- Identify missing values in key fields.
- Check for duplicate records.
- Flag negative abundance values.
- Identify invalid or missing years.
- Assess missing population and management-area metadata.
- Flag potential abundance outliers for further review.
- Generate a downloadable data quality report.

### Data Export
- Download filtered salmon abundance records as CSV.
- Export statistical summary tables.
- Download QA/QC reports.
- Export visualizations as image files.

## Methods

- Generated a simulated salmon abundance dataset covering multiple years, species, management areas, and populations.
- Implemented a configurable data-loading workflow that checks for an external CSV file and falls back to simulated data when one is unavailable.
- Used `dplyr` and `tidyr` for data manipulation, filtering, and summarization.
- Built reactive data expressions to ensure dashboard outputs update when users change filters.
- Used `ggplot2` and `plotly` to create interactive data visualizations.
- Applied descriptive statistical methods to summarize abundance patterns and temporal changes.
- Developed QA/QC checks to identify missing values, duplicate records, invalid values, and potential outliers.
- Implemented Shiny download handlers to export filtered data, statistical summaries, QA/QC reports, and plots.

## Data

**Current dataset:** Simulated Pacific salmon abundance records generated within the application.

**External data support:** The application checks for a CSV file at `data/raw/salmon_abundance.csv`. If the file is available, the app loads it; otherwise, it uses the simulated dataset.

No real-world salmon abundance observations are currently used. The simulated data are intended for demonstrating dashboard functionality and should not be interpreted as actual population estimates or ecological trends.

## Technologies

- **R** – Data processing and statistical analysis
- **Shiny** – Interactive web application framework
- **dplyr** – Data manipulation and summarization
- **tidyr** – Data organization
- **ggplot2** – Data visualization
- **Plotly** – Interactive charts
- **DT** – Interactive data tables
- **readr** – CSV data import
- **scales** – Axis and value formatting

## Running the Application

1. Clone or download this repository.
2. Open the project in RStudio.
3. Install the required packages if they are not already installed.
4. Open `app.R`.
5. Run the application using:

```r
shiny::runApp()
