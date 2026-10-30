# Salmon Abundance Explorer

Interactive RShiny app for exploring Pacific salmon abundance by management area and year, built as a demonstration of reactive data filtering, time-series visualization, and downloadable outputs.

## Overview

This project is a single-file RShiny application that lets users filter salmon abundance records by management area and year range, then view an updated trend plot and summary table in real time. The app is structured around a reactive filtered dataset, so every output responds to the sidebar controls without re-running the script.

It was built to demonstrate the workflow behind a decision-support tool: load a dataset, let the user narrow it, show the result visually, and let them take the filtered data away.

## Features

- Filter by management area using checkboxes
- Filter by year range using a slider
- Reactive abundance trend plot with one line per area
- Summary table of mean abundance by area for the selected period
- Download the filtered dataset as a CSV

## Methods

- Generated a demo dataset of annual abundance records for Areas 7 and 8, 2015–2024.
- Wrote the dataset to CSV on first run if it is not already present, so the app is self-contained.
- Read the CSV with `readr::read_csv()`.
- Built the interface with `fluidPage()`, `sidebarLayout()`, and standard Shiny input widgets.
- Created a reactive expression that subsets the data by selected area and year range.
- Rendered the trend plot with `ggplot2`, using `geom_line()` and `geom_point()` coloured by area.
- Rendered the summary table with `renderTable()` and `aggregate()`.
- Added a `downloadHandler()` to export the filtered data.

## Data & Tools

**Data**

- Demo salmon abundance records for Areas 7 and 8 (2015–2024), generated within the script

**Tools**

- R
- RShiny
- ggplot2
- readr
