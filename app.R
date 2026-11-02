# ============================================================
# PACIFIC SALMON ABUNDANCE & FISHERIES DATA EXPLORER
# ============================================================
#
# Interactive environmental data analytics application
#
# Features:
# - Multi-dimensional data filtering
# - Interactive salmon abundance trends
# - Management area comparisons
# - Year-over-year percentage changes
# - Abundance distribution analysis
# - Descriptive statistics
# - Linear trend analysis
# - Automated data QA/QC
# - Detailed data-quality issue reporting
# - Downloadable datasets, summaries and reports

# ============================================================
# 1. LOAD LIBRARIES
# ============================================================

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)
library(readr)
library(janitor)
library(stringr)
library(scales)


# ============================================================
# 2. PROJECT CONFIGURATION
# ============================================================

DATA_PATH <- "data/raw/salmon_abundance.csv"

APP_TITLE <- "Pacific Salmon Abundance Explorer"

if (!dir.exists("outputs")) {
  dir.create("outputs", recursive = TRUE)
}


# ============================================================
# 3. GENERATE DEMONSTRATION DATA
# ============================================================

generate_demo_data <- function() {
  
  set.seed(2026)
  
  years <- 2015:2024
  
  species <- c(
    "Chinook",
    "Chum",
    "Coho",
    "Pink",
    "Sockeye"
  )
  
  areas <- c(
    "Area 7",
    "Area 8",
    "Area 9",
    "Area 10"
  )
  
  populations <- c(
    "Population A",
    "Population B",
    "Population C"
  )
  
  demo <- expand_grid(
    year = years,
    species = species,
    area = areas,
    population = populations
  ) %>%
    mutate(
      
      abundance = round(
        runif(
          n(),
          min = 250,
          max = 5000
        )
      ),
      
      abundance_type = "Demonstration abundance",
      
      unit = "Individuals",
      
      source = "Simulated demonstration dataset",
      
      method = "Synthetic random generation"
      
    )
  
  demo
  
}


# ============================================================
# 4. IMPORT AND STANDARDIZE DATA
# ============================================================

load_salmon_data <- function() {
  
  if (file.exists(DATA_PATH)) {
    
    message("Importing external salmon dataset.")
    
    imported <- read_csv(
      DATA_PATH,
      show_col_types = FALSE
    ) %>%
      clean_names()
    
    required_columns <- c(
      "year",
      "species",
      "area",
      "population",
      "abundance"
    )
    
    missing_columns <- setdiff(
      required_columns,
      names(imported)
    )
    
    if (length(missing_columns) > 0) {
      
      stop(
        paste(
          "Missing required columns:",
          paste(missing_columns, collapse = ", ")
        )
      )
      
    }
    
    optional_columns <- c(
      "abundance_type",
      "unit",
      "source",
      "method"
    )
    
    for (column in optional_columns) {
      
      if (!column %in% names(imported)) {
        imported[[column]] <- NA_character_
      }
      
    }
    
    imported <- imported %>%
      mutate(
        
        year = suppressWarnings(
          as.integer(year)
        ),
        
        abundance = suppressWarnings(
          as.numeric(abundance)
        ),
        
        across(
          all_of(optional_columns),
          as.character
        ),
        
        across(
          all_of(c(
            "species",
            "area",
            "population"
          )),
          as.character
        )
        
      )
    
    return(imported)
    
  } else {
    
    message(
      "No external dataset found. Using simulated demonstration data."
    )
    
    return(generate_demo_data())
    
  }
  
}


salmon_data <- load_salmon_data()


# ============================================================
# 5. DATA QUALITY ASSESSMENT
# ============================================================

run_quality_checks <- function(data) {
  
  total_records <- nrow(data)
  
  missing_values <- sum(
    is.na(data)
  )
  
  duplicate_records <- sum(
    duplicated(
      data[
        c(
          "year",
          "species",
          "area",
          "population",
          "abundance_type"
        )
      ]
    )
  )
  
  missing_years <- sum(is.na(data$year))
  
  missing_abundance <- sum(
    is.na(data$abundance)
  )
  
  negative_abundance <- sum(
    !is.na(data$abundance) &
      data$abundance < 0
  )
  
  invalid_years <- sum(
    !is.na(data$year) &
      (
        data$year < 1900 |
          data$year > as.integer(
            format(Sys.Date(), "%Y")
          ) + 1
      )
  )
  
  missing_species <- sum(
    is.na(data$species) |
      trimws(data$species) == ""
  )
  
  missing_area <- sum(
    is.na(data$area) |
      trimws(data$area) == ""
  )
  
  missing_population <- sum(
    is.na(data$population) |
      trimws(data$population) == ""
  )
  
  missing_metadata <- sum(
    is.na(data$source) |
      trimws(data$source) == ""
  )
  
  # Potential outliers using 1.5 x IQR.
  # Outliers are flagged, not automatically removed.
  
  outlier_data <- data %>%
    filter(
      !is.na(abundance),
      !is.na(species),
      !is.na(area)
    ) %>%
    group_by(species, area) %>%
    mutate(
      
      q1 = quantile(
        abundance,
        0.25,
        na.rm = TRUE
      ),
      
      q3 = quantile(
        abundance,
        0.75,
        na.rm = TRUE
      ),
      
      iqr_value = q3 - q1,
      
      potential_outlier =
        abundance < (q1 - 1.5 * iqr_value) |
        abundance > (q3 + 1.5 * iqr_value)
      
    ) %>%
    ungroup()
  
  potential_outliers <- sum(
    outlier_data$potential_outlier,
    na.rm = TRUE
  )
  
  checks <- tibble(
    
    check = c(
      "Total records",
      "Missing values",
      "Duplicate records",
      "Missing years",
      "Missing abundance",
      "Negative abundance",
      "Invalid years",
      "Missing species",
      "Missing management area",
      "Missing population identifiers",
      "Missing source metadata",
      "Potential statistical outliers"
    ),
    
    result = c(
      total_records,
      missing_values,
      duplicate_records,
      missing_years,
      missing_abundance,
      negative_abundance,
      invalid_years,
      missing_species,
      missing_area,
      missing_population,
      missing_metadata,
      potential_outliers
    )
    
  )
  
  checks %>%
    mutate(
      
      status = case_when(
        
        check == "Total records" ~ "Information",
        
        result == 0 ~ "Pass",
        
        check == "Potential statistical outliers" ~ "Review",
        
        TRUE ~ "Review"
        
      )
      
    )
  
}


# ============================================================
# 6. DETAILED QA/QC ISSUE REPORT
# ============================================================

generate_issue_report <- function(data) {
  
  add_issues <- function(rows, issue_name, field_name, values) {
    
    if (length(rows) == 0) {
      
      return(
        tibble(
          row_number = integer(),
          issue = character(),
          field = character(),
          value = character()
        )
      )
      
    }
    
    tibble(
      row_number = rows,
      issue = issue_name,
      field = field_name,
      value = as.character(values[rows])
    )
    
  }
  
  issues <- bind_rows(
    
    add_issues(
      which(is.na(data$year)),
      "Missing year",
      "year",
      data$year
    ),
    
    add_issues(
      which(is.na(data$abundance)),
      "Missing abundance",
      "abundance",
      data$abundance
    ),
    
    add_issues(
      which(!is.na(data$abundance) & data$abundance < 0),
      "Negative abundance",
      "abundance",
      data$abundance
    ),
    
    add_issues(
      which(is.na(data$species) | data$species == ""),
      "Missing species",
      "species",
      data$species
    ),
    
    add_issues(
      which(is.na(data$area) | data$area == ""),
      "Missing management area",
      "area",
      data$area
    ),
    
    add_issues(
      which(is.na(data$population) | data$population == ""),
      "Missing population identifier",
      "population",
      data$population
    )
    
  )
  
  duplicate_rows <- which(
    duplicated(
      data[
        c(
          "year",
          "species",
          "area",
          "population",
          "abundance_type"
        )
      ]
    )
  )
  
  issues <- bind_rows(
    
    issues,
    
    add_issues(
      duplicate_rows,
      "Potential duplicate record",
      "record",
      duplicate_rows
    )
    
  )
  
  invalid_year_rows <- which(
    !is.na(data$year) &
      (
        data$year < 1900 |
          data$year > as.integer(
            format(Sys.Date(), "%Y")
          ) + 1
      )
  )
  
  issues <- bind_rows(
    
    issues,
    
    add_issues(
      invalid_year_rows,
      "Invalid year",
      "year",
      data$year
    )
    
  )
  
  issues
  
}


# ============================================================
# 7. USER INTERFACE
# ============================================================

ui <- navbarPage(
  
  title = APP_TITLE,
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#176B87",
    secondary = "#64CCC5"
  ),
  
  header = tags$head(
    
    tags$style(HTML("

      body {
        background-color: #f5f7fa;
      }

      .navbar {
        font-weight: 600;
      }

      .card {
        border-radius: 10px;
        margin-bottom: 15px;
      }

      .value-card {
        padding: 18px;
        min-height: 110px;
      }

      .value-number {
        font-size: 27px;
        font-weight: bold;
        color: #176B87;
      }

      .value-label {
        font-size: 13px;
        color: #666;
      }

      .section-heading {
        font-weight: 600;
        margin-bottom: 12px;
      }

      .disclaimer {
        background-color: #fff3cd;
        border-left: 5px solid #d99b16;
        padding: 12px;
        margin-bottom: 15px;
      }

    "))
    
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      width = 3,
      
      h4("Data filters"),
      
      selectizeInput(
        "species",
        "Salmon species",
        choices = sort(unique(
          na.omit(salmon_data$species)
        )),
        selected = sort(unique(
          na.omit(salmon_data$species)
        )),
        multiple = TRUE
      ),
      
      selectizeInput(
        "area",
        "Management area",
        choices = sort(unique(
          na.omit(salmon_data$area)
        )),
        selected = sort(unique(
          na.omit(salmon_data$area)
        )),
        multiple = TRUE
      ),
      
      selectizeInput(
        "population",
        "Population",
        choices = sort(unique(
          na.omit(salmon_data$population)
        )),
        selected = sort(unique(
          na.omit(salmon_data$population)
        )),
        multiple = TRUE
      ),
      
      uiOutput("year_filter"),
      
      selectInput(
        "metric",
        "Abundance metric",
        choices = NULL
      ),
      
      checkboxInput(
        "remove_missing",
        "Exclude records with missing abundance",
        value = TRUE
      ),
      
      actionButton(
        "reset",
        "Reset filters",
        icon = icon("refresh"),
        class = "btn-outline-primary"
      ),
      
      hr(),
      
      strong("Dataset information"),
      
      textOutput("dataset_source"),
      
      textOutput("record_count")
      
    ),
    
    mainPanel(
      
      width = 9,
      
      uiOutput("demo_warning"),
      
      tabsetPanel(
        
        id = "main_tabs",
        
        # ----------------------------------------------------
        # OVERVIEW
        # ----------------------------------------------------
        
        tabPanel(
          
          "Overview",
          
          br(),
          
          fluidRow(
            
            column(
              3,
              wellPanel(
                div(
                  class = "value-card",
                  div(
                    class = "value-label",
                    "Filtered records"
                  ),
                  div(
                    class = "value-number",
                    textOutput("kpi_records")
                  )
                )
              )
            ),
            
            column(
              3,
              wellPanel(
                div(
                  class = "value-card",
                  div(
                    class = "value-label",
                    "Species represented"
                  ),
                  div(
                    class = "value-number",
                    textOutput("kpi_species")
                  )
                )
              )
            ),
            
            column(
              3,
              wellPanel(
                div(
                  class = "value-card",
                  div(
                    class = "value-label",
                    "Management areas"
                  ),
                  div(
                    class = "value-number",
                    textOutput("kpi_areas")
                  )
                )
              )
            ),
            
            column(
              3,
              wellPanel(
                div(
                  class = "value-card",
                  div(
                    class = "value-label",
                    "Mean abundance"
                  ),
                  div(
                    class = "value-number",
                    textOutput("kpi_mean")
                  )
                )
              )
            )
            
          ),
          
          h4(
            "Annual salmon abundance trends",
            class = "section-heading"
          ),
          
          plotlyOutput(
            "trend_plot",
            height = "400px"
          ),
          
          hr(),
          
          fluidRow(
            
            column(
              6,
              
              h4("Management area comparison"),
              
              plotlyOutput(
                "area_plot",
                height = "350px"
              )
              
            ),
            
            column(
              6,
              
              h4("Annual percentage change"),
              
              plotlyOutput(
                "change_plot",
                height = "350px"
              )
              
            )
            
          )
          
        ),
        
        
        # ----------------------------------------------------
        # DISTRIBUTION
        # ----------------------------------------------------
        
        tabPanel(
          
          "Distribution",
          
          br(),
          
          h4("Abundance distribution"),
          
          p(
            "Examine abundance distribution and variability across selected areas and species."
          ),
          
          plotlyOutput(
            "distribution_plot",
            height = "450px"
          ),
          
          hr(),
          
          h4("Statistical summary"),
          
          DTOutput("statistics_table"),
          
          br(),
          
          downloadButton(
            "download_statistics",
            "Download statistical summary"
          )
          
        ),
        
        
        # ----------------------------------------------------
        # DATA EXPLORER
        # ----------------------------------------------------
        
        tabPanel(
          
          "Data Explorer",
          
          br(),
          
          h4("Filtered salmon abundance records"),
          
          DTOutput("data_table"),
          
          br(),
          
          downloadButton(
            "download_data",
            "Download filtered dataset"
          )
          
        ),
        
        
        # ----------------------------------------------------
        # QA/QC
        # ----------------------------------------------------
        
        tabPanel(
          
          "Data QA/QC",
          
          br(),
          
          h4("Data quality assessment"),
          
          p(
            "Automated checks identify potential data-quality issues. Results require contextual review."
          ),
          
          DTOutput("qa_table"),
          
          hr(),
          
          h4("Detailed QA/QC issue report"),
          
          DTOutput("issue_table"),
          
          br(),
          
          downloadButton(
            "download_qa",
            "Download QA/QC report"
          )
          
        ),
        
        
        # ----------------------------------------------------
        # METHODS AND METADATA
        # ----------------------------------------------------
        
        tabPanel(
          
          "Methods & Metadata",
          
          br(),
          
          h4("Project methodology"),
          
          p(
            "This application uses a structured data-processing workflow to support exploration of Pacific salmon abundance observations."
          ),
          
          h5("Data processing"),
          
          tags$ul(
            
            tags$li(
              "Import and standardization of environmental datasets."
            ),
            
            tags$li(
              "Conversion of year and abundance fields."
            ),
            
            tags$li(
              "Filtering by species, area, population, and year."
            ),
            
            tags$li(
              "Descriptive statistical analysis."
            ),
            
            tags$li(
              "Temporal trend and percentage-change calculations."
            )
            
          ),
          
          h5("QA/QC procedures"),
          
          tags$ul(
            
            tags$li("Missing-value assessment."),
            
            tags$li("Potential duplicate-record detection."),
            
            tags$li("Negative abundance screening."),
            
            tags$li("Year validation."),
            
            tags$li("Metadata completeness assessment."),
            
            tags$li("Potential statistical outlier identification.")
            
          ),
          
          h5("Interpretation limitations"),
          
          p(
            "Abundance estimates may differ in survey intensity, estimation methods, population coverage, and data completeness. Comparisons should account for these differences."
          ),
          
          p(
            "A statistical trend is descriptive and does not establish the cause of population changes."
          ),
          
          hr(),
          
          h5("Data dictionary"),
          
          tags$ul(
            
            tags$li(
              strong("year: "),
              "Calendar year of observation."
            ),
            
            tags$li(
              strong("species: "),
              "Pacific salmon species."
            ),
            
            tags$li(
              strong("area: "),
              "Fisheries management area."
            ),
            
            tags$li(
              strong("population: "),
              "Population or monitoring-unit identifier."
            ),
            
            tags$li(
              strong("abundance: "),
              "Numeric abundance estimate."
            ),
            
            tags$li(
              strong("abundance_type: "),
              "Type of abundance measure."
            ),
            
            tags$li(
              strong("unit: "),
              "Measurement unit."
            ),
            
            tags$li(
              strong("source: "),
              "Original dataset or source."
            ),
            
            tags$li(
              strong("method: "),
              "Survey or estimation methodology."
            )
            
          ),
          
          h5("Software and packages"),
          
          p(
            "R, Shiny, dplyr, tidyr, ggplot2, plotly, DT, readr, janitor, and scales."
          )
          
        )
        
      )
      
    )
    
  )
  
)


# ============================================================
# 8. SERVER LOGIC
# ============================================================

server <- function(input, output, session) {
  
  
  # ----------------------------------------------------------
  # DATASET INFORMATION
  # ----------------------------------------------------------
  
  is_demo <- !file.exists(DATA_PATH)
  
  
  output$dataset_source <- renderText({
    
    if (is_demo) {
      "Simulated demonstration dataset"
    } else {
      "External CSV dataset"
    }
    
  })
  
  
  output$demo_warning <- renderUI({
    
    if (is_demo) {
      
      div(
        class = "disclaimer",
        
        strong("Demonstration data: "),
        
        "This application currently uses simulated salmon abundance records. These values are not actual fisheries observations."
        
      )
      
    }
    
  })
  
  
  output$record_count <- renderText({
    
    paste(
      "Total source records:",
      format(
        nrow(salmon_data),
        big.mark = ","
      )
    )
    
  })
  
  
  # ----------------------------------------------------------
  # YEAR FILTER
  # ----------------------------------------------------------
  
  output$year_filter <- renderUI({
    
    years <- salmon_data$year[
      !is.na(salmon_data$year)
    ]
    
    validate(
      need(
        length(years) > 0,
        "No valid year values available."
      )
    )
    
    sliderInput(
      "year",
      "Year range",
      min = min(years),
      max = max(years),
      value = range(years),
      step = 1,
      sep = ""
    )
    
  })
  
  
  # ----------------------------------------------------------
  # ABUNDANCE METRIC FILTER
  # ----------------------------------------------------------
  
  observe({
    
    metrics <- unique(
      na.omit(salmon_data$abundance_type)
    )
    
    if (length(metrics) == 0) {
      metrics <- "All abundance types"
    }
    
    updateSelectInput(
      session,
      "metric",
      choices = metrics,
      selected = metrics[1]
    )
    
  })
  
  
  # ----------------------------------------------------------
  # RESET FILTERS
  # ----------------------------------------------------------
  
  observeEvent(input$reset, {
    
    updateSelectizeInput(
      session,
      "species",
      selected = unique(
        na.omit(salmon_data$species)
      )
    )
    
    updateSelectizeInput(
      session,
      "area",
      selected = unique(
        na.omit(salmon_data$area)
      )
    )
    
    updateSelectizeInput(
      session,
      "population",
      selected = unique(
        na.omit(salmon_data$population)
      )
    )
    
    years <- salmon_data$year[
      !is.na(salmon_data$year)
    ]
    
    if (length(years) > 0) {
      
      updateSliderInput(
        session,
        "year",
        value = range(years)
      )
      
    }
    
    updateCheckboxInput(
      session,
      "remove_missing",
      value = TRUE
    )
    
  })
  
  
  # ----------------------------------------------------------
  # REACTIVE FILTERED DATASET
  # ----------------------------------------------------------
  
  filtered_data <- reactive({
    
    req(input$species)
    req(input$area)
    req(input$population)
    req(input$year)
    
    result <- salmon_data %>%
      
      filter(
        
        species %in% input$species,
        
        area %in% input$area,
        
        population %in% input$population,
        
        !is.na(year),
        
        year >= input$year[1],
        
        year <= input$year[2]
        
      )
    
    if (
      !is.null(input$metric) &&
      input$metric != "All abundance types"
    ) {
      
      result <- result %>%
        filter(
          abundance_type == input$metric
        )
      
    }
    
    if (isTRUE(input$remove_missing)) {
      
      result <- result %>%
        filter(
          !is.na(abundance)
        )
      
    }
    
    result
    
  })
  
  
  # ----------------------------------------------------------
  # ANNUAL SUMMARY
  # ----------------------------------------------------------
  
  annual_summary <- reactive({
    
    filtered_data() %>%
      
      filter(
        !is.na(abundance)
      ) %>%
      
      group_by(
        year,
        species,
        area
      ) %>%
      
      summarise(
        
        mean_abundance = mean(
          abundance,
          na.rm = TRUE
        ),
        
        median_abundance = median(
          abundance,
          na.rm = TRUE
        ),
        
        population_count = n_distinct(
          population
        ),
        
        records = n(),
        
        .groups = "drop"
        
      )
    
  })
  
  
  # ----------------------------------------------------------
  # YEAR-OVER-YEAR CHANGE
  # ----------------------------------------------------------
  
  annual_change <- reactive({
    
    annual_summary() %>%
      
      arrange(
        species,
        area,
        year
      ) %>%
      
      group_by(
        species,
        area
      ) %>%
      
      mutate(
        
        previous_year = lag(year),
        
        previous_abundance = lag(
          mean_abundance
        ),
        
        percentage_change = if_else(
          
          !is.na(previous_abundance) &
            previous_abundance != 0 &
            year == previous_year + 1,
          
          (
            mean_abundance - previous_abundance
          ) / previous_abundance * 100,
          
          NA_real_
          
        )
        
      ) %>%
      
      ungroup()
    
  })
  
  
  # ----------------------------------------------------------
  # STATISTICAL SUMMARY AND LINEAR TREND
  # ----------------------------------------------------------
  
  statistical_summary <- reactive({
    
    stats <- filtered_data() %>%
      
      filter(
        !is.na(abundance),
        !is.na(year)
      ) %>%
      
      group_by(
        species,
        area
      ) %>%
      
      summarise(
        
        Records = n(),
        
        Populations = n_distinct(
          population
        ),
        
        Mean = mean(
          abundance
        ),
        
        Median = median(
          abundance
        ),
        
        Minimum = min(
          abundance
        ),
        
        Maximum = max(
          abundance
        ),
        
        Standard_deviation = sd(
          abundance
        ),
        
        .groups = "drop"
        
      )
    
    trend <- annual_summary() %>%
      
      group_by(
        species,
        area
      ) %>%
      
      summarise(
        
        Trend_slope = if (
          n_distinct(year) >= 3
        ) {
          
          unname(
            coef(
              lm(mean_abundance ~ year)
            )[2]
          )
          
        } else {
          
          NA_real_
          
        },
        
        .groups = "drop"
        
      ) %>%
      
      mutate(
        
        Trend_direction = case_when(
          
          is.na(Trend_slope) ~ "Insufficient data",
          
          Trend_slope > 0 ~ "Increasing",
          
          Trend_slope < 0 ~ "Decreasing",
          
          TRUE ~ "No linear change"
          
        )
        
      )
    
    stats %>%
      
      left_join(
        trend,
        by = c("species", "area")
      )
    
  })
  
  
  # ==========================================================
  # 9. DASHBOARD INDICATORS
  # ==========================================================
  
  output$kpi_records <- renderText({
    
    format(
      nrow(filtered_data()),
      big.mark = ","
    )
    
  })
  
  
  output$kpi_species <- renderText({
    
    n_distinct(
      filtered_data()$species,
      na.rm = TRUE
    )
    
  })
  
  
  output$kpi_areas <- renderText({
    
    n_distinct(
      filtered_data()$area,
      na.rm = TRUE
    )
    
  })
  
  
  output$kpi_mean <- renderText({
    
    values <- filtered_data()$abundance
    
    if (all(is.na(values)) || length(values) == 0) {
      return("N/A")
    }
    
    format(
      round(
        mean(values, na.rm = TRUE),
        1
      ),
      big.mark = ","
    )
    
  })
  
  
  # ==========================================================
  # 10. INTERACTIVE VISUALIZATIONS
  # ==========================================================
  
  # ----------------------------------------------------------
  # ANNUAL ABUNDANCE TRENDS
  # ----------------------------------------------------------
  
  output$trend_plot <- renderPlotly({
    
    plot_data <- annual_summary()
    
    validate(
      need(
        nrow(plot_data) > 0,
        "No records available for the selected filters."
      )
    )
    
    p <- ggplot(
      
      plot_data,
      
      aes(
        x = year,
        y = mean_abundance,
        colour = area,
        group = area,
        text = paste0(
          "Year: ", year,
          "<br>Species: ", species,
          "<br>Area: ", area,
          "<br>Mean abundance: ",
          round(mean_abundance, 1),
          "<br>Populations represented: ",
          population_count
        )
      )
      
    ) +
      
      geom_line(
        linewidth = 1
      ) +
      
      geom_point(
        size = 2
      ) +
      
      facet_wrap(
        ~ species,
        scales = "free_y"
      ) +
      
      labs(
        title = "Annual Salmon Abundance",
        subtitle = "Mean abundance per recorded population",
        x = "Year",
        y = "Mean abundance",
        colour = "Management area"
      ) +
      
      scale_x_continuous(
        breaks = pretty_breaks()
      ) +
      
      theme_minimal(
        base_size = 13
      ) +
      
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold")
      )
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
  
  # ----------------------------------------------------------
  # MANAGEMENT AREA COMPARISON
  # ----------------------------------------------------------
  
  output$area_plot <- renderPlotly({
    
    plot_data <- annual_summary() %>%
      
      group_by(
        species,
        area
      ) %>%
      
      summarise(
        
        mean_abundance = mean(
          mean_abundance,
          na.rm = TRUE
        ),
        
        .groups = "drop"
        
      )
    
    validate(
      need(
        nrow(plot_data) > 0,
        "No records available."
      )
    )
    
    p <- ggplot(
      
      plot_data,
      
      aes(
        x = area,
        y = mean_abundance,
        fill = area,
        text = paste0(
          "Species: ", species,
          "<br>Area: ", area,
          "<br>Mean abundance: ",
          round(mean_abundance, 1)
        )
      )
      
    ) +
      
      geom_col(
        show.legend = FALSE
      ) +
      
      facet_wrap(
        ~ species,
        scales = "free_y"
      ) +
      
      labs(
        x = "Management area",
        y = "Mean annual abundance"
      ) +
      
      theme_minimal(
        base_size = 12
      ) +
      
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
  
  # ----------------------------------------------------------
  # YEAR-OVER-YEAR PERCENTAGE CHANGE
  # ----------------------------------------------------------
  
  output$change_plot <- renderPlotly({
    
    plot_data <- annual_change() %>%
      
      filter(
        !is.na(percentage_change)
      )
    
    validate(
      need(
        nrow(plot_data) > 0,
        "Insufficient consecutive annual observations to calculate change."
      )
    )
    
    p <- ggplot(
      
      plot_data,
      
      aes(
        x = year,
        y = percentage_change,
        fill = area,
        text = paste0(
          "Year: ", year,
          "<br>Species: ", species,
          "<br>Area: ", area,
          "<br>Change: ",
          round(percentage_change, 2),
          "%"
        )
      )
      
    ) +
      
      geom_col(
        position = "dodge"
      ) +
      
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      
      facet_wrap(
        ~ species,
        scales = "free_y"
      ) +
      
      labs(
        x = "Year",
        y = "Change (%)"
      ) +
      
      theme_minimal(
        base_size = 12
      )
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
  
  # ----------------------------------------------------------
  # ABUNDANCE DISTRIBUTION
  # ----------------------------------------------------------
  
  output$distribution_plot <- renderPlotly({
    
    plot_data <- filtered_data() %>%
      
      filter(
        !is.na(abundance)
      )
    
    validate(
      need(
        nrow(plot_data) > 0,
        "No abundance observations available."
      )
    )
    
    p <- ggplot(
      
      plot_data,
      
      aes(
        x = area,
        y = abundance,
        fill = area
      )
      
    ) +
      
      geom_boxplot(
        alpha = 0.75,
        outlier.shape = 16
      ) +
      
      facet_wrap(
        ~ species,
        scales = "free_y"
      ) +
      
      labs(
        x = "Management area",
        y = "Abundance"
      ) +
      
      theme_minimal(
        base_size = 13
      ) +
      
      theme(
        legend.position = "none"
      )
    
    ggplotly(p)
    
  })
  
  
  # ==========================================================
  # 11. DATA TABLES
  # ==========================================================
  
  output$data_table <- renderDT({
    
    datatable(
      
      filtered_data(),
      
      options = list(
        pageLength = 15,
        scrollX = TRUE
      ),
      
      filter = "top",
      
      rownames = FALSE
      
    )
    
  })
  
  
  output$statistics_table <- renderDT({
    
    datatable(
      
      statistical_summary(),
      
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      
      rownames = FALSE
      
    ) %>%
      
      formatRound(
        columns = c(
          "Mean",
          "Median",
          "Standard_deviation",
          "Trend_slope"
        ),
        digits = 2
      )
    
  })
  
  
  # ==========================================================
  # 12. QA/QC OUTPUTS
  # ==========================================================
  
  output$qa_table <- renderDT({
    
    qa_results <- run_quality_checks(
      salmon_data
    )
    
    datatable(
      
      qa_results,
      
      options = list(
        pageLength = 15,
        searching = FALSE
      ),
      
      rownames = FALSE
      
    )
    
  })
  
  
  output$issue_table <- renderDT({
    
    issues <- generate_issue_report(
      salmon_data
    )
    
    datatable(
      
      issues,
      
      options = list(
        pageLength = 15,
        scrollX = TRUE
      ),
      
      rownames = FALSE
      
    )
    
  })
  
  
  # ==========================================================
  # 13. DOWNLOADABLE OUTPUTS
  # ==========================================================
  
  # ----------------------------------------------------------
  # FILTERED DATASET
  # ----------------------------------------------------------
  
  output$download_data <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "salmon_filtered_data_",
        Sys.Date(),
        ".csv"
      )
      
    },
    
    content = function(file) {
      
      write_csv(
        filtered_data(),
        file
      )
      
    }
    
  )
  
  
  # ----------------------------------------------------------
  # QA/QC REPORT
  # ----------------------------------------------------------
  
  output$download_qa <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "salmon_QAQC_report_",
        Sys.Date(),
        ".csv"
      )
      
    },
    
    content = function(file) {
      
      write_csv(
        
        generate_issue_report(
          salmon_data
        ),
        
        file
        
      )
      
    }
    
  )
  
  
  # ----------------------------------------------------------
  # STATISTICAL SUMMARY
  # ----------------------------------------------------------
  
  output$download_statistics <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "salmon_statistics_",
        Sys.Date(),
        ".csv"
      )
      
    },
    
    content = function(file) {
      
      write_csv(
        statistical_summary(),
        file
      )
      
    }
    
  )
  
}


# ============================================================
# 14. LAUNCH APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)


# ============================================================
# END OF APPLICATION
# ============================================================
