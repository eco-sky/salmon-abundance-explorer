# ---- Project information ----
# Generate demo salmon abundance data for the RShiny app
# Date: 2026-10-20

# ---- Load libraries ----
library(shiny)
library(ggplot2)
library(readr)

# ---- Build data ----
years <- 2015:2024

salmon <- data.frame(
  year = rep(years, times = 2),
  area = rep(c("Area 7", "Area 8"), each = length(years)),
  abundance = c(
    1200, 1450, 980, 1620, 1340, 890, 1710, 1530, 1120, 1880,  # Area 7
    800, 760, 640, 910, 720, 510, 880, 940, 670, 1010           # Area 8
  )
)

# ---- Write file ----
write.csv(salmon, "salmon.csv", row.names = FALSE)

# ---- Check ----
head(salmon)
str(salmon)

# ---- Data ----
data <- read_csv("salmon.csv", show_col_types = FALSE)

# ---- User interface ----
ui <- fluidPage(
  titlePanel("Pacific Salmon Abundance Explorer"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "area", "Select area:",
        choices = unique(dat$area),
        selected = unique(dat$area)
      ),
      sliderInput(
        "yr", "Year range:",
        min = min(dat$year), max = max(dat$year),
        value = c(min(dat$year), max(dat$year)),
        step = 1, sep = ""
      ),
      downloadButton("download", "Download filtered data")
    ),
    mainPanel(
      plotOutput("trend"),
      tableOutput("summary")
    )
  )
)

# ---- Server ----
server <- function(input, output) {
  
  filtered <- reactive({
    dat[dat$area %in% input$area &
          dat$year >= input$yr[1] &
          dat$year <= input$yr[2], ]
  })
  
  output$trend <- renderPlot({
    ggplot(filtered(), aes(x = year, y = abundance, colour = area)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(x = "Year", y = "Abundance", colour = "Area") +
      theme_minimal(base_size = 14)
  })
  
  output$summary <- renderTable({
    aggregate(abundance ~ area, data = filtered(), FUN = mean)
  })
  
  output$download <- downloadHandler(
    filename = function() paste0("salmon_filtered_", Sys.Date(), ".csv"),
    content = function(file) write.csv(filtered(), file, row.names = FALSE)
  )
}

# ---- Launch ----
shinyApp(ui, server)
