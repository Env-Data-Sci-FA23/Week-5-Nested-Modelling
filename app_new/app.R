#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# install.packages("shinythemes")
# install.packages("leaflet")
# install.packages("plotly")

library(shiny)
library(plotly)
library(dplyr)
library(shinythemes)
library(bslib)
library(tmap)
library(sf)
library(tidyverse)
library(ggplot2)

final_wq <- readRDS("data/final_wq.RDS")

final_wq_map <- readRDS("data/final_wq_map.RDS")
# UI
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "yeti"),
  titlePanel("Colorado River Basin Water Quality"),
  h5(
    "In this app you can filter annual occurrences of specific water quality parameters by basin, parameter, years and concentration."
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "basin.y",
        label = "Basin",
        choices = c(
          "roaring",
          "gunnison",
          "colorado1",
          "colorado3",
          "colorado4",
          "colorado5",
          "dolores",
          "eagle"
        ),
        selected = c(
          "roaring",
          "gunnison",
          "colorado1",
          "colorado3",
          "colorado4",
          "colorado5",
          "dolores",
          "eagle"
        ),
        multiple = TRUE
      ),
      selectInput(
        inputId = "parameter",
        label = "Parameter",
        choices = c(
          "Calcium",
          "Chloride",
          "Magnesium",
          "Potassium",
          "Sodium",
          "Sulfate"
        ),
        selected = c(
          "Calcium",
          "Chloride",
          "Magnesium",
          "Potassium",
          "Sodium",
          "Sulfate"
        ),
        multiple = TRUE
      ),
      selectInput(
        inputId = "year",
        label = "Year",
        choices = c(
          "2000",
          "2001",
          "2002",
          "2003",
          "2004",
          "2005",
          "2006",
          "2007",
          "2008",
          "2009",
          "2010",
          "2011",
          "2012",
          "2013",
          "2014",
          "2015",
          "2016",
          "2017",
          "2018",
          "2019",
          "2020",
          "2021",
          "2022",
          "2023",
          "2024"
        ),
        selected = c(
          "2000",
          "2001",
          "2002",
          "2003",
          "2004",
          "2005",
          "2006",
          "2007",
          "2008",
          "2009",
          "2010",
          "2011",
          "2012",
          "2013",
          "2014",
          "2015",
          "2016",
          "2017",
          "2018",
          "2019",
          "2020",
          "2021",
          "2022",
          "2023",
          "2024"
        ),
        multiple = TRUE
      ),
      sliderInput(
        inputId = "total_conc",
        label = "Concentration",
        min = 3,
        max = 14000,
        value = c(3, 14000)
      )
    ),
    mainPanel(#outputs: tabsets w/ plot, map and summary
      tabsetPanel(
        type = "tabs",
        tabPanel("Plot", plotlyOutput("scatter_plot"), p("Total annual concentration of parameters")),
        tabPanel("Map", tmapOutput("map"), p("Map of individual basin locations with interactive parameter and concentration information by year")),
        tabPanel("Summary", verbatimTextOutput("summary"), p("Summary of metadata. This is mostly applicable for statistics purposes."))
      ))
  )
)

# Server
server <- function(input, output) {
  # Create a reactive subset of the data based on user input - for plotly data
  filtered_data <- reactive({
    data_subset <- final_wq %>%
      ungroup() %>%
      filter(basin.y %in% input$basin.y) %>%
      filter(parameter %in% input$parameter) %>%
      filter(year %in% input$year) %>%
      filter(total_conc >= input$total_conc[1] &
               total_conc <= input$total_conc[2])
    return(data_subset)
  })

  #create a reactive subset for basin points on map
  filtered_data_map <- reactive({
    final_wq_map %>%
      ungroup() %>%
      #filter(basin.y %in% input$basin) %>%
      filter(parameter %in% input$parameter) %>%
      filter(year %in% input$year) %>%
      filter(total_conc >= input$total_conc[1] &
               total_conc <= input$total_conc[2]) %>%
      st_as_sf()
  })

  output$scatter_plot <- renderPlotly({
    if (nrow(filtered_data()) == 0)
      return(NULL)

    plot_ly(filtered_data()) %>%
      add_trace(
        x = ~ year,
        y = ~ total_conc,
        name = ~ basin.y,
        type = "scatter",
        mode = "lines+markers"
      ) %>%
      layout(
        yaxis = list(
          title = paste("Total Concentration (mg/l)"),
          range = c(0, max(filtered_data()$total_conc))
        ),
        xaxis = list(range = c(
          min(filtered_data()$year), max(filtered_data()$year)
        ),
        showgrid = TRUE),
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          x = 0.01,
          y = 1.2
        )
      )
  })

  #render Tmap with points
  output$map <- renderTmap({
    tm_shape(filtered_data_map()) +
      tm_dots(
        col = "blue",
        size = 0.1,
        style = "cat",
        title = "Basin locations",
        popup.vars = c(
          "Parameter" = "parameter",
          "Basin" = "basin.y",
          "Total Concentration (mg / l)" = "total_conc"
        )

      )

  })

  output$summary <- renderPrint({
    summary(filtered_data())
  })

}

# Run the app
shinyApp(ui, server)

