#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# read in data
final_wq <- readRDS("data/final_wq.RDS")

# set tmap mode to interactive
tmap_mode("view")


#install.packages(c("shiny", "shinythemes", "tmap", "sf", "ggplot2", "plotly"))
library(shiny)
library(shinythemes)
library(tmap)
library(sf)
library(ggplot2)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Colorado Water Quality"),
  h5(
    "In this app you can filter annual occurrences by basin, type of parameter, and concentration. You can also click on individual occurrences to view metadata."
  ),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "basin.y",
        label = "Basin",
        choices = list(
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
        )
      ),
      checkboxGroupInput(
        inputId = "parameter",
        label = "Parameter Type",
        choiceNames = list(
          "Calcium",
          "Chloride",
          "Magnesium",
          "Potassium",
          "Sodium",
          "Sulfate"
        ),
        choiceValues = list(
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
        )
      ),
      checkboxGroupInput(
        inputId = "year",
        label = "Year",
        choices = list(
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
        )
      ),
      sliderInput(
        inputId = "total_conc",
        label = "Concentration",
        min = 3,
        max = 14000,
        value = c(3, 14000)
      )
    ),
    mainPanel(tmapOutput("map"),
              # New UI element for the bar plot
              plotOutput("bar_plot"))
  )
)

# Server
server <- function(input, output) {
  # Make a reactive object for the final_wq data
  print(final_wq)


  annual_react <- reactive({
    final_wq %>%
      ungroup() %>%
      #filter(basin.y %in% input$basin) %>%
      filter(parameter %in% input$parameter) %>%
      filter(year %in% input$year) %>%
      filter(total_conc >= input$total_conc[1] &
               total_conc <= input$total_conc[2]) %>%
      st_as_sf()
  })

  output$total_conc <- renderPlotly({


    if(nrow(combined()) == 0)
      return(NULL)

    if(input$total_conc == "total_conc"){

      plotly::plot_ly() %>%
        add_bars(x = final_wq()$year, y = final_wq()$total_conc, name = ~ final_wq()$basin.y,
                 color = ~ final_wq()$basin.y) %>%
        plotly::layout(yaxis = list(title = "Total Concentration (mg/l)"),
                       xaxis = list(range = c(input$range[1], input$range[2]),
                                    showgrid = TRUE),
                       showlegend = TRUE,
                       legend = list(orientation = "h", x = 0.01, y = 1.4))
    }else {

      plot_ly(final_wq()) %>%
        add_trace(x = final_wq()$year,
                  y = final_wq()$total_conc,
                  name = ~ final_wq()$basin.y,
                  linetype = ~ final_df()$basin.y,
                  mode = "lines+markers") %>%
        plotly::layout(yaxis = list(title = paste(input$total_conc, "mg/l"), range = list(0, max(final_wq()$total_conc)),
                       xaxis = list(range = c(input$range[1], input$range[2]),
                                    showgrid = T),
                       showlegend = TRUE,
                       legend = list(orientation = "h", x = 0.01, y = 1.2)))


    }


    plotly::plot_ly() %>%
      add_bars(x = combined()$year, y = combined()$total_conc, name = ~combined()$basin.y,
               color = ~ combined()$basin.y) %>%
      plotly::layout(yaxis = list(title = "Total_conc (mg/l)"),
                     xaxis = list(range = c(input$range[1], input$range[2]),
                                  showgrid = TRUE),
                     legend = list(orientation = "h", x = 0.01, y = 1.4))

  })


  output$map <- renderTmap({
    tm_shape(annual_react()) +
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

  }

# Run the Shiny app
 shinyApp(ui, server)

