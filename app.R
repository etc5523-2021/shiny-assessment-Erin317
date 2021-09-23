library(shiny)
library(DT)
library(tidyverse)
library(plotly)
library(here)
library(RColorBrewer)
options(scipen=999)

# once you've prepared the data uncomment this line
tidy_fuels <- read_csv(here("data", "cooking.csv"))
tidy_fuels$tooltip <- glue::glue_data(tidy_fuels,
                                      "Country: {country}",
                                      "\nYear: {year}",
                                      "\nPopulation: {scales::label_number_auto()(total_population)}",
                                      "\nProportion: {scales::percent(cooking, scale = 1, accuracy = 1)}",
                                      "\nGDP: {scales::dollar(gdp_per_capita)}")
# fuels_dtable <- tidy_fuels %>%




# you might want to use highlight_key here

ui <- fluidPage(
  title = "Indoor Air Pollution",
  tabsetPanel(
    tabPanel("chart",
      icon = icon("line-chart"),
      fluidRow(
        column(
          2,

          checkboxInput("linear_scale",
                        "Linearize x-axis",
                        value = FALSE
          )
        ),
        column(
          6,
          offset = 1,
          # also possible to use plotly here
          selectizeInput("countries", "Select Countries",
            choices = unique(tidy_fuels$country),
            multiple = TRUE
          )
        ),
        column(
          2,
          offset = 1,
          checkboxInput("small_countries",
            "Hide countries < 1 million",
            value = FALSE
          )
        )
      ),
      plotlyOutput("chart", width = "100%"),
      sliderInput("year",
        "Year",
        min = 2000,
        max = 2016,
        value = 2016,
        sep = "",
        width = "100%"
      )
    ),
    tabPanel("table", dataTableOutput("table"), icon = icon("table")),
    tabPanel("about", icon = icon("question"))
  )
)

server <- function(input, output, session) {
  # Define reactive expressions here for filtering data
  chart_plot <- reactive({
    country_selected <- input$countries
    if(is.null(country_selected)) country_selected <- unique(tidy_fuels$country)
    tidy_fuels %>%
      filter(year == input$year)
             #country == country_selected)
  })


  output$chart <- renderPlotly({
    if(input$linear_scale){
      if(input$small_countries){
        # linear and small plot
        plot1_small <- chart_plot() %>%
          filter(total_population > 1000000) %>%
          highlight_key(~country) %>%
          ggplot(aes(gdp_per_capita,
                     cooking,
                     color = continent,
                     size = total_population,
                     text = tooltip)) +
          geom_point(alpha = .7)+
          scale_y_continuous(labels = scales::label_percent(scale = 1))+
          labs(title = "Access to clean fuels for cooking vs. GDP per capita, 2000 to 2016",
               x = "GDP per captia($)",
               y = "Access to clean fuels and technologies for cooking",
               size = "Popoulation",
               color = "")+
          scale_size(range = c(1, 10),
                     guide = "none")+
          theme_classic()+
          theme(axis.line = element_line(color = "grey85"),
                axis.ticks = element_line(color = "grey85"))+
          scale_colour_brewer(palette = "Set2")

        p1_small <- ggplotly(plot1_small, tooltip = "text") %>%
          config(displaylogo = FALSE,
                 modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "zoom2d", "pan2d"))
        p1_small

      }
      else{
        # only linear plot
        plot1 <- chart_plot() %>%
          highlight_key(~country) %>%
          ggplot(aes(gdp_per_capita,
                     cooking,
                     color = continent,
                     size = total_population,
                     text = tooltip)) +
          geom_point(alpha = .7)+
          scale_y_continuous(labels = scales::label_percent(scale = 1))+
          labs(title = "Access to clean fuels for cooking vs. GDP per capita, 2000 to 2016",
               x = "GDP per captia($)",
               y = "Access to clean fuels and technologies for cooking",
               size = "Popoulation",
               color = "")+
          scale_size(range = c(1, 10),
                     guide = "none")+
          theme_classic()+
          theme(axis.line = element_line(color = "grey85"),
                axis.ticks = element_line(color = "grey85"))+
          scale_colour_brewer(palette = "Set2")

        p1 <- ggplotly(plot1, tooltip = "text") %>%
          config(displaylogo = FALSE,
                 modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "zoom2d", "pan2d"))
        p1
      }
    }
    else{
      if(input$small_countries){
        # log and small plot
        plot2_small <- chart_plot() %>%
          filter(total_population > 1000000) %>%
          highlight_key(~country) %>%
          ggplot(aes(gdp_per_capita,
                     cooking,
                     color = continent,
                     size = total_population,
                     text = tooltip)) +
          geom_point(alpha = .7)+
          scale_x_log10()+
          scale_size_continuous(trans = "log10") +
          scale_y_continuous(labels = scales::label_percent(scale = 1))+
          labs(title = "Access to clean fuels for cooking vs. GDP per capita, 2000 to 2016",
               x = "GDP per captia($)",
               y = "Access to clean fuels and technologies for cooking",
               size = "Popoulation",
               color = "")+
          scale_size(range = c(1, 10),
                     guide = "none")+
          theme_classic()+
          theme(axis.line = element_line(color = "grey85"),
                axis.ticks = element_line(color = "grey85"))+
          scale_colour_brewer(palette = "Set2")

        p2_small <- ggplotly(plot2_small, tooltip = "text") %>%
          config(displaylogo = FALSE,
                 modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "zoom2d", "pan2d"))
        p2_small
      }
      else{
        # only log plot
        plot2 <- chart_plot() %>%
          highlight_key(~country) %>%
          ggplot(aes(gdp_per_capita,
                     cooking,
                     color = continent,
                     size = total_population,
                     text = tooltip)) +
          geom_point(alpha = .7)+
          scale_x_log10()+
          scale_size_continuous(trans = "log10") +
          scale_y_continuous(labels = scales::label_percent(scale = 1))+
          labs(title = "Access to clean fuels for cooking vs. GDP per capita, 2000 to 2016",
               x = "GDP per captia($)",
               y = "Access to clean fuels and technologies for cooking",
               size = "Popoulation",
               color = "")+
          scale_size(range = c(1, 10),
                     guide = "none")+
          theme_classic()+
          theme(axis.line = element_line(color = "grey85"),
                axis.ticks = element_line(color = "grey85"))+
          scale_colour_brewer(palette = "Set2")

        p2 <- ggplotly(plot2, tooltip = "text") %>%
          config(displaylogo = FALSE,
                 modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "zoom2d", "pan2d"))
        p2
      }
    }
  })

  output$table <- renderDataTable({
    datatable(tidy_fuels %>%
                filter(year == input$year) %>%
                select(-tooltip))
  })
}

runApp(shinyApp(ui, server))
