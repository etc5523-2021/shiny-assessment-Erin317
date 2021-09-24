library(shiny)
library(DT)
library(tidyverse)
library(plotly)
library(here)
library(rworldmap)
library(crosstalk)
options(scipen=999)

# once you've prepared the data uncomment this line
tidy_fuels <- read_csv(here("data", "cooking.csv"))
tidy_fuels$tooltip <- glue::glue_data(tidy_fuels,
                                      "Country: {country}",
                                      "\nYear: {year}",
                                      "\nPopulation: {scales::label_number_auto()(total_population)}",
                                      "\nProportion: {scales::percent(cooking, scale = 1, accuracy = 1)}",
                                      "\nGDP: {scales::dollar(gdp_per_capita)}")


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
          # offset = 1,
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
  ),
  includeCSS("styles.css")
)

server <- function(input, output, session) {
  # Define reactive expressions here for filtering data

# plot data
  selected_args <- reactive({
    if(input$small_countries){
      tidy_fuels %>%
        filter(year == input$year,
               total_population > 1000000)
    }
    else{
      tidy_fuels %>%
        filter(year == input$year)
    }
  })


# table data
  dytable_args <- reactive({
    # if(!is.null(input$countries)) tidy_fuels <- tidy_fuels %>% filter(country %in% input$countries)
     if(input$small_countries){
       tidy_fuels %>% filter(year %in% c(2000:input$year),
                             total_population > 1000000)
     } else{
       tidy_fuels %>% filter(year %in% c(2000:input$year))
     }
  })


  # Define outputs here
  output$chart <- renderPlotly({
    # set palette
    mypalette <- c("Asia" = "#B5DEFF",
                   "Europe" = "#FFEF78",
                   "Africa" = "#EB92BE",
                   "North America" = "#CAB8FF",
                   "South America" = "#FFC069",
                   "Oceania" = "#B3E283")
    # four situations
    if(input$linear_scale){
      # linear plot
      plot <- selected_args() %>%
        highlight_key(~country,"Select Countries") %>%
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
        scale_color_manual(values = mypalette)

      p <- highlight(ggplotly(plot, tooltip = "text") %>%
        config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "zoom2d", "pan2d")),
        selectize = TRUE, persistent = TRUE)
      p

    }
    else{
      # log plot
      plot <- selected_args() %>%
        highlight_key(~country,"Select Countries") %>%
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
        scale_color_manual(values = mypalette)

      p <- highlight(ggplotly(plot, tooltip = "text") %>%
                       config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "zoom2d", "pan2d")),
                     selectize = TRUE, persistent = TRUE)
      p
    }
  })

  output$table <- renderDataTable({
   dytable <- dytable_args() %>%
     mutate(cooking = format(round(cooking,2),nsmall = 2),
            total_population = scales::comma(total_population, accuracy = 1000),
            gdp_per_capita = format(round(gdp_per_capita,2),nsmall = 2)) %>%
     group_by(country) %>%
     arrange(-year) %>%
     select(-tooltip,-code)
   table <- DT::datatable(dytable,
                  rownames = FALSE,
                  options = list(initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#4B6587',
                            'color': 'white'});","}")),
                  colnames = c("Cotinent", "Country", "Year","Cooking (%)","GDP/capital ($)", "Population"))
  })
}

runApp(shinyApp(ui, server))
