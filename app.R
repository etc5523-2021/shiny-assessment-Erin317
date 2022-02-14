library(shiny)
library(DT)
library(tidyverse)
library(plotly)
library(here)
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
    tabPanel(
      "chart",
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
          2,
          offset = 1,
          checkboxInput("small_countries",
                        "Hide countries < 1 million",
                        value = FALSE
          )
        )
      ),
      plotlyOutput("chart", width = "100%"),
      sliderInput("year1",
                  "Year",
                  min = 2000,
                  max = 2016,
                  value = 2016,
                  sep = "",
                  width = "100%"
      )
    ),
    tabPanel(
      "table",  icon = icon("table"),
      fluidRow(
        column(
          6, offset = 0.5,
          selectizeInput("countries", "Select Countries",
                         choices = unique(tidy_fuels$country),
                         multiple = TRUE)
        ),
        column(
          12,offset = 0.5,
          dataTableOutput("table")
        )
      ),
      sliderInput("year2",
                  "Year",
                  min = 2000,
                  max = 2016,
                  value = 2016,
                  sep = "",
                  width = "100%"
      )
    ),
    tabPanel(
      "about",
      icon = icon("question"),
      fixedRow(
        column(
          10,offset = 1,
          tags$h1("About me 🥰"),
          tags$p(HTML("Hi👋! I'm Xinying!")),
          tags$p(HTML("I'm a postgraduate student majoring at business analytics from Monash university,",
                      "obsessed with data visualization, interactive reporting, data structuring and cleaning,",
                      "and statistical and machine learning.",
                      "I really love this shinyapp, hope you like it!")),
          tags$p(HTML("We'll reproduce interactive graphics from <a href=\"https://databank.worldbank.org/reports.aspx?dsid=2&series=EG.CFT.ACCS.ZS\">Our World in Data</a>. on the topic of air pollution,
                    And the data was collected from <a href = \"https://datacatalog.worldbank.org/dataset/world-development-indicators\">The World Bank</a>,
                    to explore the relationships between the wealth of a country and the proportion of the population that have access to clean cooking fuels, from 2000 to 2016. You will have the opportunity to explore this here 💖")),
          tags$h1("About data 📄"),
          tags$p(HTML("Access to clean fuels and technologies for cooking is the proportion of total population primarily
                    using clean cooking fuels and technologies for cooking. Under WHO guidelines,
                    kerosene is excluded from clean cooking fuels.")),
          tags$p(HTML("Statistical concept and methodology: Data for access to clean fuels and technologies for cooking are based on the the World Health Organization’s (WHO) Global Household Energy Database.
        They are collected among different sources: only data from nationally representative household surveys (including national censuses) were used.
        Survey sources include Demographic and Health Surveys (DHS) and Living Standards Measurement Surveys (LSMS), Multi-Indicator Cluster Surveys (MICS), the World Health Survey (WHS),
        other nationally developed and implemented surveys, and various government agencies (for example, ministries of energy and utilities).
        To develop the historical evolution of clean fuels and technologies use rates, a multi-level non-parametrical mixed model, using both fixed and random effects,
        was used to derive polluting fuel use estimates for 150 countries (ref. Bonjour S, Adair-Rohani H, Wolf J, Bruce NG, Mehta S, Prüss-Ustün A, Lahiff M, Rehfuess EA, Mishra V, Smith KR.
        Solid Fuel Use for Household Cooking: Country and Regional Estimates for 1980-2010.
        For a country with no data, estimates are derived by using regional trends or assumed to be universal access if a country is classified as developed by the United Nations.")),
          tags$h1("About this Shinyapp 🖇"),
          tags$p(
            tags$a(
              href = "https://www.r-project.org",
              target = "_blank",
              tags$img(class = "image-responsive",
                       src = "https://www.r-project.org/logo/Rlogo.svg",
                       style = "max-width: 150px;"
              )
            ),
            tags$a(
              href = "https://shiny.rstudio.com/",
              target = "_blank",
              tags$img(class = "image-responsive",
                       src = "https://github.com/rstudio/shiny/blob/main/man/figures/logo.png",
                       style = "max-width: 150px; margin-left: 2em;"
              )
            ),
            tags$a(
              href = "https://ggplot2.tidyverse.org/",
              target = "_blank",
              tags$img(class = "image-responsive",
                       src = "https://github.com/tidyverse/ggplot2/blob/master/man/figures/logo.png",
                       style = "max-width: 150px; margin-left: 2em;"
              )
            )
          ),
          tags$p(
            "This dashboard was built in",
            tags$a(href = "https://r-project.org", target = "_blank", "R"),
            "and", tags$a(href = "https://rstudio.com", target = "_blank", "RStudio"), "with",
            tags$strong("shiny,"),
            tags$strong("DT,"),
            tags$strong("here,"),
            tags$strong("plotly,"),
            "the", tags$strong("tidyverse,"),
            "and many more packages."
          )
        )
      )
    )
  ),
  includeCSS("styles.css")
)


server <- function(input, output, session) {
  # Define reactive expressions here for filtering data

# for log plot
  selected_args <- reactive({
   # if(!is.null(input$countries)) tidy_fuels <- tidy_fuels %>% filter(country %in% input$countries)
    if(input$small_countries){
      tidy_fuels %>%
        filter(year == input$year1,
               total_population > 1000000)
    }
    else{
      tidy_fuels %>%
        filter(year == input$year1)
    }
  })

# for linear plot

  linear_plot <- reactive({
    # if(!is.null(input$countries)) tidy_fuels <- tidy_fuels %>% filter(country %in% input$countries)
    if(input$small_countries){
      tidy_fuels %>%
        filter(year %in% c(2000:input$year1),
               total_population > 1000000)
    }
    else{
      tidy_fuels %>%
        filter(year %in% c(2000:input$year1))
    }
  })

# table data
  dytable_args <- reactive({
    if(!is.null(input$countries)) tidy_fuels <- tidy_fuels %>% filter(country %in% input$countries)
    if(input$small_countries){
      tidy_fuels %>% filter(year %in% c(2000:input$year2),
                            total_population > 1000000)
    } else{
      tidy_fuels %>% filter(year %in% c(2000:input$year2))
    }
  })

# slider bar
# year1
  observe({
    val <- input$year1
    updateSliderInput(session, "year2",value = val,
                      min = 2000, max = 2016)
  })
# year2
  observe({
    val <- input$year2
    updateSliderInput(session, "year1",value = val,
                      min = 2000, max = 2016)
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
      plot <- linear_plot() %>%
        highlight_key(~country, "Select Countries") %>%
        ggplot(aes(gdp_per_capita,
                   cooking,
                   color = continent,
                   text = tooltip)) +
        geom_line(aes(group = country),
                  alpha = .7, size = 1, arrow = arrow())+
        geom_point(size = 0.5, alpha = 0.8)+
        scale_x_continuous(labels = scales::label_dollar())+
        scale_y_continuous(labels = scales::label_percent(scale = 1))+
        labs(x = "GDP per captia($)",
             y = "Access to clean fuels and technologies for cooking",
             size = "Popoulation",
             color = "")+
        ggtitle(paste0("Access to clean fuels for cooking vs. GDP per capita, ",input$year1))+
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
        scale_x_log10(labels = scales::label_dollar())+
        scale_size_continuous(trans = "log10") +
        scale_y_continuous(labels = scales::label_percent(scale = 1))+
        labs(title = paste0("Access to clean fuels for cooking vs. GDP per capita, ",input$year1),
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
     mutate(cooking = round(cooking,2),
            total_population = scales::comma(total_population, accuracy = 1000),
            gdp_per_capita = round(gdp_per_capita,3)) %>%
     group_by(country) %>%
     arrange(-year) %>%
     select(-tooltip,-code)
   DT::datatable(dytable,
                 rownames = FALSE,
                 extensions = 'Buttons',
                 options = list(initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#4B6587',
                            'color': 'white'});","}"),
                      dom = 'Bfrtip',
                      buttons = c('csv', 'excel', 'pdf')),
                 colnames = c("Cotinent", "Country", "Year","Cooking (%)","GDP/capital ($)", "Population"))

  })

}

runApp(shinyApp(ui, server))
