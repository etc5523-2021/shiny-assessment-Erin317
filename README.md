
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ETC5523 Shinyapp

Rename the above to something more meaningful.

## Xinying Xu 30464145

This is a template that contains materials to create a shiny gadget
application for assessment 2. You will need the following packages
installed to generate the gadget from this template:

``` r
install.packages(c("shiny", "crosstalk", "plotly", "DT", "tidyverse", "here"))
```

## How to run the app

This is a shiny app to explore the relationship between the wealth of a
country and the proportion of the population that have access to clean
cooking fuels. You can clone or download directly from
`https://github.com/etc5523-2021/shiny-assessment-Erin317` in your local
computer.

To run the shiny app locally, you need the following packages installed
to generate the gadget from this repository:

``` r
install.packages(c("shiny", "crosstalk", "plotly", "DT", "tidyverse", "here"))
```

In the end, use `shiny::runApp()` in the console.

This shiny app was built using many great tools in the R ecosystem.
Thanks to all of the developers of these open source packages:

-   [shiny](https://shiny.rstudio.com/)
-   [plotly](https://plotly.com/)
-   [tidyverse](https://www.tidyverse.org/)
-   [DT](https://rstudio.github.io/DT/)
-   [here](https://here.r-lib.org/)

## Session Info

Add the session info here.

``` r
library(shiny)
library(DT)
#> 
#> Attaching package: 'DT'
#> The following objects are masked from 'package:shiny':
#> 
#>     dataTableOutput, renderDataTable
library(tidyverse)
library(plotly)
#> 
#> Attaching package: 'plotly'
#> The following object is masked from 'package:ggplot2':
#> 
#>     last_plot
#> The following object is masked from 'package:stats':
#> 
#>     filter
#> The following object is masked from 'package:graphics':
#> 
#>     layout
library(here)
#> here() starts at /private/var/folders/cv/mh18n9hj7_z7jq50ymd9nqfw0000gn/T/RtmpjJx4EL/reprex-8c3322143a6-drear-kitty
options(scipen=999)

# once you've prepared the data uncomment this line
tidy_fuels <- read_csv(here("data", "cooking.csv"))
#> Error: '/private/var/folders/cv/mh18n9hj7_z7jq50ymd9nqfw0000gn/T/RtmpjJx4EL/reprex-8c3322143a6-drear-kitty/data/cooking.csv' does not exist.
tidy_fuels$tooltip <- glue::glue_data(tidy_fuels,
                                      "Country: {country}",
                                      "\nYear: {year}",
                                      "\nPopulation: {scales::label_number_auto()(total_population)}",
                                      "\nProportion: {scales::percent(cooking, scale = 1, accuracy = 1)}",
                                      "\nGDP: {scales::dollar(gdp_per_capita)}")
#> Error in glue::glue_data(tidy_fuels, "Country: {country}", "\nYear: {year}", : object 'tidy_fuels' not found



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
          6,
          offset = 1,
          # also possible to use plotly here
          selectizeInput("countries", "Select Countries",
                         choices = unique(tidy_fuels$country),
                         multiple = TRUE)
        ),
        dataTableOutput("table"),
        p(class = 'text-center', downloadButton('tabledata', 'Download Filtered Data')),
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
      fluidRow(
        box(
          title = "About me 🥰",
          width = "6 col-lg-4",
          tags$p(
            "Hi👋! I'm Xinying!"),
          tags$p(
            "I'm a postgraduate student majoring at business analytics from Monash university,",
            "obsessed with data visualization, interactive reporting, data structuring and cleaning,",
            "and statistical and machine learning.",
            "I really love this shinyapp, hope you like it!"
          ),
          tags$p(
            "We'll reproduce interactive graphics from",
            tags$a(href = "https://databank.worldbank.org/reports.aspx?dsid=2&series=EG.CFT.ACCS.ZS", "Our World in Data,", target = "_blank"),
            "on the topic of air pollution.",
            "And the data was collected from ",
            tags$a(href = "https://datacatalog.worldbank.org/dataset/world-development-indicators", "The World Bank,", target = "_blank"),
            "to explore the relationships between the wealth of a country and the proportion of the population that have access to clean cooking fuels, from 2000 to 2016.",
            "You will have the opportunity to explore this here 💖"
          )
        ),
        box(
          title = "About data 📄",
          status = "danger",
          width = "6 col-lg-4",
          tags$p(
            "Access to clean fuels and technologies for cooking is the proportion of total population primarily
              using clean cooking fuels and technologies for cooking. Under WHO guidelines,
              kerosene is excluded from clean cooking fuels."
          ),
          tags$p("Statistical concept and methodology: Data for access to clean fuels and technologies for cooking are based on the the World Health Organization’s (WHO) Global Household Energy Database.
                   They are collected among different sources: only data from nationally representative household surveys (including national censuses) were used.
                   Survey sources include Demographic and Health Surveys (DHS) and Living Standards Measurement Surveys (LSMS), Multi-Indicator Cluster Surveys (MICS), the World Health Survey (WHS),
                   other nationally developed and implemented surveys, and various government agencies (for example, ministries of energy and utilities).
                   To develop the historical evolution of clean fuels and technologies use rates, a multi-level non-parametrical mixed model, using both fixed and random effects,
                   was used to derive polluting fuel use estimates for 150 countries (ref. Bonjour S, Adair-Rohani H, Wolf J, Bruce NG, Mehta S, Prüss-Ustün A, Lahiff M, Rehfuess EA, Mishra V, Smith KR.
                   Solid Fuel Use for Household Cooking: Country and Regional Estimates for 1980-2010. Environ Health Perspect (): .doi:10.1289/ehp.1205987.).
                   For a country with no data, estimates are derived by using regional trends or assumed to be universal access if a country is classified as developed by the United Nations."
          )
        ),
        box(
          title = "About this Shinyapp 🖇",
          # status = "primary",
          width = "6 col-lg-4",
          tags$p(
            class = "text-center",
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
                       src = "https://github.com/rstudio/shiny/blob/master/man/figures/logo.png?raw=true",
                       style = "max-width: 150px; margin-left: 2em;"
              )
            ),
            tags$a(
              href = "https://ggplot2.tidyverse.org/",
              target = "_blank",
              tags$img(class = "image-responsive",
                       src = "https://github.com/tidyverse/ggplot2/blob/master/man/figures/logo.png?raw=true",
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
#> The `name` provided ('line-chart') is deprecated in Font Awesome 5:
#> * please consider using 'chart-line' or 'fas fa-chart-line' instead
#> * use the `verify_fa = FALSE` to deactivate these messages
#> Error in unique(tidy_fuels$country): object 'tidy_fuels' not found


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
                 width="50%",
                 rownames = FALSE,
                 options = list(initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#4B6587',
                            'color': 'white'});","}")),
                 colnames = c("Cotinent", "Country", "Year","Cooking (%)","GDP/capital ($)", "Population"))

  })

  output$tabledata = downloadHandler('cooking-filtered.csv', content = function(file) {
    df <- tidy_fuels %>%
      filter(year %in% input$year2,
             country %in% input$countries) %>%
      select(-tooltip)
    write.csv(df, file)
  })
}

runApp(shinyApp(ui, server))
#> Error in force(ui): object 'ui' not found
```

<sup>Created on 2021-09-25 by the [reprex
package](https://reprex.tidyverse.org) (v2.0.0)</sup>

<details style="margin-bottom:10px;">
<summary>
Session info
</summary>

``` r
sessioninfo::session_info()
#> ─ Session info ───────────────────────────────────────────────────────────────
#>  setting  value                       
#>  version  R version 4.0.3 (2020-10-10)
#>  os       macOS Big Sur 10.16         
#>  system   x86_64, darwin17.0          
#>  ui       X11                         
#>  language (EN)                        
#>  collate  en_US.UTF-8                 
#>  ctype    en_US.UTF-8                 
#>  tz       Asia/Shanghai               
#>  date     2021-09-25                  
#> 
#> ─ Packages ───────────────────────────────────────────────────────────────────
#>  package     * version date       lib source        
#>  assertthat    0.2.1   2019-03-21 [1] CRAN (R 4.0.2)
#>  backports     1.2.1   2020-12-09 [1] CRAN (R 4.0.2)
#>  bit           4.0.4   2020-08-04 [1] CRAN (R 4.0.2)
#>  bit64         4.0.5   2020-08-30 [1] CRAN (R 4.0.2)
#>  broom         0.7.9   2021-07-27 [1] CRAN (R 4.0.2)
#>  bslib         0.3.0   2021-09-02 [1] CRAN (R 4.0.2)
#>  cellranger    1.1.0   2016-07-27 [1] CRAN (R 4.0.2)
#>  cli           3.0.1   2021-07-17 [1] CRAN (R 4.0.2)
#>  colorspace    2.0-2   2021-06-24 [1] CRAN (R 4.0.2)
#>  crayon        1.4.1   2021-02-08 [1] CRAN (R 4.0.2)
#>  data.table    1.14.0  2021-02-21 [1] CRAN (R 4.0.2)
#>  DBI           1.1.1   2021-01-15 [1] CRAN (R 4.0.2)
#>  dbplyr        2.1.1   2021-04-06 [1] CRAN (R 4.0.2)
#>  digest        0.6.27  2020-10-24 [1] CRAN (R 4.0.2)
#>  dplyr       * 1.0.7   2021-06-18 [1] CRAN (R 4.0.2)
#>  DT          * 0.19    2021-09-02 [1] CRAN (R 4.0.2)
#>  ellipsis      0.3.2   2021-04-29 [1] CRAN (R 4.0.2)
#>  evaluate      0.14    2019-05-28 [1] CRAN (R 4.0.1)
#>  fansi         0.5.0   2021-05-25 [1] CRAN (R 4.0.2)
#>  fastmap       1.1.0   2021-01-25 [1] CRAN (R 4.0.2)
#>  fontawesome   0.2.2   2021-07-02 [1] CRAN (R 4.0.2)
#>  forcats     * 0.5.1   2021-01-27 [1] CRAN (R 4.0.2)
#>  fs            1.5.0   2020-07-31 [1] CRAN (R 4.0.2)
#>  generics      0.1.0   2020-10-31 [1] CRAN (R 4.0.2)
#>  ggplot2     * 3.3.5   2021-06-25 [1] CRAN (R 4.0.2)
#>  glue          1.4.2   2020-08-27 [1] CRAN (R 4.0.2)
#>  gtable        0.3.0   2019-03-25 [1] CRAN (R 4.0.2)
#>  haven         2.3.1   2020-06-01 [1] CRAN (R 4.0.2)
#>  here        * 1.0.1   2020-12-13 [1] CRAN (R 4.0.2)
#>  highr         0.9     2021-04-16 [1] CRAN (R 4.0.2)
#>  hms           1.1.0   2021-05-17 [1] CRAN (R 4.0.2)
#>  htmltools     0.5.2   2021-08-25 [1] CRAN (R 4.0.2)
#>  htmlwidgets   1.5.3   2020-12-10 [1] CRAN (R 4.0.2)
#>  httpuv        1.6.2   2021-08-18 [1] CRAN (R 4.0.2)
#>  httr          1.4.2   2020-07-20 [1] CRAN (R 4.0.2)
#>  jquerylib     0.1.4   2021-04-26 [1] CRAN (R 4.0.2)
#>  jsonlite      1.7.2   2020-12-09 [1] CRAN (R 4.0.2)
#>  knitr         1.33    2021-04-24 [1] CRAN (R 4.0.2)
#>  later         1.3.0   2021-08-18 [1] CRAN (R 4.0.2)
#>  lazyeval      0.2.2   2019-03-15 [1] CRAN (R 4.0.2)
#>  lifecycle     1.0.0   2021-02-15 [1] CRAN (R 4.0.2)
#>  lubridate     1.7.10  2021-02-26 [1] CRAN (R 4.0.2)
#>  magrittr      2.0.1   2020-11-17 [1] CRAN (R 4.0.2)
#>  mime          0.11    2021-06-23 [1] CRAN (R 4.0.2)
#>  modelr        0.1.8   2020-05-19 [1] CRAN (R 4.0.2)
#>  munsell       0.5.0   2018-06-12 [1] CRAN (R 4.0.2)
#>  pillar        1.6.2   2021-07-29 [1] CRAN (R 4.0.2)
#>  pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.0.2)
#>  plotly      * 4.9.4.1 2021-06-18 [1] CRAN (R 4.0.2)
#>  promises      1.2.0.1 2021-02-11 [1] CRAN (R 4.0.2)
#>  purrr       * 0.3.4   2020-04-17 [1] CRAN (R 4.0.2)
#>  R6            2.5.1   2021-08-19 [1] CRAN (R 4.0.2)
#>  Rcpp          1.0.7   2021-07-07 [1] CRAN (R 4.0.2)
#>  readr       * 2.0.1   2021-08-10 [1] CRAN (R 4.0.2)
#>  readxl        1.3.1   2019-03-13 [1] CRAN (R 4.0.2)
#>  reprex        2.0.0   2021-04-02 [1] CRAN (R 4.0.2)
#>  rlang         0.4.11  2021-04-30 [1] CRAN (R 4.0.2)
#>  rmarkdown     2.10    2021-08-06 [1] CRAN (R 4.0.2)
#>  rprojroot     2.0.2   2020-11-15 [1] CRAN (R 4.0.2)
#>  rstudioapi    0.13    2020-11-12 [1] CRAN (R 4.0.2)
#>  rvest         1.0.1   2021-07-26 [1] CRAN (R 4.0.2)
#>  sass          0.4.0   2021-05-12 [1] CRAN (R 4.0.2)
#>  scales        1.1.1   2020-05-11 [1] CRAN (R 4.0.2)
#>  sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 4.0.2)
#>  shiny       * 1.7.0   2021-09-22 [1] CRAN (R 4.0.3)
#>  stringi       1.7.4   2021-08-25 [1] CRAN (R 4.0.2)
#>  stringr     * 1.4.0   2019-02-10 [1] CRAN (R 4.0.2)
#>  styler        1.5.1   2021-07-13 [1] CRAN (R 4.0.2)
#>  tibble      * 3.1.4   2021-08-25 [1] CRAN (R 4.0.2)
#>  tidyr       * 1.1.3   2021-03-03 [1] CRAN (R 4.0.2)
#>  tidyselect    1.1.1   2021-04-30 [1] CRAN (R 4.0.2)
#>  tidyverse   * 1.3.1   2021-04-15 [1] CRAN (R 4.0.2)
#>  tzdb          0.1.2   2021-07-20 [1] CRAN (R 4.0.2)
#>  utf8          1.2.2   2021-07-24 [1] CRAN (R 4.0.2)
#>  vctrs         0.3.8   2021-04-29 [1] CRAN (R 4.0.2)
#>  viridisLite   0.4.0   2021-04-13 [1] CRAN (R 4.0.2)
#>  vroom         1.5.4   2021-08-05 [1] CRAN (R 4.0.2)
#>  withr         2.4.2   2021-04-18 [1] CRAN (R 4.0.2)
#>  xfun          0.25    2021-08-06 [1] CRAN (R 4.0.2)
#>  xml2          1.3.2   2020-04-23 [1] CRAN (R 4.0.2)
#>  xtable        1.8-4   2019-04-21 [1] CRAN (R 4.0.2)
#>  yaml          2.2.1   2020-02-01 [1] CRAN (R 4.0.2)
#> 
#> [1] /Library/Frameworks/R.framework/Versions/4.0/Resources/library
```

</details>
