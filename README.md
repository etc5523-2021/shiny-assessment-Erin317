
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ETC5523 Shinyapp

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

<details open>
<summary>
<span title="Click to Expand"> Current session info </span>
</summary>

``` r
─ Session info ─────────────────────────────────────────
 setting  value                       
 version  R version 4.0.3 (2020-10-10)
 os       macOS Big Sur 10.16         
 system   x86_64, darwin17.0          
 ui       RStudio                     
 language (EN)                        
 collate  en_US.UTF-8                 
 ctype    en_US.UTF-8                 
 tz       Asia/Shanghai               
 date     2021-09-25                  

─ Packages ─────────────────────────────────────────────
 package     * version date       lib source        
 assertthat    0.2.1   2019-03-21 [1] CRAN (R 4.0.2)
 backports     1.2.1   2020-12-09 [1] CRAN (R 4.0.2)
 bit           4.0.4   2020-08-04 [1] CRAN (R 4.0.2)
 bit64         4.0.5   2020-08-30 [1] CRAN (R 4.0.2)
 broom         0.7.9   2021-07-27 [1] CRAN (R 4.0.2)
 bslib         0.3.0   2021-09-02 [1] CRAN (R 4.0.2)
 cachem        1.0.6   2021-08-19 [1] CRAN (R 4.0.2)
 cellranger    1.1.0   2016-07-27 [1] CRAN (R 4.0.2)
 cli           3.0.1   2021-07-17 [1] CRAN (R 4.0.2)
 clipr         0.7.1   2020-10-08 [1] CRAN (R 4.0.2)
 colorspace    2.0-2   2021-06-24 [1] CRAN (R 4.0.2)
 crayon        1.4.1   2021-02-08 [1] CRAN (R 4.0.2)
 crosstalk     1.1.1   2021-01-12 [1] CRAN (R 4.0.2)
 data.table    1.14.0  2021-02-21 [1] CRAN (R 4.0.2)
 DBI           1.1.1   2021-01-15 [1] CRAN (R 4.0.2)
 dbplyr        2.1.1   2021-04-06 [1] CRAN (R 4.0.2)
 desc          1.3.0   2021-03-05 [1] CRAN (R 4.0.2)
 details       0.2.1   2020-01-12 [1] CRAN (R 4.0.2)
 digest        0.6.27  2020-10-24 [1] CRAN (R 4.0.2)
 dplyr       * 1.0.7   2021-06-18 [1] CRAN (R 4.0.2)
 DT          * 0.19    2021-09-02 [1] CRAN (R 4.0.2)
 ellipsis      0.3.2   2021-04-29 [1] CRAN (R 4.0.2)
 evaluate      0.14    2019-05-28 [1] CRAN (R 4.0.1)
 fansi         0.5.0   2021-05-25 [1] CRAN (R 4.0.2)
 fastmap       1.1.0   2021-01-25 [1] CRAN (R 4.0.2)
 fontawesome   0.2.2   2021-07-02 [1] CRAN (R 4.0.2)
 forcats     * 0.5.1   2021-01-27 [1] CRAN (R 4.0.2)
 fs            1.5.0   2020-07-31 [1] CRAN (R 4.0.2)
 generics      0.1.0   2020-10-31 [1] CRAN (R 4.0.2)
 ggplot2     * 3.3.5   2021-06-25 [1] CRAN (R 4.0.2)
 glue          1.4.2   2020-08-27 [1] CRAN (R 4.0.2)
 gtable        0.3.0   2019-03-25 [1] CRAN (R 4.0.2)
 haven         2.3.1   2020-06-01 [1] CRAN (R 4.0.2)
 here        * 1.0.1   2020-12-13 [1] CRAN (R 4.0.2)
 hms           1.1.0   2021-05-17 [1] CRAN (R 4.0.2)
 htmltools     0.5.2   2021-08-25 [1] CRAN (R 4.0.2)
 htmlwidgets   1.5.3   2020-12-10 [1] CRAN (R 4.0.2)
 httpuv        1.6.2   2021-08-18 [1] CRAN (R 4.0.2)
 httr          1.4.2   2020-07-20 [1] CRAN (R 4.0.2)
 jquerylib     0.1.4   2021-04-26 [1] CRAN (R 4.0.2)
 jsonlite      1.7.2   2020-12-09 [1] CRAN (R 4.0.2)
 knitr         1.33    2021-04-24 [1] CRAN (R 4.0.2)
 labeling      0.4.2   2020-10-20 [1] CRAN (R 4.0.2)
 later         1.3.0   2021-08-18 [1] CRAN (R 4.0.2)
 lazyeval      0.2.2   2019-03-15 [1] CRAN (R 4.0.2)
 lifecycle     1.0.0   2021-02-15 [1] CRAN (R 4.0.2)
 lubridate     1.7.10  2021-02-26 [1] CRAN (R 4.0.2)
 magrittr      2.0.1   2020-11-17 [1] CRAN (R 4.0.2)
 mime          0.11    2021-06-23 [1] CRAN (R 4.0.2)
 modelr        0.1.8   2020-05-19 [1] CRAN (R 4.0.2)
 munsell       0.5.0   2018-06-12 [1] CRAN (R 4.0.2)
 pillar        1.6.2   2021-07-29 [1] CRAN (R 4.0.2)
 pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.0.2)
 plotly      * 4.9.4.1 2021-06-18 [1] CRAN (R 4.0.2)
 png           0.1-7   2013-12-03 [1] CRAN (R 4.0.2)
 promises      1.2.0.1 2021-02-11 [1] CRAN (R 4.0.2)
 purrr       * 0.3.4   2020-04-17 [1] CRAN (R 4.0.2)
 R6            2.5.1   2021-08-19 [1] CRAN (R 4.0.2)
 Rcpp          1.0.7   2021-07-07 [1] CRAN (R 4.0.2)
 readr       * 2.0.1   2021-08-10 [1] CRAN (R 4.0.2)
 readxl        1.3.1   2019-03-13 [1] CRAN (R 4.0.2)
 reprex        2.0.0   2021-04-02 [1] CRAN (R 4.0.2)
 rlang         0.4.11  2021-04-30 [1] CRAN (R 4.0.2)
 rmarkdown     2.10    2021-08-06 [1] CRAN (R 4.0.2)
 rprojroot     2.0.2   2020-11-15 [1] CRAN (R 4.0.2)
 rsconnect     0.8.24  2021-08-05 [1] CRAN (R 4.0.2)
 rstudioapi    0.13    2020-11-12 [1] CRAN (R 4.0.2)
 rvest         1.0.1   2021-07-26 [1] CRAN (R 4.0.2)
 sass          0.4.0   2021-05-12 [1] CRAN (R 4.0.2)
 scales        1.1.1   2020-05-11 [1] CRAN (R 4.0.2)
 sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 4.0.2)
 shiny       * 1.7.0   2021-09-22 [1] CRAN (R 4.0.3)
 sourcetools   0.1.7   2018-04-25 [1] CRAN (R 4.0.2)
 stringi       1.7.4   2021-08-25 [1] CRAN (R 4.0.2)
 stringr     * 1.4.0   2019-02-10 [1] CRAN (R 4.0.2)
 tibble      * 3.1.4   2021-08-25 [1] CRAN (R 4.0.2)
 tidyr       * 1.1.3   2021-03-03 [1] CRAN (R 4.0.2)
 tidyselect    1.1.1   2021-04-30 [1] CRAN (R 4.0.2)
 tidyverse   * 1.3.1   2021-04-15 [1] CRAN (R 4.0.2)
 tinytex       0.31    2021-03-30 [1] CRAN (R 4.0.2)
 tzdb          0.1.2   2021-07-20 [1] CRAN (R 4.0.2)
 utf8          1.2.2   2021-07-24 [1] CRAN (R 4.0.2)
 vctrs         0.3.8   2021-04-29 [1] CRAN (R 4.0.2)
 viridisLite   0.4.0   2021-04-13 [1] CRAN (R 4.0.2)
 vroom         1.5.4   2021-08-05 [1] CRAN (R 4.0.2)
 withr         2.4.2   2021-04-18 [1] CRAN (R 4.0.2)
 xfun          0.25    2021-08-06 [1] CRAN (R 4.0.2)
 xml2          1.3.2   2020-04-23 [1] CRAN (R 4.0.2)
 xtable        1.8-4   2019-04-21 [1] CRAN (R 4.0.2)
 yaml          2.2.1   2020-02-01 [1] CRAN (R 4.0.2)

[1] /Library/Frameworks/R.framework/Versions/4.0/Resources/library
```

</details>

<br>
