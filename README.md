
<!-- README.md is generated from README.Rmd. Please edit that file -->

# airPb

The goal of the airPb package is to easily and reproducibly assess
exposure to airborne lead at specific locations in and around
Cincinnati, Ohio. The package calculates predictions of air lead
exposure from a land use random forest model developed by Dr. Cole
Brokamp based on ambient air sampling in Cincinnati, OH between 2001 and
2005. \#’ The model predictors include greenspace (NDVI) within 1000
meters, population density within 500 meters, length of bus routes
within 900 meters, percent pasure within 800 meters, percent developed
open land within 1100 meters, percent developed medium land within 400
meters, percent developed low land within 900 meters, and percent
developed high land within 1500 meters.

Additionally, these air lead exposures can be adjusted to account for
the temporal variation associated with changing air lead levels in the
area over time. Scaling factors are constructed using measurements of
airborne lead recorded by the EPA in the Cincinnati area. These scaling
factors are the average air lead measured over a time period of interest
(e.g., gestation, the month leading up to date of hospitilization, etc)
divided by the average air lead recorded over the ambient air sampling
period (2001 to 2005). Scaling factors are then applied to air lead
estimates from the land use model.

## Reference

Cole Brokamp, Roman Jandarov, MB Rao, Grace LeMasters, Patrick Ryan.
Exposure assessment models for elemental components of particulate
matter in an urban environment: A comparison of regression and random
forest approaches. Atmospheric Environment. 151. 1-11. 2017.
<http://dx.doi.org/10.1016/j.atmosenv.2016.11.066>

## Installation

airPb is hosted on GitHub; install with:

``` r
remotes::install_github('erikarasnick/airPb')
```

## Examples

``` r
library(airPb)
library(tidyverse)
#> ── Attaching packages ───────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──
#> ✔ ggplot2 3.2.1     ✔ purrr   0.3.3
#> ✔ tibble  2.1.3     ✔ dplyr   0.8.3
#> ✔ tidyr   1.0.0     ✔ stringr 1.4.0
#> ✔ readr   1.3.1     ✔ forcats 0.4.0
#> ── Conflicts ──────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
```

### Example 1: Using `calculate_airPb()` and `calculate_scaling_factors()` and manually applying scaling factors to air lead estimates.

``` r
d <- tibble::tribble(
  ~id,         ~lon,        ~lat,
    809089L, -84.69127387, 39.24710734,
    813233L, -84.47798287, 39.12005904,
    814881L, -84.47123583,  39.2631309,
    799697L, -84.41741798, 39.18541228,
    799698L, -84.41395064, 39.18322447
  )

my_dates <- data.frame(start_date = as.Date(c("2010-01-08", "2012-06-08", "2010-01-09", "2015-04-09", "2010-01-10")),
                       end_date = as.Date(c("2010-02-08", "2012-07-08", "2010-02-09", "2015-05-09", "2010-02-10")))

d %>% 
  mutate(airPb = calculate_airPb(. , return.LU.vars = FALSE), 
         scaling_factors = calculate_scaling_factors(my_dates), 
         scaled_airPb = airPb * scaling_factors)
#> # A tibble: 5 x 6
#>       id   lon   lat airPb scaling_factors scaled_airPb
#>    <int> <dbl> <dbl> <dbl>           <dbl>        <dbl>
#> 1 809089 -84.7  39.2 1.29            0.588        0.757
#> 2 813233 -84.5  39.1 1.14            0.187        0.213
#> 3 814881 -84.5  39.3 0.934           0.441        0.412
#> 4 799697 -84.4  39.2 1.03            0.401        0.415
#> 5 799698 -84.4  39.2 1.05            0.441        0.463
```

### Example 2: Using the `calculate_scaled_airPb()` wrapper function to automatically apply scaling factors to air lead estimates.

A common use case is calculating monthly exposures. For example, we may
have a pair of coordinates recorded annually for each participant. In
the data below, we have 2 unique ids, with lat/lon recorded once per
year.

``` r
d <- tibble::tribble(
  ~id,         ~lon,        ~lat,        ~date,
    809089L, -84.69127387, 39.24710734, as.Date("2010-01-08"),
    809089L, -84.69127387, 39.24710734, as.Date("2011-01-08"),
    809089L, -84.69127387, 39.24710734, as.Date("2012-01-08"),
    799697L, -84.41741798, 39.18541228, as.Date("2011-01-10"),
    799697L, -84.41741798, 39.18541228, as.Date("2012-02-10")
  )
```

We want to scale the air lead measurements to monthly exposures between
these dates, but we need `start_date` and `end_date` columns that
represent the monthy time periods we want to average over.

``` r
d <- d %>% 
  mutate(from = date,
         to = from + lubridate::years(1)) %>% 
  group_by(id, date) %>% 
  nest() %>% 
  mutate(dates = map(data, ~seq.Date(from = .x$from, 
                                     to = .x$to, 
                                     by = '3 months'))) %>% 
  unnest(cols=c('data', 'dates')) %>% 
  dplyr::select(-from, -to) %>% 
  rename(start_date = dates) %>% 
  mutate(end_date = lead(start_date)) %>% 
  filter(!is.na(end_date)) %>% 
  ungroup()

d %>% 
  mutate(scaled_airPb = calculate_scaled_airPb(.))
#> # A tibble: 20 x 7
#>        id date         lon   lat start_date end_date   scaled_airPb
#>     <int> <date>     <dbl> <dbl> <date>     <date>            <dbl>
#>  1 809089 2010-01-08 -84.7  39.2 2010-01-08 2010-04-08        0.549
#>  2 809089 2010-01-08 -84.7  39.2 2010-04-08 2010-07-08        0.535
#>  3 809089 2010-01-08 -84.7  39.2 2010-07-08 2010-10-08        1.07 
#>  4 809089 2010-01-08 -84.7  39.2 2010-10-08 2011-01-08        0.765
#>  5 809089 2011-01-08 -84.7  39.2 2011-01-08 2011-04-08        0.380
#>  6 809089 2011-01-08 -84.7  39.2 2011-04-08 2011-07-08        0.495
#>  7 809089 2011-01-08 -84.7  39.2 2011-07-08 2011-10-08        0.612
#>  8 809089 2011-01-08 -84.7  39.2 2011-10-08 2012-01-08        0.441
#>  9 809089 2012-01-08 -84.7  39.2 2012-01-08 2012-04-08        0.538
#> 10 809089 2012-01-08 -84.7  39.2 2012-04-08 2012-07-08        0.389
#> 11 809089 2012-01-08 -84.7  39.2 2012-07-08 2012-10-08        0.263
#> 12 809089 2012-01-08 -84.7  39.2 2012-10-08 2013-01-08        0.567
#> 13 799697 2011-01-10 -84.4  39.2 2011-01-10 2011-04-10        0.292
#> 14 799697 2011-01-10 -84.4  39.2 2011-04-10 2011-07-10        0.405
#> 15 799697 2011-01-10 -84.4  39.2 2011-07-10 2011-10-10        0.536
#> 16 799697 2011-01-10 -84.4  39.2 2011-10-10 2012-01-10        0.317
#> 17 799697 2012-02-10 -84.4  39.2 2012-02-10 2012-05-10        0.361
#> 18 799697 2012-02-10 -84.4  39.2 2012-05-10 2012-08-10        0.178
#> 19 799697 2012-02-10 -84.4  39.2 2012-08-10 2012-11-10        0.260
#> 20 799697 2012-02-10 -84.4  39.2 2012-11-10 2013-02-10        0.691
```
