
<!-- README.md is generated from README.Rmd. Please edit that file -->

# frictionless

<!-- badges: start -->

[![Funding](https://img.shields.io/static/v1?label=powered+by&message=lifewatch.be&labelColor=1a4e8a&color=f15922)](http://lifewatch.be)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/frictionless)](https://CRAN.R-project.org/package=frictionless)
[![R-CMD-check](https://github.com/frictionlessdata/frictionless-r/workflows/R-CMD-check/badge.svg)](https://github.com/frictionlessdata/frictionless-r/actions)
[![codecov](https://codecov.io/gh/frictionlessdata/frictionless-r/branch/main/graph/badge.svg?token=bKtiHW21K0)](https://codecov.io/gh/frictionlessdata/frictionless-r)
<!-- badges: end -->

Frictionless is an R package to read and write [Frictionless Data
Packages](https://specs.frictionlessdata.io/data-package/) in a
[Tidyverse](https://www.tidyverse.org/) way.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("frictionlessdata/frictionless-r")
```

## Usage

### Read data from a local Data Package

You can read data from a Data Package using the two read functions
(`read_package()`, then `read_resource()`):

``` r
library(frictionless)

# Read a datapackage.json file
package <- read_package(system.file("extdata", "datapackage.json", package = "frictionless"))

# List the resource names
package$resource_names
#> [1] "deployments"  "observations" "media"

# Read data from the resource "observations"
read_resource(package, "observations")
#> # A tibble: 8 × 7
#>   observation_id deployment_id timestamp           scientific_name     count
#>   <chr>          <chr>         <dttm>              <chr>               <dbl>
#> 1 1-1            1             2020-09-28 00:13:07 Capreolus capreolus     1
#> 2 1-2            1             2020-09-28 15:59:17 Capreolus capreolus     1
#> 3 1-3            1             2020-09-28 16:35:23 Lepus europaeus         1
#> 4 1-4            1             2020-09-28 17:04:04 Lepus europaeus         1
#> 5 1-5            1             2020-09-28 19:19:54 Sus scrofa              2
#> 6 2-1            2             2021-10-01 01:25:06 Sus scrofa              1
#> 7 2-2            2             2021-10-01 01:25:06 Sus scrofa              1
#> 8 2-3            2             2021-10-01 04:47:30 Sus scrofa              1
#> # … with 2 more variables: life_stage <fct>, comments <chr>
```

### Read data from an online Data Package

The read functions can also be used to access all data from an online
Frictionless Data Package (e.g. one published on
[Zenodo](https://doi.org/10.5281/zenodo.5070086)) via the
`datapackage.json` URL:

``` r
# Read the datapackage.json file: this will give you access to all Data 
# Resources without reading them, which is convenient and fast
package <- read_package("https://zenodo.org/record/5070086/files/datapackage.json")
#> Please make sure you have the right to access data from this Data Package for your proposed use.
#> Follow applicable norms or requirements to credit the dataset and its authors.
#> For more information, see https://doi.org/10.5281/zenodo.5070086

# List the resource names
package$resource_names
#> [1] "reference-data" "gps"            "acceleration"

# Read the gps data: will return a single data frame, even though the data are 
# split over multiple csv files
read_resource(package, "gps")
#> # A tibble: 73,047 × 21
#>     `event-id` visible timestamp           `location-long` `location-lat`
#>          <dbl> <lgl>   <dttm>                        <dbl>          <dbl>
#>  1 14256075762 TRUE    2018-05-25 16:11:37            4.25           51.3
#>  2 14256075763 TRUE    2018-05-25 16:16:41            4.25           51.3
#>  3 14256075764 TRUE    2018-05-25 16:21:29            4.25           51.3
#>  4 14256075765 TRUE    2018-05-25 16:26:28            4.25           51.3
#>  5 14256075766 TRUE    2018-05-25 16:31:21            4.25           51.3
#>  6 14256075767 TRUE    2018-05-25 16:36:09            4.25           51.3
#>  7 14256075768 TRUE    2018-05-25 16:40:57            4.25           51.3
#>  8 14256075769 TRUE    2018-05-25 16:45:55            4.25           51.3
#>  9 14256075770 TRUE    2018-05-25 16:50:49            4.25           51.3
#> 10 14256075771 TRUE    2018-05-25 16:55:36            4.25           51.3
#> # … with 73,037 more rows, and 16 more variables:
#> #   bar:barometric-pressure <dbl>, external-temperature <dbl>, gps:dop <dbl>,
#> #   gps:satellite-count <dbl>, gps-time-to-fix <dbl>, ground-speed <dbl>,
#> #   heading <dbl>, height-above-msl <dbl>, location-error-numerical <dbl>,
#> #   manually-marked-outlier <lgl>, vertical-error-numerical <dbl>,
#> #   sensor-type <chr>, individual-taxon-canonical-name <chr>,
#> #   tag-local-identifier <chr>, individual-local-identifier <chr>, …
```

For more functionality, see the [function
reference](https://frictionlessdata.github.io/frictionless-r/reference/index.html).
