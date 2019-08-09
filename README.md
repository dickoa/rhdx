
# rhdx

<!-- README.md is generated from README.Rmd. Please edit that file -->

[![GitLab CI Build
Status](https://gitlab.com/dickoa/rhdx/badges/master/build.svg)](https://gitlab.com/dickoa/rhdx/pipelines)
[![Codecov Code
Coverage](https://codecov.io/gl/dickoa/rhdx/branch/master/graph/badge.svg)](https://codecov.io/gl/dickoa/rhdx)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

`rhdx` is an R client for the Humanitarian Exchange Data platform.

## Introduction

The [Humanitarian Data Exchange platform](https://data.humdata.org/) is
the open platform to easily find and analyze humanitarian data.

## Installation

This package is not on yet on CRAN and to install it, you will need the
[`remotes`](https://github.com/r-lib/remotes) package. You can get
`rhdx` from Gitlab or Github (mirror)

``` r
## install.packages("remotes")
remotes::install_gitlab("dickoa/rhdx")
remotes::install_github("dickoa/rhdx")
```

## rhdx: A quick tutorial

``` r
library("rhdx")
```

The first step is to connect to HDX using the `set_rhdx_config` function
and check the config using `get_rhdx_config`

``` r
set_rhdx_config(hdx_site = "prod")
get_rhdx_config()
## <HDX Configuration>
##   HDX site: prod
##   HDX site url: https://data.humdata.org/
##   HDX API key:
```

Now that we are connected to HDX, we can search for dataset using
`search_datasets`, access resources withini the dataset page with the
`get_resources` function and finally read the data directly into the `R`
session using `read_session`. `magrittr` pipes operator are also
supported

``` r
library(tidyverse)
search_datasets("ACLED Mali", rows = 2) %>% ## search dataset in HDX, limit the results to two rows
  pluck(1) %>% ## select the first dataset
  get_resource(1) %>% ## pick the first resource
  read_resource() ## read this HXLated data into R
## # A tibble: 2,516 x 30
##    data_id   iso event_id_cnty event_id_no_cnty event_date  year
##  *   <dbl> <dbl> <chr>                    <dbl> <date>     <dbl>
##  1 2942561   466 MLI2605                   2605 2019-01-26  2019
##  2 2942562   466 MLI2606                   2606 2019-01-26  2019
##  3 2942557   466 MLI2601                   2601 2019-01-25  2019
##  4 2942558   466 MLI2602                   2602 2019-01-25  2019
##  5 2942559   466 MLI2603                   2603 2019-01-25  2019
##  6 2942560   466 MLI2604                   2604 2019-01-25  2019
##  7 2942555   466 MLI2599                   2599 2019-01-24  2019
##  8 2942556   466 MLI2600                   2600 2019-01-24  2019
##  9 2942553   466 MLI2597                   2597 2019-01-23  2019
## 10 2942554   466 MLI2598                   2598 2019-01-23  2019
## # … with 2,506 more rows, and 24 more variables:
## #   time_precision <dbl>, event_type <chr>, actor1 <chr>,
## #   assoc_actor_1 <chr>, inter1 <dbl>, actor2 <chr>,
## #   assoc_actor_2 <chr>, inter2 <dbl>, interaction <dbl>,
## #   region <chr>, country <chr>, admin1 <chr>, admin2 <chr>,
## #   admin3 <chr>, location <chr>, latitude <dbl>,
## #   longitude <dbl>, geo_precision <dbl>, source <chr>,
## #   source_scale <chr>, notes <chr>, fatalities <dbl>,
## #   timestamp <dbl>, iso3 <chr>
```

`read_resource` will not work with resources in HDX, so far the
following format are supported: `csv`, `xlsx`, `xls`, `json`, `geojson`,
`zipped shapefile`, `kmz`, `zipped geodatabase` and `zipped geopackage`.
I will consider adding more data types in the future, feel free to file
an issue if it doesn’t work as expected or you want to add a support for
a format.

## A step by step tutorial to getting data from rhdx

### Connect to a server

In order to connect to HDX, we can use the `set_rhdx_config` function

``` r
set_rhdx_config(hdx_site = "prod")
```

### Search datasets

Once a server is chosen, we can now search from dataset using the
`search_datasets` In this case we will limit just to two results (`rows`
parameter).

``` r
list_of_ds <- search_datasets("displaced Nigeria", rows = 2)
list_of_ds
## [[1]]
## <HDX Dataset> 4fbc627d-ff64-4bf6-8a49-59904eae15bb
##   Title: Nigeria - Internally displaced persons - IDPs
##   Name: idmc-idp-data-for-nigeria
##   Date: 01/01/2009-12/31/2016
##   Tags (up to 5): displacement, idmc, population
##   Locations (up to 5): nga
##   Resources (up to 5): displacement_data, conflict_data, disaster_data

## [[2]]
## <HDX Dataset> 4adf7874-ae01-46fd-a442-5fc6b3c9dff1
##   Title: Nigeria Baseline Assessment Data [IOM DTM]
##   Name: nigeria-baseline-data-iom-dtm
##   Date: 01/31/2018
##   Tags (up to 5): adamawa, assessment, baseline-data, baseline-dtm, bauchi
##   Locations (up to 5): nga
##   Resources (up to 5): DTM Nigeria Baseline Assessment Round 21, DTM Nigeria Baseline Assessment Round 20, DTM Nigeria Baseline Assessment Round 19, DTM Nigeria Baseline Assessment Round 18, DTM Nigeria Baseline Assessment Round 17
```

### Choose the dataset you want to manipulate in R, in this case we will take the first one.

The result of `search_datasets` is a list of HDX datasets, you can
manipulate this list like any other `list` in `R`. We can use
`purrr::pluck` to select the element we want in our list, here it is the
first.

``` r
ds <- pluck(list_of_ds, 1)
ds
## <HDX Dataset> 4fbc627d-ff64-4bf6-8a49-59904eae15bb
##   Title: Nigeria - Internally displaced persons - IDPs
##   Name: idmc-idp-data-for-nigeria
##   Date: 01/01/2009-12/31/2016
##   Tags (up to 5): displacement, idmc, population
##   Locations (up to 5): nga
##   Resources (up to 5): displacement_data, conflict_data, disaster_data
```

### List all resources in the dataset

With our dataset, the next step is to list all the resources. If you are
not familiar with CKAN terminology, `resources` refer to the actual
files shared in a dataset page and you can download. Each dataset page
contains one or more resources.

``` r
get_resources(ds)
## [[1]]
## <HDX Resource> f57be018-116e-4dd9-a7ab-8002e7627f36
##   Name: displacement_data
##   Description: Internally displaced persons - IDPs (new displacement associated with conflict and violence)
##   Size:
##   Format: JSON

## [[2]]
## <HDX Resource> 6261856c-afb9-4746-b340-9cf531cbd38f
##   Name: conflict_data
##   Description: Internally displaced persons - IDPs (people displaced by conflict and violence)
##   Size:
##   Format: JSON

## [[3]]
## <HDX Resource> b8ff1f4b-105c-4a6c-bf54-a543a486ab7e
##   Name: disaster_data
##   Description: Internally displaced persons - IDPs (new displacement associated with disasters)
##   Size:
##   Format: JSON
```

### Choose a resource we need to download/read

For this example, we are looking for the displacement data and it’s the
first resource in the dataset page. We can use `pluck` on the list of
resources or the helper function `get_resource(resource,
resource_index)` to select the resource we want to use. The selected
resource can be then downloaded and store for further use or directly
read into your R session using the `read_session` function. The resource
is a `json` file and it can be read directly using `jsonlite` package,
we added a `simplify_json` option to get a `vector` or a `data.frame`
when possible instead of a `list`.

``` r
idp_nga_rs <- get_resource(ds, 1)
idp_nga_df <- read_resource(idp_nga_rs, simplify_json = TRUE, folder = tempdir())
idp_nga_df
## $results
##   iso iso3 geo_name year conflict_new_displacements
## 1  NG  NGA  Nigeria 2009                       5000
## 2  NG  NGA  Nigeria 2010                       5000
## 3  NG  NGA  Nigeria 2011                      65000
## 4  NG  NGA  Nigeria 2012                      63000
## 5  NG  NGA  Nigeria 2013                     471000
## 6  NG  NGA  Nigeria 2014                     975000
## 7  NG  NGA  Nigeria 2015                     737000
## 8  NG  NGA  Nigeria 2016                     501000
##   disaster_new_displacements conflict_stock_displacement
## 1                     140000                          NA
## 2                     560000                          NA
## 3                       6300                          NA
## 4                    6112000                          NA
## 5                     117000                     3300000
## 6                       3000                     1075000
## 7                     100000                     2096000
## 8                      78000                     1955000

## $lookups
## named list()

## $errors
## list()

## $success
## [1] TRUE

## $params
## [1] "/api/displacement_data?iso3=NGA&ci=HDX00AKEYJUl17"

## $total
## [1] 8

## $limit
## [1] 0

## $offset
## [1] 0
```

### Using `magrittr` pipe

All these operations can be chained using pipes `%>%` and allow for a
powerful grammar to easily get humanitarian data in R.

``` r
library(tidyverse)

set_rhdx_config(hdx_site = "prod")

idp_nga_df <-
  search_datasets("displaced Nigeria", rows = 2) %>%
  pluck(1) %>%
  get_resource(1) %>% ## get the first resource
  read_resource(simplify_json = TRUE, folder = tempdir()) ## the file will be downloaded in a temporary directory

idp_nga_df
## $results
##   iso iso3 geo_name year conflict_new_displacements
## 1  NG  NGA  Nigeria 2009                       5000
## 2  NG  NGA  Nigeria 2010                       5000
## 3  NG  NGA  Nigeria 2011                      65000
## 4  NG  NGA  Nigeria 2012                      63000
## 5  NG  NGA  Nigeria 2013                     471000
## 6  NG  NGA  Nigeria 2014                     975000
## 7  NG  NGA  Nigeria 2015                     737000
## 8  NG  NGA  Nigeria 2016                     501000
##   disaster_new_displacements conflict_stock_displacement
## 1                     140000                          NA
## 2                     560000                          NA
## 3                       6300                          NA
## 4                    6112000                          NA
## 5                     117000                     3300000
## 6                       3000                     1075000
## 7                     100000                     2096000
## 8                      78000                     1955000

## $lookups
## named list()

## $errors
## list()

## $success
## [1] TRUE

## $params
## [1] "/api/displacement_data?iso3=NGA&ci=HDX00AKEYJUl17"

## $total
## [1] 8

## $limit
## [1] 0

## $offset
## [1] 0
```

## Meta

  - Please [report any issues or
    bugs](https://gitlab.com/dickoa/rhdx/issues).
  - License: MIT
  - Please note that this project is released with a [Contributor Code
    of Conduct](CONDUCT.md). By participating in this project you agree
    to abide by its terms.
