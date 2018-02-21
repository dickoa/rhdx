
# rhdx

<!-- README.md is generated from README.Rmd. Please edit that file -->

[![GitLab CI Build
Status](https://gitlab.com/dickoa/rhdx/badges/master/build.svg)](https://gitlab.com/dickoa/rhdx/pipelines)
[![Codecov Code
Coverage](https://codecov.io/gl/dickoa/rhdx/branch/master/graph/badge.svg)](https://codecov.io/gl/dickoa/rhdx)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

`rhdx` is an R client for the Humanitarian Exchange Data
platform.

![](https://gitlab.com/dickoa/rhdx/raw/5ec77127ad1a7322c6ff118f4b0a8fdcbba71788/inst/demo/demo.gif)

## Installation

This package is not on yet on CRAN and to install it, you will need the
[`devtools`](https://github.com/r-lib/devtools) package and the
development version of the [`crul`](https://github.com/ropensci/crul)
package.

``` r
## install.packages("devtools") 
devtools::install_github("ropensci/crul") ## need the dev versio > 0.5.0
devtools::install_git("https://gitlab.com/dickoa/rhdx")
```

## rhdx tutorial

``` r
library("rhdx")
```

``` r
Configuration$create(hdx_site = "test")
Configuration$read()
## <HDX Configuration> 
##   HDX site: test
##   HDX site url: https://test-data.humdata.org/
##   HDX API key: 
```

``` r
dataset <- Dataset$read_from_hdx("acled-conflict-data-for-africa-realtime-2016")
dataset
## <HDX Dataset> 6f36a41c-f126-4b18-aaaf-6c2ddfbc5d4d 
##   Title: ACLED Conflict Data for Africa (Realtime - 2016)
##   Name: acled-conflict-data-for-africa-realtime-2016
##   Date: 12/03/2016
##   Tags (up to 5): conflict, political violence, protests, war
##   Locations (up to 5): dza, ago, ben, bwa, bfa
##   Resources (up to 5): ACLED-All-Africa-File_20160101-to-date.xlsx, ACLED-All-Africa-File_20160101-to-date_csv.zip

dataset$get_dataset_date()
## [1] "12/03/2016"
```

``` r
datasets <- Dataset$search_in_hdx("ACLED", rows = 1)
datasets
## [[1]]
## <HDX Dataset> ac9f19f0-132e-46e6-9a9c-7d29dca8a469 
##   Title: ACLED Conflict Data for Algeria
##   Name: acled-conflict-data-for-algeria
##   Date: 01/01/1997-12/31/2015
##   Tags (up to 5): conflict, political violence, protests, war
##   Locations (up to 5): dza
##   Resources (up to 5): Algeria.xlsx
```

``` r
resources <- datasets[[1]]$get_resources()
resources
## [[1]]
## <HDX Resource> 87ce2238-9049-4fb7-aa53-3c9d44b6183b 
##   Name: Algeria.xlsx
##   Description: 
##   Size: 
##   Format: XLSX
```

``` r
resources[[1]]$download()
## trying URL 'http://www.acleddata.com/wp-content/uploads/2016/01/Eritrea.xlsx'
## Content type 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' length 69983 bytes (68 KB)
## ==================================================
## downloaded 68 KB
```

We can also read the resources directly in R

``` r
dplyr::glimpse(resources[[1]]$read_session())
## reading sheet:  Sheet1 
## Observations: 3,698
## Variables: 25
## $ GWNO             <dbl> 615, 615, 615, 615, 615, 615, 615, ...
## $ EVENT_ID_CNTY    <chr> "1ALG", "2ALG", "3ALG", "4ALG", "5A...
## $ EVENT_ID_NO_CNTY <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, ...
## $ EVENT_DATE       <dttm> 1997-01-04, 1997-01-05, 1997-01-06...
## $ YEAR             <dbl> 1997, 1997, 1997, 1997, 1997, 1997,...
## $ TIME_PRECISION   <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
## $ EVENT_TYPE       <chr> "Violence against civilians", "Viol...
## $ ACTOR1           <chr> "GIA: Armed Islamic Group", "GIA: A...
## $ ALLY_ACTOR_1     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ INTER1           <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,...
## $ ACTOR2           <chr> "Civilians (Algeria)", "Civilians (...
## $ ALLY_ACTOR_2     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ INTER2           <dbl> 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,...
## $ INTERACTION      <dbl> 27, 27, 27, 27, 27, 27, 27, 27, 27,...
## $ COUNTRY          <chr> "Algeria", "Algeria", "Algeria", "A...
## $ ADMIN1           <chr> "Blida", "Tipaza", "Tipaza", "Alger...
## $ ADMIN2           <chr> "Blida", "Douaouda", "Hadjout", "Bo...
## $ ADMIN3           <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ LOCATION         <chr> "Blida", "Douaouda", "Hadjout", "Al...
## $ LATITUDE         <dbl> 36.46860, 36.67250, 36.51390, 36.75...
## $ LONGITUDE        <dbl> 2.828900, 2.789400, 2.417800, 3.041...
## $ GEO_PRECISION    <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
## $ SOURCE           <chr> "www.algeria-watch.org", "www.alger...
## $ NOTES            <chr> "4 January: 16 citizens were murder...
## $ FATALITIES       <dbl> 16, 18, 23, 20, 5, 14, 43, 54, 30, ...
```

so far the following format are supported: `csv`, `xlsx`, `xls`, `zipped
shapefile`, `zipped kml` `kmz` `zipped geodatabase` and `zipped
geopackage`

## Get data from HDX

### Connect to a server

You need to use the `Configuration` class to select a server

``` r
conf <- Configuration$setup(hdx_site = "demo")
conf
## <HDX Configuration> 
##   HDX site: demo
##   HDX site url: https://demo-data.humdata.org/
##   HDX API key: 
```

Switch to the production server

``` r
conf <- Configuration$setup(hdx_site = "prod")
conf
## <HDX Configuration> 
##   HDX site: prod
##   HDX site url: https://data.humdata.org/
##   HDX API key: 
```

### Search datasets

Once we have a server, we can search from dataset using the `Dataset`
class and `search_in_hdx` method.

``` r
list_of_ds <- Dataset$search_in_hdx("displaced Nigeria", rows = 2)
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

``` r
ds <- list_of_ds[[1]]
ds
## <HDX Dataset> 4fbc627d-ff64-4bf6-8a49-59904eae15bb 
##   Title: Nigeria - Internally displaced persons - IDPs
##   Name: idmc-idp-data-for-nigeria
##   Date: 01/01/2009-12/31/2016
##   Tags (up to 5): displacement, idmc, population
##   Locations (up to 5): nga
##   Resources (up to 5): displacement_data, conflict_data, disaster_data
```

### Download or read resources from datasets

``` r
list_of_rs <- ds$get_resources()
list_of_rs
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

We are looking for the displacement data, it’s the first resource in our
list `list_of_rs`

``` r
idp_nga_rs <- list_of_rs[[1]]
idp_nga_df <- idp_nga_rs$read_session(json_simplifyVector = TRUE, folder = tempdir())
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

## rhdx package API

  - Configuration - `create()` - `setup` - `read`
  - Dataset - `read_from_hdx` - `search_in_hdx`
  - Resource - `download` - `read_from_hdx` - `search_in_hdx`

## Future dev

  - Develop all `create_in_hdx` methods
  - Add tidy tools to easily get data
  - Shiny apps amd Rstudio add-in to browse and select data
    interactively

## Meta

  - Please [report any issues or
    bugs](https://gitlab.dickoa/rhdx/issues).
  - License: MIT
  - Please note that this project is released with a [Contributor Code
    of Conduct](CONDUCT.md). By participating in this project you agree
    to abide by its terms.
