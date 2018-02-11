rhdx
====

[![GitLab CI Build
Status](https://gitlab.com/dickoa/rhdx/badges/master/build.svg)](https://gitlab.com/dickoa/rhdx/pipelines)
[![AppVeyror Build
status](https://ci.appveyor.com/api/projects/status/qytbcx7vjq0t9ao5/branch/master?svg=true)](https://ci.appveyor.com/project/dickoa/rhdx)
[![Codecov Code
Coverage](https://codecov.io/gl/dickoa/rhdx/branch/master/graph/badge.svg)](https://codecov.io/gl/dickoa/rhdx)
[![](http://www.r-pkg.org/badges/version/rhdx)](http://www.r-pkg.org/pkg/rhdx)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/rhdx)](http://www.r-pkg.org/pkg/rhdx)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

`rhdx` is an R client for the Humanitarian Exchange Data platform.

Installation
------------

Development version

    install.packages("devtools")
    devtools::install_git("https://gitlab.com/dickoa/rhdx")

    library('rhdx')

Note: the default base CKAN URL is set to
<http://data.techno-science.ca/>. Functions requiring write permissions
in CKAN additionally require a privileged CKAN API key. You can change
this using `rhdx_setup()`, or change the URL using the `url` parameter
in each function call. To set one or both, run:

    Configuration$create(prod = "demo") # restores default CKAN url to http://data.techno-science.ca/

    rhdx_setup(prod = "demo") # restores default CKAN url to http://data.techno-science.ca/

rhdx package API
----------------

There are a suite of CKAN things (package, resource, etc.) that each
have a set of functions in this package. The functions for each CKAN
thing have an S3 class that is returned from most functions, and can be
passed to most other functions (this also facilitates piping). The
following is a list of the function groups for certain CKAN things, with
the prefix for the functions that work with that thing, and the name of
the S3 class:

-   Configuration - `create()` - `setup` - `read`
-   Dataset - `read_from_hdx` - `search_in_hdx`
-   Resource - `download` - `get_dataset` - `search_in_hdx`

The R6 class objects all look very similar; for example:

    <HDX Resource> 8abc92ad-7379-4fb8-bba0-549f38a26ddb
      Name: Data From Digital Portal
      Description:
      Size:
      Format: CSV

All classes state the type of object, have the ID to the right of the
type, then have a varying set of key-value fields deemed important. This
printed object is just a summary of an R list, so you can index to
specific values (e.g., `result$description`). If you feel there are
important fields left out of these printed summaries, let us know.

> note: Many examples are given in brief for readme brevity

Packages
--------

List packages

    Dataset$search_in_hdx(query = "Somalia IDP", rows = 5L)

### The National Geothermal Data System

Website: <http://geothermaldata.org/>

    rhdx_setup(hdx_site = "prod")
    x <- Dataset$search_in_hdx(q = '*:*', rows = 1)
    x$results

Future dev
----------

### Rstudio Add-in to browse data interactively

Meta
----

-   Please [report any issues or
    bugs](https://gitlab.dickoa/rhdx/issues).
-   License: MIT
-   Please note that this project is released with a [Contributor Code
    of Conduct](CONDUCT.md). By participating in this project you agree
    to abide by its terms.
