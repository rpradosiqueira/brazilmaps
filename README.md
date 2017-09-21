<!-- README.md is generated from README.Rmd. Please edit that file -->
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/brazilmaps)](https://CRAN.R-project.org/package=brazilmaps) [![CRAC\_Downloads](https://cranlogs.r-pkg.org/badges/grand-total/brazilmaps)](https://CRAN.R-project.org/package=brazilmaps)

brazilmaps
==========

The goal of brazilmaps is to provide Brazilian map spatial objects of varying region types (e.g. cities, states, microregions, mesoregions).

Installation
------------

Install the release version from CRAN:

``` r
install.packages("brazilmaps")
```

or the development version from github

``` r
# install.packages("devtools")
devtools::install_github("rpradosiqueira/brazilmaps")
```

Example
-------

Let's assume that we want to plot the brazilian municipalities of the Midwest Region. To do this simply execute:

``` r
library(brazilmaps)

# Get de map
midwest_cities <- get_brmap(geo = "City",
                            geo.filter = list(Region = 5))
#> Warning: package 'bindrcpp' was built under R version 3.3.3

# Plot
plot_brmap(midwest_cities)
#> Regions defined for each Polygons
```

![](README-midwest-cities-1.png)

I'm preparing a vignette with more examples that will come soon.
