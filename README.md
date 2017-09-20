<!-- README.md is generated from README.Rmd. Please edit that file -->
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

# Plot
plot_brmap(midwest_cities)
```

I'm preparing a vignette with more examples that will come soon.
