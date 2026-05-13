
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Project Status: Concept – Minimal or no implementation has been done
yet, or the repository is only intended to be a limited example, demo,
or
proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License](https://img.shields.io/badge/license-GPL--3-blue.svg?style=flat)](https://www.gnu.org/licenses/gpl-3.0.html)
[![Release](https://img.shields.io/github/release/inbo/waterbirds1.1.svg)](https://github.com/inbo/waterbirds1.1/releases)
![GitHub](https://img.shields.io/github/license/inbo/waterbirds1.1) [![R
build
status](https://github.com/inbo/waterbirds1.1/workflows/check%20package%20on%20main/badge.svg)](https://github.com/inbo/waterbirds1.1/actions)
![r-universe
name](https://inbo.r-universe.dev/badges/:name?color=c04384)
![r-universe package](https://inbo.r-universe.dev/badges/waterbirds1.1)
[![Codecov test
coverage](https://codecov.io/gh/inbo/waterbirds1.1/branch/main/graph/badge.svg)](https://app.codecov.io/gh/inbo/waterbirds1.1?branch=main)
![GitHub code size in
bytes](https://img.shields.io/github/languages/code-size/inbo/waterbirds1.1.svg)
![GitHub repo
size](https://img.shields.io/github/repo-size/inbo/waterbirds1.1.svg)
<!-- badges: end -->

# waterbirds1.1

The goal of waterbirds1.1 is to calculate the nutrient input by
waterbirds, based on the tool Waterbirds v1.1 (2007) from S. Bauwer & S.
Hahn, Netherlands Institute of Ecology (NIOO-KNAW) and the related
publications of *Hahn et al. (2007 and 2008)*.

References:

Hahn S., Bauwer S. & Klaassen M. (2007). Estimating the contribution of
carnivorous waterbirds to nutrient loading in freshwater habitats.
Freshwater Biology 52: 2421-2433.
[doi:10.1111/j.1365-2427.2007.01838.x](https://doi.org/10.1111/j.1365-2427.2007.01838.x)

Hahn S., Bauwer S. & Klaassen M. (2008). Quantification of allochthonous
nutrient input into freshwater bodies by herbivorous waterbirds.
Freshwater Biology 53: 181-193.
[doi:10.1111/j.1365-2427.2007.01881.x](https://doi.org/10.1111/j.1365-2427.2007.01881.x)

## Installation

You can install the package from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("inbo/waterbirds1.1")
```

## Calculate nutrient input with `waterbirds1.1`

*Hahn et al. (2007 and 2008)* presented 2 different models for
calculating the nutrient input of nitrogen and phosphorus by waterbirds:

- an intake model that calculates the nutrient loadings based on daily
  food intake by waterbirds

- a dropping (*Hahn et al., 2008*) or excretion model (*Hahn et al.,
  2007*) based on the daily faecal output

R-package `waterbirds1.1` has generic functions `give_intake_model()`
and `give_dropping_model()` for calculating these models that require a
dataset with abundance data (and some predefined variables that default
to the values suggested in *Hahn et al. (2007 and 2008)* but that can be
customised). These functions use predefined info to distinguish between
carnivores and herbivores, and food type defaults to grass in case of
the intake model of herbivores.

A minimal code example on how to use these functions:

``` r
library(waterbirds1.1)

# make (or load) a dataset with abundance data:
dataset <- data.frame(
  species = c("Anas crecca", "Ardea cinerea"),
  month = 3,
  year = 2026,
  n_individuals = 1,
  location = "ZwartWater"
)

give_intake_model(bird_abundance = dataset)
#> # A tibble: 2 × 9
#>   location   species  year n_tot_25 n_tot_50 n_tot_75 p_tot_25 p_tot_50 p_tot_75
#>   <chr>      <chr>   <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#> 1 ZwartWater Anas c…  2026  0.00218  0.00265  0.00312 0.000180 0.000216 0.000248
#> 2 ZwartWater Ardea …  2026  0.253    0.262    0.267   0.0586   0.0725   0.0888

give_dropping_model(bird_abundance = dataset)
#> # A tibble: 2 × 5
#>   location   species        year   n_tot    p_tot
#>   <chr>      <chr>         <dbl>   <dbl>    <dbl>
#> 1 ZwartWater Anas crecca    2026 0.00178 0.000245
#> 2 ZwartWater Ardea cinerea  2026 0.102   0.0468
```

These generic functions use some more detailed functions in the
background that are specific for carnivores or herbivores. For
carnivores, there are additional functions for breeding birds,
corresponding to the calculation described in *Hahn et al. (2007)*.
These functions expect the variables in a different format, but they can
be used as well.

All documentation on the use of the functions, including an example, can
be accessed via `Reference` in the navigation bar.

As there are some caveats in the use of abundance data from
[observations.be](https://observations.be/), we elaborate on this in
`vignette("observations", package = "waterbirds1.1")`.
