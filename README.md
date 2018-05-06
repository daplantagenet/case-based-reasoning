[![cran version](http://www.r-pkg.org/badges/version/cbr)](https://cran.rstudio.com/web/packages/zoon) 
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/cbr?)](https://cran.r-project.org/web/packages/cbr/)
[![Build Status](https://travis-ci.org/sipemu/case-based-reasoning.svg?branch=master)](https://travis-ci.org/sipemu/case-based-reasoning)

# Case Based Reasoning for Survival Data

The R package case-based-reasoning provides an R interface case based reasoning on survival data.

## Installation

#### CRAN

```
install.packages("cbr")
```

#### GITHUB

```
install.packages("devtools")
devtools::install_github("sipemu/case-based-reasoning")
```

## Features

This R package provides two methods case based reasoning for survival data:

- Cox-Regression

- Random Survival Forest (Proximity and Depth Measure)

Besides the functionality of searching similar cases, some additional features are included:

- automatic validation of the key variables between the query and similar cases dataset

- checking proportional hazard assumption for the Cox Model

- C++-functions for distance calculation


## Example: Cox-Beta-Model



### Reference

[1] http://www.biostat.jhsph.edu/~estuart/StuRub_MatchingChapter_07.pdf

[2] http://www.biomedcentral.com/1472-6947/14/24

## License

GPL-2
