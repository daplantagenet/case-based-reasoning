[![cran version](http://www.r-pkg.org/badges/version/cbr)](https://cran.rstudio.com/web/packages/zoon) 
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/cbr?)](https://cran.r-project.org/web/packages/cbr/)
[![Build Status](https://travis-ci.org/sipemu/case-based-reasoning.svg?branch=master)](https://travis-ci.org/sipemu/case-based-reasoning)

# Case Based Reasoning

The R package case-based-reasoning provides an R interface case based reasoning using machine learning.

## Installation

#### CRAN

```
install.packages("CaseBasedReasoning")
```

#### GITHUB

```
install.packages("devtools")
devtools::install_github("sipemu/case-based-reasoning")
```

## Features

This R package provides two methods case based reasoning by using an endpoint:

- Linear, logistic, and Cox regression

- Proximity and Depth Measure extracted from a fitted random forest

Besides the functionality of searching similar cases, some additional features are included:

- automatic validation of the key variables between the query and similar cases dataset

- checking proportional hazard assumption for the Cox Model

- C++-functions for distance calculation


## Example: Cox-Beta-Model



### Reference

#### Main

- [Dippon et al. A statistical approach to case based reasoning, with application to breast cancer data](http://dl.acm.org/citation.cfm?id=608456) (2002),

- [Klenk et al.](http://www.vis.uni-stuttgart.de/~klenksn/paper/medicaldb.pdf) (2009), and

- [Friedel et al. Postoperative Survival of Lung Cancer Patients: Are There Predictors beyond TNM?](http://ar.iiarjournals.org/content/33/4/1609.short) (2012).

#### Other

- [Stuart, e. et al. Matching methods for causal inference: Designing observational studies](http://www.biostat.jhsph.edu/~estuart/StuRub_MatchingChapter_07.pdf)

- [Defossez et al. Temporal representation of care trajectories of cancer patients using data from a regional information system: an application in breast cancer](http://www.biomedcentral.com/1472-6947/14/24)
