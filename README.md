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

- Proximity and Depth Measure extracted from a fitted random forest ([ranger](https://github.com/imbs-hl/ranger) package)

Besides the functionality of searching similar cases, some additional features are included:

- automatic validation of the key variables between the query and similar cases dataset

- checking proportional hazard assumption for the Cox Model

- C++-functions for distance calculation


## Example: Cox-Beta-Model

### Initialization

In the first example, we use the Cox-Model and the `ovarian` data set from the 
`survival` package. In the first step we initialize the R6 data object. 

```
library(tidyverse)
library(survival)
library(CaseBasedReasoning)
ovarian$resid.ds <- factor(ovarian$resid.ds)
ovarian$rx <- factor(ovarian$rx)
ovarian$ecog.ps <- factor(ovarian$ecog.ps)

# initialize R6 object
coxBeta <- CoxBetaModel$new(Surv(futime, fustat) ~ age + resid.ds + rx + ecog.ps)
```

### Similar Cases 

After the initialization, we may want to get for each case in the query data the most similar case from the learning data. 
```{r}
# fit model 
ovarian %>% 
  coxBeta$fit()
# get similar cases
ovarian %>%
  coxBeta$get_similar_cases(k = 3) -> matchedData
```

**Note 1:** In the initialization step, we dropped all cases with missing values in the variables of ` data` and ` endPoint`. 

**Note 2:** The `data.table` returned from `coxBeta$get_similar_cases` has an additional column `caseId`. By this column you may map the similar cases to cases in data, e.g. if you had chosen ` k = 3`, then the first three elements in the column `caseId` will be ` 1` (following three ` 2` and so on). This means that this three cases are the three most similar cases to case ` 1` in verum data.

### Distance Matrix

Alternatively, you may just be interested in the distance matrix, then you go this way:

```{r}
ovarian %>%
  coxBeta$calc_distance_matrix() -> ditMatrix
```
`coxBeta$calc_distance_matrix()` calculates the full distance matrix. This matrix the dimension: cases of data versus cases of query data. If the query dataset is bot available, this functions calculates a n times n distance matrix of all pairs in data. 
The distance matrix is saved internally in the cbrCoxModel object: ` coxBeta$distMat`.


## References

### Main

- Dippon et al. [A statistical approach to case based reasoning, with application to breast cancer data](http://dl.acm.org/citation.cfm?id=608456) (2002),

- Klenk et al. [Missing Link](http://www.vis.uni-stuttgart.de/~klenksn/paper/medicaldb.pdf) (2009), and

- Friedel et al. [Postoperative Survival of Lung Cancer Patients: Are There Predictors beyond TNM?](http://ar.iiarjournals.org/content/33/4/1609.short) (2012).

### Other

- Stuart, E. et al. [Matching methods for causal inference: Designing observational studies](http://www.biostat.jhsph.edu/~estuart/StuRub_MatchingChapter_07.pdf)

- Defossez et al. [Temporal representation of care trajectories of cancer patients using data from a regional information system: an application in breast cancer](http://www.biomedcentral.com/1472-6947/14/24)
