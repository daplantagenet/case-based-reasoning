# Case Based Reasoning for Survival Data

### Installation

```{r}
install.packages(devtools)
devtools::install_github("sipemu/case-based-reasoning")
```

Please follwo the instruction of the following link for multicore support for RandomForest package:

http://www.ccs.miami.edu/~hishwaran/rfsrc.html

### Example: Distance based on the coefficients of the Cox Regression 

#### Get distance matrix 
```{r}
cbr.cox <- cbrCoxModel$new(refData=ovarian, endPoint=c("futime", "fustat"))
cbr.cox$getFullDistanceMatrix()
heatmap(cbr.cox$distMat)
```

### Example: Distance based on the coefficients of the Random Forest for Survival Data

#### Get distance matrix 
```{r}
cbr.RF <- cbrRFProxy$new(refData=ovarian, newData=ovarian, endPoint=c("futime", "fustat"), impute =TRUE)
cbr.RF$getFullDistanceMatrix()
cbr.RF$
heatmap(cbr.RF$distMat)
```

### Literatur

[1] http://www.biostat.jhsph.edu/~estuart/StuRub_MatchingChapter_07.pdf

[2] http://www.biomedcentral.com/1472-6947/14/24

## License

Case-based-reasoning is licensed under the <a href="http://www.tldrlegal.com/l/AGPL3" target="\_blank">AGPLv3</a>. The documentation and videos on this site are licensed under the creative commons attribution, non-commercial, share-alike license <a href="http://creativecommons.org/licenses/by-nc-sa/4.0/" target="_blank">CC-NC-SA</a>.

As a summary, the AGPLv3 license requires, attribution, including copyright and license information in copies of the software, stating changes if the code is modified, and disclosure of all source code. Details are in the COPYING file.

&copy; Simon Müller (2015) <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/" target="_blank"></a>
