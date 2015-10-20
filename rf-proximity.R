# RandomForest Proximity

## Initialization 

In the first example, we use the Cox-Model and the `ovarian` data set from the 
`survival` package. In the first step we initialize the R6 data object. The initialization needs several inputs:
  
  **Arguments:**
  
  ------------------------ | ----
  *learning*       | data set for learning the Cox model
*verumData* | Verum data set. For each case in the verum data, we are looking for the m (=1,...,k) similar cases. Learning and verum data set need the same structure (variable names and scales)
*learnVars (optional)* | A character vector variable names. This variables are used for learning the model. Do not include time2event and event variable here. Default: all variables, except endPoint variables, are used for model fitting.
*endPoint (optional)* | A character vector of length two. The first elements contains the variable name of the time 2 event variable and the second the name of the event variable. Default: ` c("Time2Event", "Event")`
*impute (optional)* | TRUE/FALSE: Missing value imputation. Actually, not implemented for the Cox Model.



<br>
  ```{r, warning=FALSE, message=FALSE}
library(survival)
library(cbr)
ovarian$resid.ds <- factor(ovarian$resid.ds)
ovarian$rx <- factor(ovarian$rx)
ovarian$ecog.ps <- factor(ovarian$ecog.ps)

# initialize R6 object
sc <- cbrCoxModel$new(learning=ovarian, verumData=ovarian, learnVars=c("age", "resid.ds", "rx", "ecog.ps"), endPoint=c("futime", "fustat"), impute=F)
```
If `impute` is set to `FALSE` (for the Cox model the `ìmpute` variable is always set to `FALSE`) then all cases with missing values in the learning and end point variables are dropped (similar to `na.omit`) and the reduced data set without missing values is saved internally. You get an information on how many cases were dropped. 


## Similar Cases 

After the initialization, we may get for each case in verum data the most similar case from the learning data. 
```{r}
sc$getSimilarCases(nCases = 1)
```
You may extract then the similar cases and the verum data and put them together:
  ```{r, eval=FALSE}
verum <- sc$getVerumData()
verum$group <- "Verum"
simCases <- sc$simCases
simCases$caseId <- NULL
simCases$group <- "Similar Cases"
df <- rbind(verum, simCases)
```
**Note 1:** In the initialization step, we dropped all cases with missing values in the variables of ` learnVars` and ` endPoint`. The function `sc$getVerumData()` returns the data set without the missing values if ` impute` is set to ` FALSE` else you get full verum data with imputed values (just for RSF model).

**Note 2:** The ` data.frame` returned from ` sc$simCases` has an additional column `caseId`. By this column you may map the similar cases to cases in verum data, e.g. if you had chosen ` nCases = 3`, then the first three elements in the column `caseId` will be ` 1` (following three ` 2` and so on). This means that this three cases are the three most similar cases to case ` 1` in verum data.

### Validate

You may want to check the distibution of the ` learnVars` variables of the verum and similar cases data set:
  ```{r}
pp <- sc$validate()
pp
```
The function ` sc$validate()` returns an ` gg` and ` ggplot` object. 


## Distance Matrix

Alternatively, you may just be interested in the distance matrix, then you go this way:
  
  ```{r}
sc <- cbrCoxModel$new(learning=ovarian, verumData=ovarian, learnVars=names(ovarian)[-c(1, 2)], endPoint=c("futime", "fustat"), impute=F)

sc$getFullDistanceMatrix()
```
` sc$getFullDistanceMatrix()` calculates the full distance matrix. This matrix the dimension: cases of learning data versus cases of verum data. In the above example we used the same data set for learning and as verum data. Such case can also be achieved by:
  ```{r, eval=FALSE}
sc <- cbrCoxModel$new(learning=ovarian, learnVars=names(ovarian)[-c(1, 2)], endPoint=c("futime", "fustat"), impute=F)

sc$getFullDistanceMatrix()
```
The distance matrix is saved internally in the sc object: ` sc$distMat`.

## Shiny App for manifold learning

You may now want to analyse your data with t-SNE:
  ```{r, eval=FALSE}
cbrDistViz(sc)
```

This opens a shiny app and you may then search for clusters in the by t-SNE reduced data set. 

**Note**: On big data sets, the t-SNE may need some time. Do not close the shiny window.
