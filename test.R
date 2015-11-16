library(randomForestSRC)
library(XLConnect)
data(veteran, package = "randomForestSRC")

v.obj <- rfsrc(Surv(time, status) ~ age, data = veteran, 
               ntree = 100, 
               membership = T,
               proximity = T, forest=T)
treeAge <- v.obj$forest$nativeArray[v.obj$forest$nativeArray$treeID == 1, ]
writeWorksheetToFile("RFSC.xlsx", treeAge, sheet="Alter")
# save trees
rf2rfz(v.obj, "Alter")

# all variables
v.obj <- rfsrc(Surv(time, status) ~ ., data = veteran, 
               ntree = 100, 
               membership = T,
               proximity = T, forest=T)
treeAll <- v.obj$forest$nativeArray[v.obj$forest$nativeArray$treeID == 1, ]
writeWorksheetToFile("RFSC.xlsx", treeAll, sheet="Alle Variablen")
# save trees
rf2rfz(v.obj, "All")

