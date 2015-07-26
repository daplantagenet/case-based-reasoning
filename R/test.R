# library(survival)
# library(cbr)
# 
# # Cox model
# ovarian$tt <- factor(sample(1:3, 26, replace=T))
# ovarian$resid.ds <- factor(ovarian$resid.ds)
# ovarian$rx <- factor(ovarian$rx)
# ovarian$ecog.ps <- factor(ovarian$ecog.ps)
# 
# sc <- cbrCoxModel$new(learning=ovarian, verumData=ovarian, learnVars=names(ovarian)[-c(1, 2)], endPoint=c("futime", "fustat"))
# sc$learn()
# sc$getSimilarCases(1)
# sc$simCases
# pp <- sc$validate()
# pp <- sc$check_linearity()
# 
# sc$getFullDistanceMatrix()
# p <- cmdscale(sc$distMat, k = 2)
# plot(p, col=as.factor(ovarian$fustat))
# p <- prcomp(sc$distMat)
# plot(p, col=as.factor(ovarian$rx))
# library(Rtsne)
# p <- Rtsne(sc$distMat, is_distance = T, perplexity = 5, theta = .1)
# plot(p$Y, col=as.factor(ovarian$rx))
# 
# 
# # RF model
# ovarian$tt <- NULL
# ovarian$rx <- factor(ovarian$rx)
# sc <- cbrRFProxy$new(learning=ovarian, verumData=ovarian, learnVars=names(ovarian)[-c(1, 2)], endPoint=c("futime", "fustat"), impute=TRUE)
# sc$learn()
# sc$getSimilarCases(1)
# pp <- sc$validate()
# sc$getVerumData()
# sc$getLearningData()
# sc$getFullDistanceMatrix()
# res <- Rtsne(X = sc$distMat, dims=2, perplexity = 5, is_distance = T)
# plot(res$Y, col=as.factor(ovarian$ecog.ps))
# 
# ovarian$futime[3] <- NA
# ovarian$age[5] <- NA
# rf <- rfsrc(Surv(futime, fustat) ~ resid.ds + age + rx, data=ovarian, na.action = "na.impute")
