#' R6 Validation Class for case based reasoning
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#' @keywords validation
cbrValidate <- R6Class("cbrValidate",
                       public=list(
                         validate=function(verumData, simCases, learnVars, plots=FALSE) {
                           # get data.frame
                           idVars <- which(names(simCases) %in% learnVars)
                           sc <- simCases %>%
                             select(idVars)
                           sc$simCases <- "Similar\nCases"
                           idVars <- which(names(verumData) %in% learnVars)
                           nc <- verumData %>%
                             select(idVars)
                           nc$simCases <- "Reference\nCases"
                           sc <- rbind(sc, nc)
                           stat.color <- c("#0A71B4", "#61636B", "#13235B", "#E36929", "#C9D30F", "66B8DC")
                           
                           ggPlot <- list()
                           tit <- c()
                           for (var in learnVars) {
                             formel <- as.formula(paste(var, "~ simCases"))
                             if (is.numeric(simCases[, var])) {
                               w <- wilcox.test(formel, data=sc, exact = F)
                               tit <- c(tit, paste0(var, " (p = ", round(w$p.value, 3), ")"))
                               cat(paste0("Two-sample Wilcoxon test for variable: ", var, "; p = ", round(w$p.value, 3), "\n"))
                               if (plots)
                                 ggPlot <-  c(ggPlot, list(ggplot(sc) +
                                                             geom_boxplot(aes_string(x="simCases", y=var), alpha=0) +
                                                             xlab("") + ylab("") + 
                                                             background_grid(major="y", minor="y")))
                             } else if (is.factor(simCases[, var])) {
                               w <- chisq.test(sc$simCases, sc[, var])
                               tit <- c(tit, paste0(var, " (p = ", round(w$p.value, 3), ")"))
                               cat(paste0("Chi-Square test for variable: ", var, "; p = ", round(w$p.value, 3), "\n"))
                               df <- data.frame(prop.table(table(sc$simCases, sc[, var]), 1))
                               if (plots)
                                 ggPlot <- c(ggPlot, list(ggplot(df) +
                                                            geom_bar(aes_string(x="Var2", y="Freq", fill="Var1"), stat = "identity", position="dodge", alpha=.7) +
                                                            scale_fill_manual(name="", values = stat.color) +
                                                            xlab("") + ylab("") + theme(legend.position="top") + 
                                                            background_grid(major="y", minor="y")))
                             }
                           }
                           if (plots) {
                             theme_set(theme_cowplot())
                             return(plot_grid(plotlist = ggPlot, labels=tit, ncol=2))
                           }
                           return()
                         }
                       ))
