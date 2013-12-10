## Test for using the classwt argument                  Author: D.Stekhoven
## ========================================================================
## Created: 22.06.2012                                  Package: missForest
library(missForest)
dat <- data(iris)
set.seed(13)
xmis <- prodNA(iris)
ximp <- missForest(xmis, ntree = 100, classwt = list(NULL, NULL, NULL, NULL, c(10, 30, 10)))
summary(ximp$ximp); ximp$OOBerror
