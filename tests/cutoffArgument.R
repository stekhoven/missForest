## Test for using the cutoff argument                   Author: D.Stekhoven
## ========================================================================
## Created: 22.06.2012                                  Package: missForest
library(missForest)
dat <- data(iris)
set.seed(13)
xmis <- prodNA(iris)
ximp <- missForest(xmis, ntree = 100, cutoff = list(1, 1, 1, 1, c(0.3, 0.6, 0.1)))
summary(ximp$ximp); ximp$OOBerror

