## Test for using the replace argument                  Author: D.Stekhoven
## ========================================================================
## Created: 22.06.2012                                  Package: missForest
library(missForest)

## load some data
dat <- data(iris)
set.seed(13)
xmis <- prodNA(iris)
ximp <- missForest(xmis, ntree = 100)
summary(ximp$ximp); ximp$OOBerror

## subsampling instead of bootstrap
set.seed(13)
xmis <- prodNA(iris)
ximp <- missForest(xmis, ntree = 100, replace = FALSE)
summary(ximp$ximp); ximp$OOBerror
