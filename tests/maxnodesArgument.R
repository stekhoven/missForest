## Test for using the maxnodes argument                 Author: D.Stekhoven
## ========================================================================
## Created: 22.06.2012                                  Package: missForest
library(missForest)
dat <- data(iris)
set.seed(13)
xmis <- prodNA(iris)
ximp <- missForest(xmis, ntree = 100, maxnodes = 4)
summary(ximp$ximp); ximp$OOBerror
