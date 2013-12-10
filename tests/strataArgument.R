## Test for using the strata argument                   Author: D.Stekhoven
## ========================================================================
## Created: 22.06.2012                                  Package: missForest
library(missForest)
dat <- data(iris)
set.seed(13)
xmis <- prodNA(iris)
ximp <- missForest(xmis, ntree = 100, strata = list(NULL, NULL, NULL, NULL, iris$Species))
summary(ximp$ximp); ximp$OOBerror
