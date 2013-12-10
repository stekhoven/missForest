## Test for argument compliance                         Author: D.Stekhoven
## ========================================================================
## Created: 13.05.2011                                  Package: missForest
library(missForest)
data(esoph)
set.seed(107)
## vanilla
esoph.mis <- prodNA(esoph, noNA = 0.1)
esoph.imp <- missForest(esoph.mis)
esoph.imp$OOBerror

## maxiter
esoph.imp <- missForest(esoph.mis, maxiter = 1)
esoph.imp$OOBerror

## decreasing
esoph.imp <- missForest(esoph.mis, decreasing = TRUE)
esoph.imp$OOBerror

## verbose not included due to inconsistent runtimes

## mtry
esoph.imp <- missForest(esoph.mis, mtry = 55)
esoph.imp$OOBerror

## ntree
esoph.imp <- missForest(esoph.mis, ntree = 10)
esoph.imp$OOBerror

## xtrue
esoph.imp <- missForest(esoph.mis, xtrue = esoph)
esoph.imp$OOBerror
esoph.imp$error
