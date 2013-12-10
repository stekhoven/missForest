## Test for reaching specified maximum of iterations    Author: D.Stekhoven
## ========================================================================
## Created: 13.05.2011                                  Package: missForest
library(missForest)
data(esoph)
set.seed(96)
esoph.mis <- prodNA(esoph, noNA = 0.1)
esoph.imp <- missForest(esoph.mis, xtrue = esoph)
man.err <- mixError(esoph.imp$ximp, esoph.mis, esoph)
if (man.err[1] == esoph.imp$error[1] & man.err[2] == esoph.imp$error[2]){
  cat('  no specified maximum number of iterations ... OK\n')
} else {
  stop("manual extracted error is not equal to 'missForest' returned error (but that is not the point here...)!")
}
set.seed(96)
esoph.mis <- prodNA(esoph, noNA = 0.1)
esoph.imp <- missForest(esoph.mis, xtrue = esoph, maxiter = 3)
man.err <- mixError(esoph.imp$ximp, esoph.mis, esoph)
if (man.err[1] == esoph.imp$error[1] & man.err[2] == esoph.imp$error[2]){
  cat('  errors returned in case of specified maximum number of iterations ... OK\n')
} else {
  stop("in case of specified number of iterations the returned error from 'missForest' is not equal to the manually extracted error using 'mixError'")
}
