## Test for imputation of mixed-type data               Author: D.Stekhoven
## ========================================================================
## Created: 13.05.2011                                  Package: missForest
library(missForest)
data(esoph)
set.seed(107)
esoph.mis <- prodNA(esoph, noNA = 0.1)
esoph.imp <- missForest(esoph.mis, xtrue = esoph)
man.err <- mixError(esoph.imp$ximp, esoph.mis, esoph)
if (man.err[1] == esoph.imp$error[1] & man.err[2] == esoph.imp$error[2]){
  cat('  test whether errors are the same ... OK\n')
} else {
  stop("error extracted by 'mixError' does not equal the returned error of 'missForest'!")
}
