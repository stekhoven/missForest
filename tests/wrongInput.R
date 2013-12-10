## Test for wrong input class (true negative case)      Author: D.Stekhoven
## ========================================================================
## Created: 13.05.2011                                  Package: missForest
library(missForest)
data(esoph)
set.seed(107)
esoph.mis <- prodNA(esoph, noNA = 0.1)
esoph.imp <- missForest(esoph.mis, xtrue = esoph)
Ximp <- esoph.imp$ximp
man.err <- mixError(Ximp, esoph.mis, esoph)
if (man.err[1] == esoph.imp$error[1] & man.err[2] == esoph.imp$error[2]){
  cat('  test whether wrong class but correct input are handled well ... OK\n')
} else {
  stop("extracting the imputed matrix 'manually' from the 'missForest' output failed in using 'mixError'!")
}
