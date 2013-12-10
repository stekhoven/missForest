## Test for imputation of continuous data               Author: D.Stekhoven
## ========================================================================
## Created: 13.05.2011                                  Package: missForest
library(missForest)
data(trees)
set.seed(107)
trees.mis <- prodNA(trees, noNA = 0.1)
trees.imp <- missForest(trees.mis, xtrue = trees)
man.err <- mixError(trees.imp$ximp, trees.mis, trees)
if (man.err == trees.imp$error){
  cat('  test whether errors are the same ... OK\n')
} else {
  stop("error extracted by 'mixError' does not equal the returned error of 'missForest'!")
}
