##
## MissForest - nonparametric missing value imputation for mixed-type data
##
## This R script contains the the function to compute the normalized root mean
## squared error.
##
## Author: D.Stekhoven, stekhoven@stat.math.ethz.ch
##############################################################################

nrmse <- function(ximp, xmis, xtrue){
  mis <- is.na(xmis)
  sqrt(mean((ximp[mis] - xtrue[mis])^{2}) / stats::var(xtrue[mis]))
}
