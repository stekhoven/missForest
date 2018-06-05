##
## MissForest - nonparametric missing value imputation for mixed-type data
##
## This R script contains the function to produce missing values in a given
## data set completely at random.
##
## Author: D.Stekhoven, stekhoven@stat.math.ethz.ch
##############################################################################

prodNA <- function(x, noNA = 0.1){
  n <- nrow(x)
  p <- ncol(x)
  NAloc <- rep(FALSE, n*p)
  NAloc[sample(n * p, floor(n * p * noNA))] <- TRUE
  x[matrix(NAloc, nrow = n, ncol = p)] <- NA
  return(x)
}
