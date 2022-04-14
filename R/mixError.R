##
## MissForest - nonparametric missing value imputation for mixed-type data
##
## This R script contains the error function for mixed-type data.
##
## Author: D.Stekhoven, stekhoven@stat.math.ethz.ch
##############################################################################

mixError <- function(ximp, xmis, xtrue)
{
  ## Purpose:
  ## Calculates the difference between to matrices. For all numeric
  ## variables the NRMSE is used and for all categorical variables
  ## the relative number of false entries is returned.
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ximp      = (imputed) matrix
  ## xmis      = matrix with missing values
  ## xtrue     = true matrix (or any matrix to be compared with ximp)
  ## ----------------------------------------------------------------------
  ## Author: Daniel Stekhoven, Date: 26 Jul 2010, 10:10

  if (inherits(ximp, 'missForest'))
    stop("'xmis' is not of class 'missForest' - maybe you forgot to point at the\n  list element $ximp from the missForest output object.")
  x.types <- varClass(ximp)
  n <- nrow(ximp)
  k <- length(unique(x.types))
  err <- rep(Inf, k)
  t.co <- 1
  if (k == 1){
    if (unique(x.types) == 'numeric'){
      names(err) <- c('NRMSE')
    } else {
      names(err) <- c('PFC')
      t.co <- 1
    }
  } else {
    names(err) <- c('NRMSE', 'PFC')
    t.co <- 2
  }
  ## for (t.type in names(err)){
  for (t.type in x.types){
    t.ind <- which(x.types == t.type)
    if (t.type == "numeric"){
      err[1] <- nrmse(ximp[,t.ind], xmis[,t.ind], xtrue[,t.ind])
    } else {
      dist <- sum(as.character(as.matrix(ximp[,t.ind])) != as.character(as.matrix(xtrue[,t.ind])))
      no.na <- sum(is.na(xmis[,x.types == 'factor']))
      if (no.na == 0){
        err[t.co] <- 0
      } else {
        err[t.co] <- dist / no.na
      }
    }
  }
  return(err)
}
