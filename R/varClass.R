##
## MissForest - nonparametric missing value imputation for mixed-type data
##
## This R script contains the function for defining variablewise data types.
##
## Author: D.Stekhoven, stekhoven@stat.math.ethz.ch
##############################################################################

varClass <- function(x){
  xAttrib <- lapply(x, attributes)
  p <- ncol(x)
  x.types <- character(p)
  for (t.co in 1 : p){
    if (is.null(xAttrib[[t.co]])){
      x.types[t.co] <- 'numeric'
    } else {
      x.types[t.co] <- 'factor'
    }
  }
  return(x.types)
}
