\name{missForest}
\alias{missForest}
\title{
  Nonparametric Missing Value Imputation using Random Forest
}
\description{
  'missForest' is used to impute missing values particularly in the case
  of mixed-type data. It can be used to impute continuous and/or
  categorical data including complex interactions and nonlinear
  relations. It yields an out-of-bag (OOB) imputation error estimate.
}
\usage{
missForest(xmis, maxiter = 10, ntree = 100, variablewise = FALSE,
                       decreasing = FALSE, verbose = FALSE,
                       mtry = floor(sqrt(ncol(xmis))), replace = TRUE,
                       classwt = NULL, cutoff = NULL, strata = NULL,
                       sampsize = NULL, nodesize = NULL, maxnodes = NULL,
                       xtrue = NA)
}
\arguments{
  \item{xmis}{
    a data matrix with missing values. The columns correspond to the
    variables and the rows to the observations.
  }
  \item{maxiter}{
    maximum number of iterations to be performed given the stopping criterion
    is not met beforehand.
  }
  \item{ntree}{
    number of trees to grow in each forest.
  }
  \item{variablewise}{
    logical. If 'TRUE' the OOB error is returned for each variable
    separately. This can be useful as a reliability check for the
    imputed variables w.r.t. to a subsequent data analysis.
  }
  \item{decreasing}{
    logical. If 'FALSE' then the variables are sorted w.r.t. increasing
    amount of missing entries during computation. 
  }
  \item{verbose}{
    logical. If 'TRUE' the user is supplied with additional output between
    iterations, i.e., estimated imputation error, runtime and if complete
    data matrix is supplied the true imputation error. See 'xtrue'.
  }
  \item{mtry}{
    number of variables randomly sampled at each split. This argument is
    directly supplied to the 'randomForest' function. Note that the
    default value is sqrt(p) for both categorical and continuous
    variables where p is the number of variables in 'xmis'.
  }
  \item{replace}{
    logical. If 'TRUE' bootstrap sampling (with replacements) is
    performed else subsampling (without replacements).
  }
  \item{classwt}{
    list of priors of the classes in the categorical variables. This is
    equivalent to the randomForest argument, however, the user has to
    set the priors for all categorical variables in the data set (for
    continuous variables set it 'NULL').
  }
  \item{cutoff}{
    list of class cutoffs for each categorical variable. Same as with
    'classwt' (for continuous variables set it '1').
  }
  \item{strata}{
    list of (factor) variables used for stratified sampling. Same as
    with 'classwt' (for continuous variables set it 'NULL').
  }
  \item{sampsize}{
    list of size(s) of sample to draw. This is equivalent to the
    randomForest argument, however, the user has to set the sizes for
    all variables.
  }
  \item{nodesize}{
    minimum size of terminal nodes. Has to be a vector of length 2, with
    the first entry being the number for continuous variables and the
    second entry the number for categorical variables. Default is 1 for
    continuous and 5 for categorical variables.
  }
  \item{maxnodes}{
    maximum number of terminal nodes for trees in the forest.
  }
  \item{xtrue}{
    optional. Complete data matrix. This can be supplied to test the
    performance. Upon providing the complete data matrix 'verbose' will
    show the true imputation error after each iteration and the output
    will also contain the final true imputation error.
  }
}
\details{
  After each iteration the difference between the previous and the new
  imputed data matrix is assessed for the continuous and categorical
  parts. The stopping criterion is defined such that the imputation
  process is stopped as soon as both differences have become larger
  once. In case of only one type of variable the computation stops as
  soon as the corresponding difference goes up for the first
  time. However, the imputation last performed where both differences
  went up is generally less accurate than the previous one. Therefore,
  whenever the computation stops due to the stopping criterion (and not
  due to 'maxiter') the before last imputation matrix is returned.
  
  The normalized root mean squared error (NRMSE) is defined as:

  \deqn{\sqrt{\frac{mean((X_{true} - X_{imp})^2)}{var(X_{true})}}}

  where \eqn{X_{true}} the complete data matrix, \eqn{X_{imp}} the
  imputed data matrix and 'mean'/'var' being used as short notation for
  the empirical mean and variance computed over the continuous missing
  values only.

  The proportion of falsely classified (PFC) is also computed over the
  categorical missing values only.

  For feasibility reasons 'ntree', 'mtry', 'nodesize' and 'maxnodes' can
  be chosen smaller. The number of trees can be chosen fairly small
  since growing many forests (e.g. p forests in each iteration) all
  observations get predicted a few times. The runtime behaves linear
  with 'ntree'. In case of high-dimensional data we recommend using a
  small 'mtry' (e.g. 100 should work) to obtain an appropriate
  imputation result within a feasible amount of time.

  See the vignette for further examples on how to use missForest.
}
\value{
  \item{ximp}{
    imputed data matrix of same type as 'xmis'.
  }
  \item{OOBerror}{
    estimated OOB imputation error. For the set of continuous
    variables in 'xmis' the NRMSE and for the set of categorical variables
    the proportion of falsely classified entries is returned. See Details
    for the exact definition of these error measures. If 'variablewise'
    is set to 'TRUE' then this will be a vector of length 'p' where 'p' is
    the number of variables and the entries will be the OOB error for each
    variable separately.
  }
  \item{error}{
    true imputation error. This is only available if 'xtrue'
    was supplied. The error measures are the same as for 'OOBerror'.}
  }
\references{Stekhoven, D.J. and Buehlmann, P. (2012), 'MissForest -
  nonparametric missing value imputation for mixed-type data',
  Bioinformatics, 28(1) 2012, 112-118, doi: 10.1093/bioinformatics/btr597}
\author{
Daniel J. Stekhoven, <stekhoven@stat.math.ethz.ch>
}
\seealso{
  \code{\link{mixError}}, \code{\link{prodNA}}, \code{\link{randomForest}}
}
\examples{
## Nonparametric missing value imputation on mixed-type data:
data(iris)
summary(iris)

## The data contains four continuous and one categorical variable.

## Artificially produce missing values using the 'prodNA' function:
set.seed(81)
iris.mis <- prodNA(iris, noNA = 0.2)
summary(iris.mis)

## Impute missing values providing the complete matrix for
## illustration. Use 'verbose' to see what happens between iterations:
iris.imp <- missForest(iris.mis, xtrue = iris, verbose = TRUE)

## The imputation is finished after five iterations having a final
## true NRMSE of 0.143 and a PFC of 0.036. The estimated final NRMSE
## is 0.157 and the PFC is 0.025 (see Details for the reason taking
## iteration 4 instead of iteration 5 as final value).

## The final results can be accessed directly. The estimated error:
iris.imp$OOBerror

## The true imputation error (if available):
iris.imp$error

## And of course the imputed data matrix (do not run this):
## iris.imp$Ximp
}
\keyword{ nonparametric }
\keyword{ classes }
\keyword{ NA }
