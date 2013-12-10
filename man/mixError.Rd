\name{mixError}
\alias{mixError}
\title{
Compute Imputation Error for Mixed-type Data
}
\description{
  'mixError' is used to calculate the imputation error particularly in
  the case of mixed-type data. Given the complete data matrix and the
  data matrix containing the missing values the normalized root mean
  squared error for the continuous and the proportion of falsely
  classified entries for the categorical variables are computed.
}
\usage{
mixError(ximp, xmis, xtrue)
}
\arguments{
  \item{ximp}{
    imputed data matrix with variables in the columns and observations in
    the rows. Note there should not be any missing values.
  }
  \item{xmis}{
    data matrix with missing values.
  }
  \item{xtrue}{
    complete data matrix. Note there should not be any missing values.
  }
}
\value{
imputation error. In case of continuous variables only this is the
normalized root mean squared error (NRMSE, see 'help(missForest)' for
further details). In case of categorical variables onlty this is the
proportion of falsely classified entries (PFC). In case of mixed-type
variables both error measures are supplied.
}
\author{
Daniel J. Stekhoven, <stekhoven@stat.math.ethz.ch>
}
\note{
This function is internally used by \code{\link{missForest}} whenever a complete
data matrix is supplied.
}
\seealso{
\code{\link{missForest}}
}
\examples{
## Compute imputation error for mixed-type data:
data(iris)

## Artificially produce missing values using the 'prodNA' function:
set.seed(81)
iris.mis <- prodNA(iris, noNA = 0.2)

## Impute missing values using 'missForest':
iris.imp <- missForest(iris.mis)

## Compute the true imputation error manually:
err.imp <- mixError(iris.imp$ximp, iris.mis, iris)
err.imp
}
\keyword{ classes }
\keyword{ NA }% __ONLY ONE__ keyword per line
