\name{nrmse}
\alias{nrmse}
\title{
  Normalized Root Mean Squared Error
}
\description{
  'nrmse' computes the normalized root mean squared error for a given
  complete data matrix, imputed data matrix and the data matrix containing
  missing values.
}
\usage{
nrmse(ximp, xmis, xtrue)
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
  see Title.
}
\references{
  Oba et al. (2003), 'A Bayesian missing value estimation method for gene
  expression profile data', Bioinformatics, 19(16), 2088-2096
}
\author{
  Daniel J. Stekhoven, <stekhoven@stat.math.ethz.ch>
}
\note{
  The NRMSE can only be computed for continuous data. For categorical or
  mixed-type data see \code{\link{mixError}}.

  This function is internally used by \code{\link{mixError}}.
}
\seealso{
  \code{\link{mixError}}
}
\keyword{ error }
