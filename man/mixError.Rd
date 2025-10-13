\name{mixError}
\alias{mixError}
\title{
  Compute Imputation Error for Mixed-type Data
}
\description{
  \code{mixError} computes imputation error for mixed-type data given the
  imputed matrix (\code{ximp}), the original matrix with missing values
  (\code{xmis}), and the complete ground truth (\code{xtrue}). It reports the
  normalized root mean squared error (NRMSE) for numeric variables and the
  proportion of falsely classified entries (PFC) for factor variables.
}
\usage{
mixError(ximp, xmis, xtrue)
}
\arguments{
  \item{ximp}{
    Imputed data matrix (or data frame) with variables in columns and
    observations in rows. There must be no missing values.
  }
  \item{xmis}{
    Data matrix (or data frame) with missing values used to derive the
    missingness pattern.
  }
  \item{xtrue}{
    Complete data matrix (or data frame) containing the true values. There
    must be no missing values.
  }
}
\value{
  A named vector with the imputation error(s):
  \itemize{
    \item \code{NRMSE}: normalized root mean squared error computed over the
      numeric entries that were missing in \code{xmis}.
    \item \code{PFC}: proportion of falsely classified entries computed over
      the factor entries that were missing in \code{xmis}.
  }
  If only one type (numeric or factor) is present among the missing entries,
  only the corresponding error is returned.
}
\note{
  Columns are treated by their \R classes: numeric metrics are computed for
  \code{numeric} columns and classification metrics for \code{factor}
  columns. Character columns should be converted to factors beforehand.

  This function is used internally by \code{\link{missForest}} when a complete
  matrix \code{xtrue} is supplied.
}
\seealso{
  \code{\link{missForest}}, \code{\link{nrmse}}
}
\references{
  \insertRef{StekhovenBuehlmann2012}{missForest}

  For the NRMSE notion in imputation benchmarking:
  \insertRef{Oba2003}{missForest}
}
\author{
  \packageAuthor{missForest}
}
\examples{
## Mixed-type error computation on iris:
data(iris)

## Introduce missingness:
set.seed(81)
iris.mis <- prodNA(iris, noNA = 0.2)

## Impute:
iris.imp <- missForest(iris.mis)

## Compute the true imputation error:
err.imp <- mixError(iris.imp$ximp, iris.mis, iris)
err.imp
}
\keyword{nonparametric}
\keyword{classes}
\keyword{NA}
