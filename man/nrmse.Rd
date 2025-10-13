\name{nrmse}
\alias{nrmse}
\title{Normalized root mean squared error}
\description{
  \code{nrmse} computes the normalized root mean squared error (NRMSE)
  for a given complete data matrix \code{xtrue}, an imputed matrix
  \code{ximp}, and the corresponding matrix with missing values \code{xmis}.
}
\usage{
nrmse(ximp, xmis, xtrue)
}
\arguments{
  \item{ximp}{
    Imputed data matrix (or data frame) with variables in columns and
    observations in rows. Must be numeric and contain no missing values.
  }
  \item{xmis}{
    Data matrix (or data frame) with the original missing values. Its
    dimensions and column order must match \code{ximp} and \code{xtrue}.
  }
  \item{xtrue}{
    Complete data matrix (or data frame). Must be numeric and contain no
    missing values. Dimensions and column order must match \code{xmis}/\code{ximp}.
  }
}
\details{
  The NRMSE is computed over the entries that were missing in \code{xmis}
  and are numeric in \code{xtrue} / \code{ximp}, using
  \deqn{\mathrm{NRMSE} = \sqrt{\frac{\mathrm{mean}\{(X_{\mathrm{true}} - X_{\mathrm{imp}})^2\}}
                                       {\mathrm{var}(X_{\mathrm{true}})}}\,,}
  where \eqn{\mathrm{mean}} and \eqn{\mathrm{var}} are the empirical mean
  and variance computed over the continuous missing entries only.
  This measure is intended for continuous data; for categorical or mixed-type
  data, see \code{\link{mixError}}.
}
\value{
  A numeric scalar: the normalized root mean squared error.
}
\references{
\insertRef{Oba2003}{missForest}
}
\author{
\packageAuthor{missForest}
}
\note{
  This function is used internally by \code{\link{mixError}}.
}
\seealso{
  \code{\link{mixError}}, \code{\link{missForest}}
}
\examples{
## Simple numeric example
set.seed(1)
xtrue <- matrix(rnorm(100), ncol = 5)
xmis  <- xtrue
xmis[sample(length(xmis), 10)] <- NA
ximp  <- xmis
ximp[is.na(ximp)] <- rowMeans(ximp, na.rm = TRUE)[row(ximp)[is.na(ximp)]]

nrmse(ximp, xmis, xtrue)
}
\keyword{error}
