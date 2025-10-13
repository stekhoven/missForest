\name{varClass}
\alias{varClass}
\title{
  Extract Variable Types from a Data Frame
}
\description{
  \code{varClass} returns the variable types of a data frame. It is used
  internally in several functions of the \pkg{missForest} package.
}
\usage{
varClass(x)
}
\arguments{
  \item{x}{A data frame with variables in the columns.}
}
\value{
  A character vector of length \eqn{p}, where \eqn{p} is the number of columns in \code{x}.
  Entries are \code{"numeric"} for continuous variables and \code{"factor"} for categorical variables.
}
\author{
  \packageAuthor{missForest}
}
\note{
  This function is used internally by \code{\link{missForest}} and \code{\link{mixError}}.
}
\seealso{
  \code{\link{missForest}}, \code{\link{mixError}}, \code{\link{nrmse}}
}
\examples{
data(iris)
varClass(iris)

## We have four continuous and one categorical variable.
}
\keyword{classes}
