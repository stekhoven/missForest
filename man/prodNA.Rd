\name{prodNA}
\alias{prodNA}
\title{
  Introduce Missing Values Completely at Random (MCAR)
}
\description{
  \code{prodNA} artificially introduces missing values by deleting entries
  completely at random (MCAR) up to a specified proportion.
}
\usage{
prodNA(x, noNA = 0.1)
}
\arguments{
  \item{x}{
    A data frame or matrix to which missing values will be added. Column
    classes are preserved; factors receive \code{NA} entries.
  }
  \item{noNA}{
    Proportion of entries in \code{x} to set to \code{NA}. Must be a number
    in \eqn{[0, 1]}. The default is \code{0.1} (10\% missingness).
  }
}
\value{
  An object of the same base type as \code{x} (data frame or matrix) with
  approximately \code{noNA} proportion of its entries set to \code{NA}.
}
\details{
  Missingness is introduced independently and uniformly over all cells,
  i.e., Missing Completely At Random (MCAR). No structure by row/column or
  variable type is imposed.

  For reproducibility, call \code{\link[base]{set.seed}} before \code{prodNA}.
}
\author{
  \packageAuthor{missForest}
}
\seealso{
  \code{\link{missForest}}, \code{\link{mixError}}
}
\examples{
data(iris)

## Introduce 5\% MCAR missingness into the iris data set:
set.seed(81)
iris.mis <- prodNA(iris, noNA = 0.05)
summary(iris.mis)

## Higher missingness:
set.seed(81)
iris.mis.20 <- prodNA(iris, noNA = 0.20)
mean(is.na(as.matrix(iris.mis.20)))
}
\keyword{nonparametric}
\keyword{classes}
\keyword{NA}
