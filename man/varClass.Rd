\name{varClass}
\alias{varClass}
\title{
  Extract Variable Types from a Dataframe
}
\description{
'varClass' returns the variable types of a dataframe. It is used
internally in several functions of the 'missForest'-package.
}
\usage{
varClass(x)
}
\arguments{
  \item{x}{data frame with variables in the columns.
}
}
\value{
a vector of length p where p denotes the number of columns in 'x'. The
entries are "numeric" for continuous variables and "factor" for
categorical variables.
}
\author{
Daniel J. Stekhoven, <stekhoven@stat.math.ethz.ch>
}
\note{
This function is internally used by \code{\link{missForest}} and \code{\link{mixError}}.
}
\seealso{
\code{\link{missForest}}, \code{\link{mixError}}, \code{\link{nrmse}}
}
\examples{
data(iris)
varClass(iris)

## We have four continuous and one categorical variable.
}
\keyword{ classes }
