\name{prodNA}
\alias{prodNA}
\title{
Introduce Missing Values Completely at Random
}
\description{
'prodNA' artificially introduces missing values. Entries in the given
dataframe are deleted completely at random up to the specified amount.
}
\usage{
prodNA(x, noNA = 0.1)
}
\arguments{
  \item{x}{
    dataframe subjected to missing value introduction.
  }
\item{noNA}{
  proportion of missing values w.r.t. the number of entries
    of 'x'.
  }
}
\value{
 dataframe with missing values.
}
\author{
Daniel J. Stekhoven, <stekhoven@stat.math.ethz.ch>
}
\seealso{
\code{\link{missForest}}
}
\examples{
data(iris)
## Introduce 5% of missing values to the iris data set
iris.mis <- prodNA(iris, 0.05)
summary(iris.mis)
}

\keyword{ classes }
\keyword{ NA }
