\name{missForest-package}
\alias{missForest-package}
\docType{package}
\title{
Nonparametric Missing Value Imputation using Random Forest (ranger by default)
}
\description{
The \pkg{missForest} package provides nonparametric missing-value imputation for mixed-type data (continuous and categorical). It models each variable with missingness using random forests that learn complex interactions and nonlinear relations and returns out-of-bag (OOB) error estimates. The default backend is \pkg{ranger} for speed and scalability, with an optional legacy \pkg{randomForest} backend for backward compatibility. Parallelization is supported either across variables (via \pkg{foreach}/\pkg{doRNG}) or within forests (via \pkg{ranger} threads).
}
\details{
\tabular{ll}{
Package: \tab missForest\cr
Type: \tab Package\cr
Version: \tab 1.6\cr
Date: \tab 2025-10-13\cr
License: \tab GPL (>= 2)\cr
}

The main function is \code{\link{missForest}}, which iteratively imputes missing entries by fitting per-variable random forests to the currently imputed data matrix. The implementation now defaults to a \pkg{ranger}-based backend while preserving the original \pkg{randomForest}-based behavior via the \code{backend} argument. See \code{\link{missForest}} for arguments, details on stopping criteria, OOB error reporting (NRMSE for numeric variables and PFC for factors), and parallel options.
}
\author{
\packageAuthor{missForest}
}
\references{
\insertRef{StekhovenBuehlmann2012}{missForest}
}
\seealso{
\code{\link{missForest}}, \code{\link{mixError}}, \code{\link{prodNA}}
}
\keyword{package}
\keyword{nonparametric}
\keyword{classes}
\keyword{NA}
