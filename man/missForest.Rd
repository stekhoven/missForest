\name{missForest}
\alias{missForest}
\title{
  Nonparametric Missing Value Imputation using Random Forests (ranger or randomForest)
}
\description{
  \code{missForest} imputes missing values for mixed-type data (numeric and
  categorical). It models complex interactions and nonlinear relations and
  returns an out-of-bag (OOB) imputation error estimate. It supports
  parallel execution and offers two backends: \pkg{ranger} (default) and
  \pkg{randomForest} (legacy/compatibility).
}
\usage{
missForest(xmis, maxiter = 10, ntree = 100, variablewise = FALSE,
           decreasing = FALSE, verbose = FALSE,
           mtry = floor(sqrt(ncol(xmis))), replace = TRUE,
           classwt = NULL, cutoff = NULL, strata = NULL,
           sampsize = NULL, nodesize = NULL, maxnodes = NULL,
           xtrue = NA, parallelize = c("no", "variables", "forests"),
           num.threads = NULL, backend = c("ranger", "randomForest"))
}
\arguments{
  \item{xmis}{
    A data frame or matrix with missing values. Columns are variables,
    rows are observations. All columns must be \code{numeric} or \code{factor}
    (character columns should be converted to factors beforehand).
  }
  \item{maxiter}{
    Maximum number of iterations unless the stopping criterion is met earlier.
  }
  \item{ntree}{
    Number of trees to grow in each per-variable forest.
  }
  \item{variablewise}{
    Logical. If \code{TRUE}, return an OOB error per variable; otherwise
    report one error for numeric variables (NRMSE) and one for factors (PFC).
  }
  \item{decreasing}{
    Logical. If \code{FALSE}, variables are processed in increasing order of
    missingness.
  }
  \item{verbose}{
    Logical. If \code{TRUE}, print iteration-wise diagnostics (estimated error,
    runtime, and—if \code{xtrue} is given—the true error).
  }
  \item{mtry}{
    Number of candidate variables at each split. Passed to the backend
    (\pkg{randomForest} or \pkg{ranger}). Default is \eqn{\sqrt{p}}.
  }
  \item{replace}{
    Logical. If \code{TRUE}, bootstrap sampling (with replacement) is used;
    otherwise subsampling (without replacement).
  }
  \item{classwt}{
    List of class priors for the categorical variables. Same list semantics as
    in \pkg{randomForest}: one element per variable (set \code{NULL} for numeric
    variables). With backend \code{"ranger"}, this maps to \code{class.weights}.
  }
  \item{cutoff}{
    List of per-class cutoff vectors for each categorical variable. As in
    \pkg{randomForest}, one element per factor variable. With backend
    \code{"ranger"}, cutoffs are emulated by fitting a probability forest and
    thresholding predicted class probabilities post-hoc.
  }
  \item{strata}{
    List of (factor) variables used for stratified sampling (legacy
    \pkg{randomForest} semantics). Ignored by \pkg{ranger}.
  }
  \item{sampsize}{
    List of sample sizes per variable (legacy \pkg{randomForest} semantics).
    With backend \code{"ranger"}, these are converted to \code{sample.fraction}
    (overall or per-class fractions, as appropriate).
  }
  \item{nodesize}{
    Minimum node size. A numeric vector of length 2:
    \emph{first} entry for \strong{numeric} variables, \emph{second} for
    \strong{factor} variables. \strong{Default:} \code{c(5, 1)}.
    With backend \code{"ranger"}, this maps to \code{min.bucket} (no exact
    1:1 to randomForest's terminal-node semantics).
  }
  \item{maxnodes}{
    Maximum number of terminal nodes per tree. Used with backend
    \code{"randomForest"}. With \code{"ranger"}, this argument is ignored
    (consider \code{max.depth} at the \pkg{ranger} level if needed).
  }
  \item{xtrue}{
    Optional complete data matrix for benchmarking. If provided, the
    iteration log includes the true imputation error, and the return value
    includes it as \code{$error}.
  }
  \item{parallelize}{
    Should \code{missForest} run in parallel? One of
    \code{"no"}, \code{"variables"}, or \code{"forests"}.
    \describe{
      \item{\code{"variables"}}{Forests for different variables are built in
        parallel using a registered \pkg{foreach} backend.}
      \item{\code{"forests"}}{Within a variable, the forest is built using
        the backend's threading (for \code{"ranger"}) or via
        \pkg{foreach} sub-forests (for \code{"randomForest"}).}
    }
    Which choice is faster depends on data shape and backend.
  }
  \item{num.threads}{
    Integer (or \code{NULL}). Number of threads for \pkg{ranger}. If
    \code{parallelize = "variables"}, per-variable \pkg{ranger} calls use
    \code{num.threads = 1} internally to avoid nested oversubscription.
    Otherwise, if \code{NULL}, \pkg{ranger}'s default is used.
    Ignored by \code{"randomForest"}.
  }
  \item{backend}{
    Character. \code{"ranger"} (default) uses \pkg{ranger} for forest fitting;
    \code{"randomForest"} retains legacy behavior for compatibility.
  }
}
\details{
  \strong{Algorithm.} The method iteratively imputes each variable with missing
  values by fitting a random forest on the observed part of that variable and
  the current imputations of all other variables. After each iteration, the
  difference between the current and previous imputed matrices is computed
  separately for numeric and factor columns. The stopping rule is met once both
  differences have increased at least once (or only the present type increases
  if there is only one type). In that case, the \emph{previous} imputation
  (before the increase) is returned. Otherwise, the process stops at
  \code{maxiter}.

  \strong{Backends.} With \code{backend = "ranger"}, arguments are mapped as:
  \itemize{
    \item \code{ntree} $\rightarrow$ \code{num.trees}
    \item \code{nodesize} (numeric/factor) $\rightarrow$ \code{min.bucket}
          for regression/classification, respectively (defaults used here are
          \code{c(5,1)}).
    \item \code{sampsize} (counts) $\rightarrow$ \code{sample.fraction}
          (overall or per-class fractions).
    \item \code{classwt} $\rightarrow$ \code{class.weights}.
    \item \code{cutoff}: emulated via probability forests and post-thresholding.
    \item \code{maxnodes}: no direct equivalent in \pkg{ranger} (ignored).
  }
  The reported OOB error uses \pkg{ranger}'s \code{$prediction.error}
  (MSE for numeric, error rate for factors), except when \code{cutoff} is used:
  in that case, the misclassification rate is computed by applying the cutoffs
  to OOB class probabilities.

  \strong{Parallelization.} Two modes are available via \code{parallelize}:
  \itemize{
    \item \code{"variables"}: different variables are imputed in parallel
          using \pkg{foreach}; per-variable \pkg{ranger} calls use
          \code{num.threads = 1}.
    \item \code{"forests"}: a single variable’s forest is built using
          \pkg{ranger} multithreading (controlled by \code{num.threads}) or,
          for \code{"randomForest"}, by combining sub-forests via \pkg{foreach}.
  }
  Make sure you have registered a parallel backend if you choose a parallel mode.

  See the vignette for further examples and discussion.
}
\value{
  \item{ximp}{
    Imputed data matrix (same classes as \code{xmis}).
  }
  \item{OOBerror}{
    Estimated OOB imputation error. For numeric variables, the normalized
    root mean squared error (NRMSE); for factors, the proportion falsely
    classified (PFC). If \code{variablewise = TRUE}, a vector of length
    \eqn{p} with per-variable errors is returned (labeled \code{"MSE"} for
    numeric and \code{"PFC"} for factors).
  }
  \item{error}{
    True imputation error (NRMSE/PFC), present only if \code{xtrue} was given.
  }
}
\references{
Stekhoven, D. J. and Bühlmann, P. (2012).
MissForest—nonparametric missing value imputation for mixed-type data.
\emph{Bioinformatics}, 28(1), 112–118. \doi{10.1093/bioinformatics/btr597}
}
\author{
Daniel J. Stekhoven, \email{stekhoven@stat.math.ethz.ch}
}
\seealso{
  \code{\link{mixError}}, \code{\link{prodNA}},
  \code{\link[randomForest]{randomForest}},
  \code{\link[ranger]{ranger}}
}
\examples{
## Mixed-type imputation on iris:
data(iris)
set.seed(81)
iris.mis <- prodNA(iris, noNA = 0.2)

## Default: ranger backend
imp_rg <- missForest(iris.mis, xtrue = iris, verbose = TRUE)
imp_rg$OOBerror
imp_rg$error  # requires xtrue

## Legacy behavior: randomForest backend
imp_rf <- missForest(iris.mis, backend = "randomForest", verbose = TRUE)

## Parallel examples (register a backend first, e.g., doParallel):
## Not run:
# library(doParallel)
# registerDoParallel(2)
# imp_vars <- missForest(iris.mis, parallelize = "variables", verbose = TRUE)
# imp_fors <- missForest(iris.mis, parallelize = "forests", verbose = TRUE,
#                        num.threads = 2)  # used by ranger
## End(Not run)
}
\keyword{nonparametric}
\keyword{classes}
\keyword{NA}
