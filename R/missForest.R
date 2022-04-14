##
## MissForest - nonparametric missing value imputation for mixed-type data
##
## This R script contains the actual missForest function.
##
## Author: D.Stekhoven, stekhoven@nexus.ethz.ch
##
## Acknowledgement: Steve Weston for input regarding parallel execution (2012)
##############################################################################

missForest <- function(xmis, maxiter = 10, ntree = 100, variablewise = FALSE,
                       decreasing = FALSE, verbose = FALSE,
                       mtry = floor(sqrt(ncol(xmis))), replace = TRUE,
                       classwt = NULL, cutoff = NULL, strata = NULL,
                       sampsize = NULL, nodesize = NULL, maxnodes = NULL,
                       xtrue = NA, parallelize = c('no', 'variables', 'forests'))
{ ## ----------------------------------------------------------------------
  ## Arguments:
  ## xmis         = data matrix with missing values
  ## maxiter      = stop after how many iterations (default = 10)
  ## ntree        = how many trees are grown in the forest (default = 100)
  ## variablewise = (boolean) return OOB errors for each variable separately
  ## decreasing   = (boolean) if TRUE the columns are sorted with decreasing
  ##                amount of missing values
  ## verbose      = (boolean) if TRUE then missForest returns error estimates,
  ##                runtime and if available true error during iterations
  ## mtry         = how many variables should be tried randomly at each node
  ## replace      = (boolean) if TRUE bootstrap sampling (with replacements)
  ##                is performed, else subsampling (without replacements)
  ## classwt      = list of priors of the classes in the categorical variables
  ## cutoff       = list of class cutoffs for each categorical variable
  ## strata       = list of (factor) variables used for stratified sampling
  ## sampsize     = list of size(s) of sample to draw
  ## nodesize     = minimum size of terminal nodes, vector of length 2, with
  ##                number for continuous variables in the first entry and
  ##                number for categorical variables in the second entry
  ## maxnodes     = maximum number of terminal nodes for individual trees
  ## xtrue        = complete data matrix
  ##
  ## ----------------------------------------------------------------------
  ## Author: Daniel Stekhoven, stekhoven@nexus.ethz.ch
  
  ## stop in case of wrong inputs passed to randomForest
  n <- nrow(xmis)
  p <- ncol(xmis)
  if (!is.null(classwt))
    stopifnot(length(classwt) == p, typeof(classwt) == 'list')
  if (!is.null(cutoff))
    stopifnot(length(cutoff) == p, typeof(cutoff) == 'list')
  if (!is.null(strata))
    stopifnot(length(strata) == p, typeof(strata) == 'list')
  if (!is.null(nodesize))
    stopifnot(length(nodesize) == 2)
  
  ## remove completely missing variables
  if (any(apply(is.na(xmis), 2, sum) == n)){
    indCmis <- which(apply(is.na(xmis), 2, sum) == n)
    xmis <- xmis[,-indCmis]
    p <- ncol(xmis)
    cat('  removed variable(s)', indCmis,
        'due to the missingness of all entries\n')
  } 
  
  ## return feedback on parallelization setup
  parallelize <- match.arg(parallelize)
  if (parallelize %in% c('variables', 'forests')) {
    if (getDoParWorkers() == 1) {
      stop("You must register a 'foreach' parallel backend to run 'missForest' in parallel. Set 'parallelize' to 'no' to compute serially.")
    } else if (verbose) {
      if (parallelize == 'variables') {
        cat("  parallelizing over the variables of the input data matrix 'xmis'\n")
      } else {
        cat("  parallelizing computation of the random forest model objects\n")
      }
    }
    if (getDoParWorkers() > p){
      stop("The number of parallel cores should not exceed the number of variables (p=", p, ")")
    }
  }
  
  ## perform initial S.W.A.G. on xmis (mean imputation)
  ximp <- xmis
  varType <- character(p)
  for (t.co in 1:p) {
    if (is.numeric(xmis[[t.co]])) {
      varType[t.co] <- 'numeric'
      ximp[is.na(xmis[,t.co]),t.co] <- mean(xmis[,t.co], na.rm = TRUE)
      next()
    } 
    if (is.factor(xmis[[t.co]])) {
      varType[t.co] <- 'factor'
      ## take the level which is more 'likely' (majority vote)
      max.level <- max(table(ximp[[t.co]]))
      ## if there are several classes which are major, sample one at random
      class.assign <- sample(names(which(max.level == summary(ximp[[t.co]]))), 1)
      ## it shouldn't be the NA class
      if (class.assign != "NA's") {
        ximp[is.na(xmis[[t.co]]),t.co] <- class.assign
      } else {
        while (class.assign == "NA's") {
          class.assign <- sample(names(which(max.level ==
                                               summary(ximp[[t.co]]))), 1)
        }
        ximp[is.na(xmis[[t.co]]),t.co] <- class.assign
      }
      next()
    }
    stop(sprintf('column %s must be factor or numeric, is %s', names(xmis)[t.co], class(xmis[[t.co]])))
  }
  
  ## extract missingness pattern
  NAloc <- is.na(xmis)            # where are missings
  noNAvar <- apply(NAloc, 2, sum) # how many are missing in the vars
  sort.j <- order(noNAvar)        # indices of increasing amount of NA in vars
  if (decreasing)
    sort.j <- rev(sort.j)
  sort.noNAvar <- noNAvar[sort.j]
  
  ## compute a list of column indices for variable parallelization
  nzsort.j <- sort.j[sort.noNAvar > 0]
  if (parallelize == 'variables') {
    '%cols%' <- get('%dorng%')
    idxList <- as.list(isplitVector(nzsort.j, chunkSize = getDoParWorkers()))
  } 
  #   else {
  #     ## force column loop to be sequential
  #     '%cols%' <- get('%do%')
  #     idxList <- nzsort.j
  #   }
  
  ## output
  Ximp <- vector('list', maxiter)
  
  ## initialize parameters of interest
  iter <- 0
  k <- length(unique(varType))
  convNew <- rep(0, k)
  convOld <- rep(Inf, k)
  OOBerror <- numeric(p)
  names(OOBerror) <- varType
  
  ## setup convergence variables w.r.t. variable types
  if (k == 1){
    if (unique(varType) == 'numeric'){
      names(convNew) <- c('numeric')
    } else {
      names(convNew) <- c('factor')
    }
    convergence <- c()
    OOBerr <- numeric(1)
  } else {
    names(convNew) <- c('numeric', 'factor')
    convergence <- matrix(NA, ncol = 2)
    OOBerr <- numeric(2)
  }
  
  ## function to yield the stopping criterion in the following 'while' loop
  stopCriterion <- function(varType, convNew, convOld, iter, maxiter){
    k <- length(unique(varType))
    if (k == 1){
      (convNew < convOld) & (iter < maxiter)
    } else {
      ((convNew[1] < convOld[1]) | (convNew[2] < convOld[2])) & (iter < maxiter)
    }
  }
  
  ## iterate missForest
  while (stopCriterion(varType, convNew, convOld, iter, maxiter)){
    if (iter != 0){
      convOld <- convNew
      OOBerrOld <- OOBerr
    }
    if (verbose){
      cat("  missForest iteration", iter+1, "in progress...")
    }
    t.start <- proc.time()
    ximp.old <- ximp
    
    if (parallelize == "variables"){
      for (idx in idxList) {
        results <- foreach(varInd = idx, .packages = 'randomForest') %cols% {
          obsi <- !NAloc[, varInd] # which i's are observed
          misi <- NAloc[, varInd] # which i's are missing
          obsY <- ximp[obsi, varInd] # training response
          obsX <- ximp[obsi, seq(1, p)[-varInd]] # training variables
          misX <- ximp[misi, seq(1, p)[-varInd]] # prediction variables
          typeY <- varType[varInd]
          if (typeY == 'numeric'){
            RF <- randomForest(
              x = obsX,
              y = obsY,
              ntree = ntree,
              mtry = mtry,
              replace = replace,
              sampsize = if (!is.null(sampsize)) sampsize[[varInd]] else
                if (replace) nrow(obsX) else ceiling(0.632 * nrow(obsX)),
              nodesize = if (!is.null(nodesize)) nodesize[1] else 1,
              maxnodes = if (!is.null(maxnodes)) maxnodes else NULL)
            ## record out-of-bag error
            oerr <- RF$mse[ntree]
            #           }
            ## predict missing values in column varInd
            misY <- predict(RF, misX)
          } else { # if Y is categorical          
            obsY <- factor(obsY) ## remove empty classes
            summarY <- summary(obsY)
            if (length(summarY) == 1){ ## if there is only one level left
              oerr <- 0
              misY <- factor(rep(names(summarY), length(misi)))
            } else {
              RF <- randomForest(
                x = obsX,
                y = obsY,
                ntree = ntree,
                mtry = mtry,
                replace = replace,
                classwt = if (!is.null(classwt)) classwt[[varInd]] else
                  rep(1, nlevels(obsY)),
                cutoff = if (!is.null(cutoff)) cutoff[[varInd]] else
                  rep(1/nlevels(obsY), nlevels(obsY)),
                strata = if (!is.null(strata)) strata[[varInd]] else obsY,
                sampsize = if (!is.null(sampsize)) sampsize[[varInd]] else
                  if (replace) nrow(obsX) else ceiling(0.632*nrow(obsX)),
                nodesize = if (!is.null(nodesize)) nodesize[2] else 5,
                maxnodes = if (!is.null(maxnodes)) maxnodes else NULL)
              ## record out-of-bag error
              oerr <- RF$err.rate[[ntree,1]]
              #             }
              ## predict missing values in column varInd
              misY <- predict(RF, misX)
            }
          }
          list(varInd = varInd, misY = misY, oerr = oerr)
        }
        ## update the master copy of the data
        for (res in results) {
          misi <- NAloc[,res$varInd]
          ximp[misi, res$varInd] <- res$misY
          OOBerror[res$varInd] <- res$oerr
        }
      }
    } else { # if parallelize != "variables"
      for (s in 1 : p) {
        varInd <- sort.j[s]
        if (noNAvar[[varInd]] != 0) {
          obsi <- !NAloc[, varInd]
          misi <- NAloc[, varInd]
          obsY <- ximp[obsi, varInd]
          obsX <- ximp[obsi, seq(1, p)[-varInd]]
          misX <- ximp[misi, seq(1, p)[-varInd]]
          typeY <- varType[varInd]
          if (typeY == "numeric") {
            if (parallelize == 'forests') {
              xntree <- NULL
              RF <- foreach(xntree = idiv(ntree, chunks = getDoParWorkers()),
                            .combine = 'combine', .multicombine = TRUE,
                            .packages = 'randomForest') %dorng% {
                              randomForest( x = obsX,
                                            y = obsY,
                                            ntree = xntree,
                                            mtry = mtry,
                                            replace = replace,
                                            sampsize = if (!is.null(sampsize)) sampsize[[varInd]] else
                                              if (replace) nrow(obsX) else ceiling(0.632 * nrow(obsX)),
                                            nodesize = if (!is.null(nodesize)) nodesize[1] else 1,
                                            maxnodes = if (!is.null(maxnodes)) maxnodes else NULL)
                            }
              ## record out-of-bag error
              OOBerror[varInd] <- mean((predict(RF) - RF$y) ^ 2, na.rm = TRUE)
#               OOBerror[varInd] <- RF$mse[ntree]
            } else {
              RF <- randomForest( x = obsX,
                                  y = obsY,
                                  ntree = ntree,
                                  mtry = mtry,
                                  replace = replace,
                                  sampsize = if (!is.null(sampsize)) sampsize[[varInd]] else
                                    if (replace) nrow(obsX) else ceiling(0.632 * nrow(obsX)),
                                  nodesize = if (!is.null(nodesize)) nodesize[1] else 1,
                                  maxnodes = if (!is.null(maxnodes)) maxnodes else NULL)
              ## record out-of-bag error
              OOBerror[varInd] <- RF$mse[ntree]
            }
          misY <- predict(RF, misX)
          } else {
            obsY <- factor(obsY)
            summarY <- summary(obsY)
            if (length(summarY) == 1) {
              misY <- factor(rep(names(summarY), sum(misi)))
            } else {
              if (parallelize == 'forests') {
                RF <- foreach(xntree = idiv(ntree, chunks = getDoParWorkers()),
                              .combine = 'combine', .multicombine = TRUE,
                              .packages = 'randomForest') %dorng% {
                                randomForest(
                                  x = obsX,
                                  y = obsY,
                                  ntree = xntree,
                                  mtry = mtry,
                                  replace = replace,
                                  classwt = if (!is.null(classwt)) classwt[[varInd]] else
                                    rep(1, nlevels(obsY)),
                                  cutoff = if (!is.null(cutoff)) cutoff[[varInd]] else
                                    rep(1/nlevels(obsY), nlevels(obsY)),
                                  strata = if (!is.null(strata)) strata[[varInd]] else obsY,
                                  sampsize = if (!is.null(sampsize)) sampsize[[varInd]] else
                                    if (replace) nrow(obsX) else ceiling(0.632 * nrow(obsX)),
                                  nodesize = if (!is.null(nodesize)) nodesize[2] else 5,
                                  maxnodes = if (!is.null(maxnodes)) maxnodes else NULL)
                              }
                ## record out-of-bag error
                ne <- as.integer(predict(RF)) != as.integer(RF$y)
                ne <- ne[! is.na(ne)]
                OOBerror[varInd] <- sum(ne) / length(ne)
              } else {
                RF <- randomForest(x = obsX, 
                                   y = obsY, 
                                   ntree = ntree, 
                                   mtry = mtry, 
                                   replace = replace, 
                                   classwt = if (!is.null(classwt)) classwt[[varInd]] else 
                                     rep(1, nlevels(obsY)),
                                   cutoff = if (!is.null(cutoff)) cutoff[[varInd]] else 
                                     rep(1 / nlevels(obsY), nlevels(obsY)),
                                   strata = if (!is.null(strata)) strata[[varInd]] else obsY, 
                                   sampsize = if (!is.null(sampsize)) sampsize[[varInd]] else 
                                     if (replace) nrow(obsX) else ceiling(0.632 * nrow(obsX)), 
                                   nodesize = if (!is.null(nodesize)) nodesize[2] else 5, 
                                   maxnodes = if (!is.null(maxnodes)) maxnodes else NULL)
                ## record out-of-bag error
                OOBerror[varInd] <- RF$err.rate[[ntree, 1]]
              }
              ## predict missing parts of Y
              misY <- predict(RF, misX)
            }
          }
          ximp[misi, varInd] <- misY
        }
      }
    }
    if (verbose){
      cat('done!\n')
    }
    
    iter <- iter + 1
    Ximp[[iter]] <- ximp
    
    t.co2 <- 1
    ## check the difference between iteration steps
    for (t.type in names(convNew)){
      t.ind <- which(varType == t.type)
      if (t.type == 'numeric'){
        convNew[t.co2] <- sum((ximp[, t.ind] - ximp.old[, t.ind])^2) / sum(ximp[, t.ind]^2)
      } else {
        dist <- sum(as.character(as.matrix(ximp[, t.ind])) != as.character(as.matrix(ximp.old[, t.ind])))
        convNew[t.co2] <- dist / (n * sum(varType == 'factor'))
      }
      t.co2 <- t.co2 + 1
    }
    
    ## compute estimated imputation error
    if (!variablewise){
      NRMSE <- sqrt(mean(OOBerror[varType == 'numeric'])/
                      var(as.vector(as.matrix(xmis[, varType == 'numeric'])),
                          na.rm = TRUE))
      PFC <- mean(OOBerror[varType == 'factor'])
      if (k == 1){
        if (unique(varType) == 'numeric'){
          OOBerr <- NRMSE
          names(OOBerr) <- 'NRMSE'
        } else {
          OOBerr <- PFC
          names(OOBerr) <- 'PFC'
        }
      } else {
        OOBerr <- c(NRMSE, PFC)
        names(OOBerr) <- c('NRMSE', 'PFC')
      }
    } else {
      OOBerr <- OOBerror
      names(OOBerr)[varType == 'numeric'] <- 'MSE'
      names(OOBerr)[varType == 'factor'] <- 'PFC'
    }
    
    if (any(!is.na(xtrue))){
      err <- suppressWarnings(mixError(ximp, xmis, xtrue))
    }
    
    ## return status output, if desired
    if (verbose){
      delta.start <- proc.time() - t.start
      if (any(!is.na(xtrue))){
        cat("    error(s):", err, "\n")
      }
      cat("    estimated error(s):", OOBerr, "\n")
      cat("    difference(s):", convNew, "\n")
      cat("    time:", delta.start[3], "seconds\n\n")
    }
  }#end while((convNew<convOld)&(iter<maxiter)){
  
  ## produce output w.r.t. stopping rule
  if (iter == maxiter){
    if (any(is.na(xtrue))){
      out <- list(ximp = Ximp[[iter]], OOBerror = OOBerr)
    } else {
      out <- list(ximp = Ximp[[iter]], OOBerror = OOBerr, error = err)
    }
  } else {
    if (any(is.na(xtrue))){
      out <- list(ximp = Ximp[[iter - 1]], OOBerror = OOBerrOld)
    } else {
      out <- list(ximp = Ximp[[iter - 1]], OOBerror = OOBerrOld,
                  error = suppressWarnings(mixError(Ximp[[iter - 1]], xmis, xtrue)))
    }
  }
  class(out) <- 'missForest'
  return(out)
}
