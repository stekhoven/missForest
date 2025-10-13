##
## MissForest - nonparametric missing value imputation for mixed-type data
##
## This R script contains the actual missForest function.
## Ranger-backed by default with optional randomForest backend for compatibility.
## New argument: backend = c('ranger', 'randomForest')
##
## Notes
##  - When backend = 'ranger' (default): uses ranger::ranger().
##    * ntree -> num.trees
##    * nodesize (terminal, RF semantics) -> min.bucket in ranger
##    * sampsize (counts) -> sample.fraction (fractions); per-class supported
##    * classwt -> class.weights
##    * cutoff: emulated via probability forest and post-thresholding
##    * maxnodes is ignored (no 1:1 mapping). Consider max.depth if needed.
##    * OOB error pulled from $prediction.error (MSE for numeric, error rate for factor)
##    * Parallelization: variables-mode uses num.threads = 1 per task; forests-mode uses ranger internal threading.
##  - When backend = 'randomForest': retains legacy behavior and arguments as before.
##    * forests-mode uses foreach to combine sub-forests exactly like the original code.
##
## Author: D.Stekhoven, stekhoven@nexus.ethz.ch
## Dependencies (Imports): ranger, randomForest, foreach, doRNG, iterators
##############################################################################

missForest_new <- function(xmis,
                       maxiter = 10,
                       ntree = 100,
                       variablewise = FALSE,
                       decreasing = FALSE,
                       verbose = FALSE,
                       mtry = floor(sqrt(ncol(xmis))),
                       replace = TRUE,
                       classwt = NULL,
                       cutoff = NULL,
                       strata = NULL,
                       sampsize = NULL,
                       nodesize = NULL,
                       maxnodes = NULL,
                       xtrue = NA,
                       parallelize = c('no', 'variables', 'forests'),
                       num.threads = NULL,
                       backend = c('ranger', 'randomForest'))
{
  ## ----------------------------------------------------------------------
  backend <- match.arg(backend)
  
  ## helper: ranger sample.fraction from randomForest-style sampsize
  .sample_fraction <- function(sampsize_i, y, replace_flag, n_obs) {
    if (is.null(sampsize_i))
      return(if (replace_flag)
        1
        else
          0.632)
    if (length(sampsize_i) == 1L) {
      frac <- as.numeric(sampsize_i) / n_obs
      return(max(min(frac, 1), .Machine$double.eps))
    }
    if (is.factor(y) && length(sampsize_i) == nlevels(y)) {
      n_per <- as.numeric(table(y))
      frac <- as.numeric(sampsize_i) / pmax(n_per, 1L)
      frac[!is.finite(frac)] <- 0
      frac <- pmin(pmax(frac, .Machine$double.eps), 1)
      names(frac) <- levels(y)
      frac <- frac[levels(y)]
      return(frac)
    }
    if (getOption("warn"))
      warning("sampsize shape not understood; using default fraction")
    if (replace_flag)
      1
    else
      0.632
  }
  
  ## stop in case of wrong inputs passed
  n <- nrow(xmis)
  p <- ncol(xmis)
  if (!is.null(classwt))
    stopifnot(length(classwt) == p, typeof(classwt) == 'list')
  if (!is.null(cutoff))
    stopifnot(length(cutoff) == p, typeof(cutoff)  == 'list')
  if (!is.null(strata))
    stopifnot(length(strata) == p, typeof(strata)  == 'list')
  if (!is.null(nodesize))
    stopifnot(length(nodesize) == 2)
  if (backend == 'ranger' && !is.null(maxnodes) && verbose)
    message("'maxnodes' ignored with ranger; no direct equivalent.")
  
  ## remove completely missing variables
  if (any(apply(is.na(xmis), 2, sum) == n)) {
    indCmis <- which(apply(is.na(xmis), 2, sum) == n)
    xmis <- xmis[, -indCmis]
    p <- ncol(xmis)
    cat('  removed variable(s)',
        indCmis,
        'due to the missingness of all entries\n')
  }
  
  ## return feedback on parallelization setup
  parallelize <- match.arg(parallelize)
  if (parallelize %in% c('variables', 'forests')) {
    if (foreach::getDoParWorkers() == 1) {
      stop(
        "You must register a 'foreach' parallel backend to run 'missForest' in parallel. Set 'parallelize' to 'no' to compute serially."
      )
    } else if (verbose) {
      if (parallelize == 'variables')
        cat("  parallelizing over variables\n")
      if (parallelize == 'forests')
        cat("  parallelizing forest building\n")
    }
    if (foreach::getDoParWorkers() > p) {
      stop("The number of parallel cores should not exceed the number of variables (p=",
           p,
           ")")
    }
  }
  
  ## initial mean/mode imputation
  ximp <- xmis
  varType <- character(p)
  for (t.co in 1:p) {
    if (is.numeric(xmis[[t.co]])) {
      varType[t.co] <- 'numeric'
      ximp[is.na(xmis[, t.co]), t.co] <- mean(xmis[, t.co], na.rm = TRUE)
      next()
    }
    if (is.factor(xmis[[t.co]])) {
      varType[t.co] <- 'factor'
      max.level <- max(table(ximp[[t.co]]))
      class.assign <- sample(names(which(max.level == summary(ximp[[t.co]]))), 1)
      if (class.assign != "NA's") {
        ximp[is.na(xmis[[t.co]]), t.co] <- class.assign
      } else {
        while (class.assign == "NA's") {
          class.assign <- sample(names(which(max.level == summary(ximp[[t.co]]))), 1)
        }
        ximp[is.na(xmis[[t.co]]), t.co] <- class.assign
      }
      next()
    }
    stop(sprintf(
      'column %s must be factor or numeric, is %s',
      names(xmis)[t.co],
      class(xmis[[t.co]])
    ))
  }
  
  ## missingness pattern
  NAloc <- is.na(xmis)
  noNAvar <- apply(NAloc, 2, sum)
  sort.j <- order(noNAvar)
  if (decreasing)
    sort.j <- rev(sort.j)
  sort.noNAvar <- noNAvar[sort.j]
  
  ## variable parallelization indices
  nzsort.j <- sort.j[sort.noNAvar > 0]
  if (parallelize == 'variables') {
    `%cols%` <- doRNG::`%dorng%`
    idxList <- as.list(iterators::isplitVector(nzsort.j, chunkSize = foreach::getDoParWorkers()))
  }
  
  ## outputs and convergence trackers
  Ximp <- vector('list', maxiter)
  iter <- 0
  k <- length(unique(varType))
  convNew <- rep(0, k)
  convOld <- rep(Inf, k)
  OOBerror <- numeric(p)
  names(OOBerror) <- varType
  
  ## setup convergence variables w.r.t. variable types
  if (k == 1) {
    if (unique(varType) == 'numeric') {
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
  stopCriterion <- function(varType, convNew, convOld, iter, maxiter) {
    k <- length(unique(varType))
    if (k == 1) {
      (convNew < convOld) & (iter < maxiter)
    } else {
      ((convNew[1] < convOld[1]) |
         (convNew[2] < convOld[2])) & (iter < maxiter)
    }
  }
  
  ## iterate missForest
  while (stopCriterion(varType, convNew, convOld, iter, maxiter)) {
    if (iter != 0) {
      convOld <- convNew
      OOBerrOld <- OOBerr
    }
    if (verbose) {
      cat("  missForest iteration", iter + 1, "in progress...")
    }
    t.start <- proc.time()
    ximp.old <- ximp
    
    if (parallelize == 'variables') {
      for (idx in idxList) {
        results <- foreach::foreach(varInd = idx,
                                    .packages = if (backend == 'ranger')
                                      'ranger'
                                    else
                                      'randomForest') %cols% {
                                        obsi <- !NAloc[, varInd]
                                        misi <- NAloc[, varInd]
                                        obsY <- ximp[obsi, varInd]
                                        obsX <- ximp[obsi, seq_len(p)[-varInd]]
                                        misX <- ximp[misi, seq_len(p)[-varInd]]
                                        typeY <- varType[varInd]
                                        
                                        if (backend == 'ranger') {
                                          ## ranger path
                                          sf <- .sample_fraction(
                                            if (!is.null(sampsize))
                                              sampsize[[varInd]]
                                            else
                                              NULL,
                                            y = if (typeY == 'factor')
                                              factor(obsY)
                                            else
                                              obsY,
                                            replace_flag = replace,
                                            n_obs = nrow(obsX)
                                          )
                                          if (typeY == 'numeric') {
                                            RF <- ranger::ranger(
                                              x = obsX,
                                              y = obsY,
                                              num.trees = ntree,
                                              mtry = mtry,
                                              replace =
                                                replace,
                                              sample.fraction = sf,
                                              min.bucket = if (!is.null(nodesize))
                                                nodesize[1]
                                              else
                                                5,
                                              write.forest =
                                                TRUE,
                                              oob.error = TRUE,
                                              num.threads = 1,
                                              verbose =
                                                FALSE
                                            )
                                            oerr <- RF$prediction.error
                                            misY <- predict(RF, data = misX)$predictions
                                          } else {
                                            obsY <- factor(obsY)
                                            summarY <- summary(obsY)
                                            if (length(summarY) == 1) {
                                              oerr <- 0
                                              misY <- factor(rep(names(summarY), sum(misi)))
                                            }
                                            else {
                                              use_prob <- !is.null(cutoff) && !is.null(cutoff[[varInd]])
                                              RF <- ranger::ranger(
                                                x = obsX,
                                                y = obsY,
                                                num.trees = ntree,
                                                mtry = mtry,
                                                replace = replace,
                                                sample.fraction = sf,
                                                class.weights = if (!is.null(classwt))
                                                  classwt[[varInd]]
                                                else
                                                  NULL,
                                                min.bucket = if (!is.null(nodesize))
                                                  nodesize[2]
                                                else
                                                  1,
                                                # or 1 if you choose that default
                                                write.forest = TRUE,
                                                oob.error = TRUE,
                                                probability = use_prob,
                                                respect.unordered.factors = "order",
                                                num.threads = 1,
                                                # avoid nested parallelism in variables-mode
                                                verbose = FALSE
                                              )
                                              
                                              if (use_prob) {
                                                probs_oob <- predict(RF)$predictions  # OOB on training by default
                                                lev <- colnames(probs_oob)
                                                co <- cutoff[[varInd]]
                                                if (is.null(names(co)))
                                                  names(co) <- lev
                                                co <- co[lev]
                                                cls_oob <- factor(lev[max.col(sweep(probs_oob, 2, co, "/"), ties.method = "first")], levels = lev)
                                                oerr <- mean(cls_oob != obsY)
                                                
                                                probs <- predict(RF, data = misX)$predictions
                                                idxc <- max.col(sweep(probs, 2, co, "/"), ties.method = "first")
                                                misY <- factor(lev[idxc], levels = lev)
                                              } else {
                                                oerr <- RF$prediction.error
                                                misY <- predict(RF, data = misX)$predictions
                                              }
                                            }
                                          }
                                        } else {
                                          ## randomForest path (legacy)
                                          if (typeY == 'numeric') {
                                            RF <- randomForest::randomForest(
                                              x = obsX,
                                              y = obsY,
                                              ntree = ntree,
                                              mtry = mtry,
                                              replace =
                                                replace,
                                              sampsize = if (!is.null(sampsize))
                                                sampsize[[varInd]]
                                              else if (replace)
                                                nrow(obsX)
                                              else
                                                ceiling(0.632 * nrow(obsX)),
                                              nodesize = if (!is.null(nodesize))
                                                nodesize[1]
                                              else
                                                5,
                                              maxnodes = if (!is.null(maxnodes))
                                                maxnodes
                                              else
                                                NULL
                                            )
                                            oerr <- RF$mse[ntree]
                                            misY <- predict(RF, misX)
                                          } else {
                                            obsY <- factor(obsY)
                                            summarY <- summary(obsY)
                                            if (length(summarY) == 1) {
                                              oerr <- 0
                                              misY <- factor(rep(names(summarY), sum(misi)))
                                            }
                                            else {
                                              RF <- randomForest::randomForest(
                                                x = obsX,
                                                y = obsY,
                                                ntree = ntree,
                                                mtry = mtry,
                                                replace =
                                                  replace,
                                                classwt = if (!is.null(classwt))
                                                  classwt[[varInd]]
                                                else
                                                  rep(1, nlevels(obsY)),
                                                cutoff = if (!is.null(cutoff))
                                                  cutoff[[varInd]]
                                                else
                                                  rep(1 / nlevels(obsY), nlevels(obsY)),
                                                strata = if (!is.null(strata))
                                                  strata[[varInd]]
                                                else
                                                  obsY,
                                                sampsize = if (!is.null(sampsize))
                                                  sampsize[[varInd]]
                                                else if (replace)
                                                  nrow(obsX)
                                                else
                                                  ceiling(0.632 * nrow(obsX)),
                                                nodesize = if (!is.null(nodesize))
                                                  nodesize[2]
                                                else
                                                  1,
                                                maxnodes = if (!is.null(maxnodes))
                                                  maxnodes
                                                else
                                                  NULL
                                              )
                                              oerr <- RF$err.rate[[ntree, 1]]
                                              misY <- predict(RF, misX)
                                            }
                                          }
                                        }
                                        list(varInd = varInd,
                                             misY = misY,
                                             oerr = oerr)
                                      }
        for (res in results) {
          misi <- NAloc[, res$varInd]
          ximp[misi, res$varInd] <- res$misY
          OOBerror[res$varInd] <- res$oerr
        }
      }
    } else {
      ## sequential across variables, optional forest-level parallel
      for (s in 1:p) {
        varInd <- sort.j[s]
        if (noNAvar[[varInd]] == 0)
          next
        obsi <- !NAloc[, varInd]
        misi <- NAloc[, varInd]
        obsY <- ximp[obsi, varInd]
        obsX <- ximp[obsi, seq_len(p)[-varInd]]
        misX <- ximp[misi, seq_len(p)[-varInd]]
        typeY <- varType[varInd]
        
        if (backend == 'ranger') {
          sf <- .sample_fraction(
            if (!is.null(sampsize))
              sampsize[[varInd]]
            else
              NULL,
            y = if (typeY == 'factor')
              factor(obsY)
            else
              obsY,
            replace_flag = replace,
            n_obs = nrow(obsX)
          )
          if (typeY == 'numeric') {
            RF <- ranger::ranger(
              x = obsX,
              y = obsY,
              num.trees = ntree,
              mtry = mtry,
              replace = replace,
              sample.fraction = sf,
              min.bucket = if (!is.null(nodesize))
                nodesize[1]
              else
                5,
              write.forest = TRUE,
              oob.error = TRUE,
              num.threads = if (!is.null(num.threads))
                num.threads
              else if (parallelize == 'forests')
                foreach::getDoParWorkers()
              else
                NULL,
              verbose = FALSE
            )
            OOBerror[varInd] <- RF$prediction.error
            misY <- predict(RF, data = misX)$predictions
          } else {
            obsY <- factor(obsY)
            summarY <- summary(obsY)
            if (length(summarY) == 1) {
              misY <- factor(rep(names(summarY), sum(misi)))
              OOBerror[varInd] <- 0
            }
            else {
              use_prob <- !is.null(cutoff) && !is.null(cutoff[[varInd]])
              RF <- ranger::ranger(
                x = obsX,
                y = obsY,
                num.trees = ntree,
                mtry = mtry,
                replace = replace,
                sample.fraction = sf,
                class.weights = if (!is.null(classwt))
                  classwt[[varInd]]
                else
                  NULL,
                min.bucket = if (!is.null(nodesize))
                  nodesize[2]
                else
                  1,
                # or 1 if you choose that default
                write.forest = TRUE,
                oob.error = TRUE,
                probability = use_prob,
                respect.unordered.factors = "order",
                num.threads = if (!is.null(num.threads))
                  num.threads
                else if (parallelize == 'forests')
                  foreach::getDoParWorkers()
                else
                  NULL,
                verbose = FALSE
              )
              
              if (use_prob) {
                probs_oob <- predict(RF)$predictions
                lev <- colnames(probs_oob)
                co <- cutoff[[varInd]]
                if (is.null(names(co)))
                  names(co) <- lev
                co <- co[lev]
                cls_oob <- factor(lev[max.col(sweep(probs_oob, 2, co, "/"), ties.method = "first")], levels = lev)
                OOBerror[varInd] <- mean(cls_oob != obsY)
                
                probs <- predict(RF, data = misX)$predictions
                idxc <- max.col(sweep(probs, 2, co, "/"), ties.method = "first")
                misY <- factor(lev[idxc], levels = lev)
              } else {
                OOBerror[varInd] <- RF$prediction.error
                misY <- predict(RF, data = misX)$predictions
              }
            }
          }
        } else {
          ## backend == 'randomForest'
          if (typeY == 'numeric') {
            if (parallelize == 'forests') {
              RF <- foreach::foreach(
                xntree = iterators::idiv(ntree, chunks = foreach::getDoParWorkers()),
                .combine = 'combine',
                .multicombine = TRUE,
                .packages = 'randomForest'
              ) %dorng% {
                randomForest::randomForest(
                  x = obsX,
                  y = obsY,
                  ntree = xntree,
                  mtry = mtry,
                  replace = replace,
                  sampsize = if (!is.null(sampsize))
                    sampsize[[varInd]]
                  else if (replace)
                    nrow(obsX)
                  else
                    ceiling(0.632 * nrow(obsX)),
                  nodesize = if (!is.null(nodesize))
                    nodesize[1]
                  else
                    5,
                  maxnodes = if (!is.null(maxnodes))
                    maxnodes
                  else
                    NULL
                )
              }
              OOBerror[varInd] <- mean((predict(RF) - RF$y)^2, na.rm = TRUE)
            } else {
              ## parallelize == 'variables'
              RF <- randomForest::randomForest(
                x = obsX,
                y = obsY,
                ntree = ntree,
                mtry = mtry,
                replace = replace,
                sampsize = if (!is.null(sampsize))
                  sampsize[[varInd]]
                else if (replace)
                  nrow(obsX)
                else
                  ceiling(0.632 * nrow(obsX)),
                nodesize = if (!is.null(nodesize))
                  nodesize[1]
                else
                  5,
                maxnodes = if (!is.null(maxnodes))
                  maxnodes
                else
                  NULL
              )
              OOBerror[varInd] <- RF$mse[ntree]
            }
            misY <- predict(RF, misX)
          } else {
            # typeY == 'factor'
            obsY <- factor(obsY)
            summarY <- summary(obsY)
            if (length(summarY) == 1) {
              misY <- factor(rep(names(summarY), sum(misi)))
              OOBerror[varInd] <- 0
            }
            else {
              if (parallelize == 'forests') {
                RF <- foreach::foreach(
                  xntree = iterators::idiv(ntree, chunks = foreach::getDoParWorkers()),
                  .combine = 'combine',
                  .multicombine = TRUE,
                  .packages = 'randomForest'
                ) %dorng% {
                  randomForest::randomForest(
                    x = obsX,
                    y = obsY,
                    ntree = xntree,
                    mtry = mtry,
                    replace = replace,
                    classwt = if (!is.null(classwt))
                      classwt[[varInd]]
                    else
                      rep(1, nlevels(obsY)),
                    cutoff  = if (!is.null(cutoff))
                      cutoff[[varInd]]
                    else
                      rep(1 / nlevels(obsY), nlevels(obsY)),
                    strata  = if (!is.null(strata))
                      strata[[varInd]]
                    else
                      obsY,
                    sampsize = if (!is.null(sampsize))
                      sampsize[[varInd]]
                    else if (replace)
                      nrow(obsX)
                    else
                      ceiling(0.632 * nrow(obsX)),
                    nodesize = if (!is.null(nodesize))
                      nodesize[2]
                    else
                      1,
                    maxnodes = if (!is.null(maxnodes))
                      maxnodes
                    else
                      NULL
                  )
                }
                ne <- as.integer(predict(RF)) != as.integer(RF$y)
                ne <- ne[!is.na(ne)]
                OOBerror[varInd] <- sum(ne) / length(ne)
              } else {
                RF <- randomForest::randomForest(
                  x = obsX,
                  y = obsY,
                  ntree = ntree,
                  mtry = mtry,
                  replace = replace,
                  classwt = if (!is.null(classwt))
                    classwt[[varInd]]
                  else
                    rep(1, nlevels(obsY)),
                  cutoff  = if (!is.null(cutoff))
                    cutoff[[varInd]]
                  else
                    rep(1 / nlevels(obsY), nlevels(obsY)),
                  strata  = if (!is.null(strata))
                    strata[[varInd]]
                  else
                    obsY,
                  sampsize = if (!is.null(sampsize))
                    sampsize[[varInd]]
                  else if (replace)
                    nrow(obsX)
                  else
                    ceiling(0.632 * nrow(obsX)),
                  nodesize = if (!is.null(nodesize))
                    nodesize[2]
                  else
                    1,
                  maxnodes = if (!is.null(maxnodes))
                    maxnodes
                  else
                    NULL
                )
                OOBerror[varInd] <- RF$err.rate[[ntree, 1]]
              }
              misY <- predict(RF, misX)
            }
          }
        }
        ximp[misi, varInd] <- misY
      }
    }
    if (verbose)
      cat('done!\n')
    
    iter <- iter + 1
    Ximp[[iter]] <- ximp
    
    t.co2 <- 1
    ## check the difference between iteration steps
    for (t.type in names(convNew)) {
      t.ind <- which(varType == t.type)
      if (t.type == 'numeric') {
        convNew[t.co2] <- sum((ximp[, t.ind] - ximp.old[, t.ind])^2) / sum(ximp[, t.ind]^2)
      } else {
        dist <- sum(as.character(as.matrix(ximp[, t.ind])) != as.character(as.matrix(ximp.old[, t.ind])))
        convNew[t.co2] <- dist / (n * sum(varType == 'factor'))
      }
      t.co2 <- t.co2 + 1
    }
    
    ## estimated imputation error
    if (!variablewise) {
      NRMSE <- sqrt(mean(OOBerror[varType == 'numeric']) /
                      var(as.vector(as.matrix(xmis[, varType == 'numeric'])), na.rm = TRUE))
      PFC <- mean(OOBerror[varType == 'factor'])
      if (k == 1) {
        if (unique(varType) == 'numeric') {
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
    
    if (any(!is.na(xtrue))) {
      err <- suppressWarnings(mixError(ximp, xmis, xtrue))
    }
    if (verbose) {
      delta.start <- proc.time() - t.start
      if (any(!is.na(xtrue)))
        cat("    error(s):", err, "\n")
      cat("    estimated error(s):", OOBerr, "\n")
      cat("    difference(s):", convNew, "\n")
      cat("    time:", delta.start[3], "seconds\n\n")
    }
  }
  
  ## output wrt stopping rule
  if (iter == maxiter) {
    if (any(is.na(xtrue))) {
      out <- list(ximp = Ximp[[iter]], OOBerror = OOBerr)
    } else {
      out <- list(ximp = Ximp[[iter]],
                  OOBerror = OOBerr,
                  error = err)
    }
  } else {
    if (any(is.na(xtrue))) {
      out <- list(ximp = Ximp[[iter - 1]], OOBerror = OOBerrOld)
    } else {
      out <- list(
        ximp = Ximp[[iter - 1]],
        OOBerror = OOBerrOld,
        error = suppressWarnings(mixError(Ximp[[iter - 1]], xmis, xtrue))
      )
    }
  }
  class(out) <- 'missForest'
  return(out)
}
