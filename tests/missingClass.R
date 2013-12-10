## Test for handling missing classes after deletion     Author: D.Stekhoven
## ========================================================================
## Created: 13.05.2011                                  Package: missForest
library(missForest)
set.seed(119)
X1 <- rep(c(4, 3, 2, 1), each = 25)
X2 <- rnorm(100, sd = X1)
X3 <- rnorm(100, sd = 2*X1)
X <- data.frame(bin = as.factor(X1), num1 = X2, num2 = X3)
X.mis <- prodNA(X, noNA = 0.01)
X.mis[1:25,1] <- NA
X.imp <- missForest(X.mis, xtrue = X)
summary(X.imp$ximp)
if (X.imp$error[2] == 1){
  cat('  test whether missing class is handled well ... OK\n')
} else {
  stop("loosing a class in removing artificially values from data matrix made 'missForest' fail!")
}
