# missForest

missForest is a nonparametric, mixed-type imputation method for basically any type of data.  
Here, we host the R-package "missForest" for the statistical software R.  

The method is based on the publication Stekhoven and Bühlmann, 2012. The R package contains a vignette on how to use "missForest" in R including many helpful examples.  

## Upcoming

Currently, we are working on going multiple imputation with missForest. We are testing several ways of doing it, including an implicit approach making multiple full data set imputations potentially unnecessary. We expect to see these first extensions in the second half of 2019.

Deal with tibbles and variable attributes.

### Potential innovations alongside: 

- storage of missForests (if this is feasible, it could be used for predictions)
- housekeeping the code for efficiency and safety

### For later...

Stuff we consider less interesting  - write us if you disagree:

- different stopping criteria (missForest is very well performing on the existing criterion, we see no need to adjust for this)
- random seed tracking/setting for fully reproducible imputation results (due to the little variability in the estimation of missForest - even if it is stochastic - results are _quasi_ reproducible)
- computation time estimation (harder than we thought and not so pressing)

## Contact us

Contact me by email: stekhoven@nexus.ethz.ch  

References: 
Stekhoven, D.J. and Buehlmann, P. (2012), 'MissForest - nonparametric missing value imputation  for mixed-type data', Bioinformatics, 28(1) 2012, 112-118, doi: 10.1093/bioinformatics/btr597
