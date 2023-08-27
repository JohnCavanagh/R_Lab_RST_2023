##################################### Meta ##########################################

# Clustered SE
# Collaborators: Krishanu Chakraborty, James Dunham, Vikram Jambulapati
# Created 27th November 2016
# Original exercise in STATA written by John Tebes and Rohit Naimpally
# Based on Slawa Rokicki's blog : R for public health 
# (http://rforpublichealth.blogspot.in/2014/10/easy-clustered-standard-errors-in-r.html)
# Version : 2.0.0
# Last edited by : Krishanu Chakraborty

#################################### Function Body ##################################

#' Estimate confidence intervals for regression coefficients
#' 
#' This is a helper function for cluster_summary().
#'
#' @param model A fitted model (object of class 'lm') 
#' @param robust_vcov A vector of cluster indicators 
confidence_intervals <- function(model, robust_vcov) {
  require(lmtest)
  t <- qt(.975, model$df.residual)
  ct <- coeftest(model, robust_vcov)
  est <- cbind(ct[,1], ct[,1] - t * ct[,2], ct[,1] + t * ct[,2])
  colnames(est) <- c("Estimate", "LowerCI", "UpperCI")
  return(est)
}

#' Summarize a fitted model with cluster-robust estimates of uncertainty 
#'
#' @param model A fitted model (object of class 'lm') 
#' @param cluster A vector of cluster indicators 
#' @return Return
cluster_summary <- function(model, cluster) {
  # We'll use the cluster.vcov and waldtest functions from the multiwayvcov
  # package, and coeftest from the lmtest package
  require(multiwayvcov)
  require(lmtest)
  
  robust_vcov <- cluster.vcov(model, cluster)
  coef_summary <- coeftest(model, robust_vcov)
  wald_summary <- waldtest(model, vcov = robust_vcov, test = "F")
  ci_table <- confidence_intervals(model, robust_vcov)
  
  return(list(coef_summary, wald_summary, ci_table))
}

# Demonstration #
# 
# library(haven)
# airquality = read_dta('airquality.dta')
# head(airquality)
# 
# fit <- lm(Ozone ~ Wind + Temp, data = airquality)
# cluster_summary(fit, airquality$Month)

# The cluster-robust standard errors given in the first element of the output
# are 21.75, 1.17, and 0.23, for the intercept and the coefficients on Wind and
# Temp, respectively.

# These estimates are identical to those in the output from Stata after running:
#
# use airquality
# reg Ozone Wind Temp, vce(cluster Month)