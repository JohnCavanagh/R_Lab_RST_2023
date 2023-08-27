#' ---
#' title: "Power Calculations and Simulations - R lab"
#' ---
#' 


#' Please install these packages if you don't have them:
#' 
## install.packages(c("haven", "ICC", "randomizr", "multiwayvcov", "lmtest", "knitr","devtools", "Hmisc"), dependencies=TRUE, INSTALL_opts = c('--no-lock'))
## library(devtools)
## devtools::install_github('vikjam/pwrcalc')
## If you have trouble installing pwrcalc, you can download it manually by using the .tar.gz file provided. 
## To install it manually, go to "Packages", select "Install" and then "Package Archive" in the install menu. 
## Browse to the pwrcalc_0.0.0.9001.tar.gz file and select "install". 



#' Once you installed the packages, load them to get started:
#' 
library(haven)
library(ICC)
library(randomizr)
library(multiwayvcov)
library(lmtest)
library(knitr)
library(pwrcalc)
library(Hmisc)


#' Before we get started make sure you've set your directory properly. You can check your directory with `getwd` and if you need to change it you can set it with `setwd`:
#' 
## getwd()
## setwd("D:/R/J-PAL Power Tutorial")  //change directory


#' # Non-parametric Power Simulations
#' 
#' Non-parametric power simulations do better than parametric power calculations when we have access to good data (historical, baseline, or pilot) on our study population. From these data, we can simulate a fake dataset that assumes the treatment has no effect and then see what effects we are powered to detect, by looking at the simulated 95% confidence interval around our null effect. We should expect that any effect greater than this confidence interval would be detected by our study.
#' 
#' In particular, power simulations do not require the assumption that the sampling distribution of your Beta coefficient(s) of interest takes a normal distribution in your (finite) sample. You may be particularly worried about this assumption (of parametric power calculations) if your sample is very small.
#' 
#' To do non-parametric power simulations we need to create a (reasonable) "fake" or simulated dataset. For example, if you have baseline data for the 3 months prior to a 12 month trial, then a reasonable way to expand this dataset would be to simply randomly draw days with replacement until you have 365 days in your dataset. Similarly, you could use historical data from the two years prior to the study to estimate the confidence interval around a null effect in the past year, with data on your outcome variable from two years ago serving as controls.
#' 
#' Let's do an example using `balsakhi` data to make this procedure more clear. The `balsakhi` dataset is provided with the package `pwrclc`.

#' 
#' ## Step 1. Upload pre-period data
#' 
#'For this example, we expand study data for the control group of the Balsakhi experiment by 2, since this is similar to what would be available from historical data:

#' 
data(balsakhi)
control_subset <- subset(balsakhi, bal == 0)
simulated = control_subset 
simulated$studentid <- (-1) * simulated$studentid
simulated <- rbind.data.frame(control_subset, simulated)

keep_cols <- c("studentid", "pre_tot", "mid_tot", "post_tot", "divid",
               "pre_totnorm", "mid_totnorm", "post_totnorm")
simulated <- simulated[, keep_cols, drop = FALSE]
simulated <- simulated[order(simulated$studentid), ]

nrow(simulated)
summary.data.frame(simulated)

write.csv(simulated, "bal_power_data_r.csv", row.names = FALSE, quote = FALSE)

#' ## Step 2. Write randomization code as you plan to randomize
#' ## &
#' ## Step 3. Write simulation code.
#' 
#' Your randomization code should take into account how you plan to randomize, including your plan to stratify, etc.
#' 
#' This power simulation program does the following:
#'    
#' (a) specifies main regression equation,
#'  
#' (b) simulates treatment assignment and runs main regressions 1000 times,
#' 
#' (c) summarizes the results from these regressions into an easy-to-read matrix.(left as an exercise)
#' 
#' To reduce the run-time, we have set this program to run over 1 iteration, but if you are truly calculating power via simulation, you should set this at 1000:
#' 
alpha <- 0.05                                    # Standard significance level
sims <- 1                                      # Number of simulations to conduct

#' 
#' Initialize a matrix to collect results. 
#' The matrix can be filed up by users according to their choice. and has been left as an exercise. 
#' For hint see the end of the document:
#' 
## colnames <- c("pvalue" , "tstat", "control_mean", "control_sd", "beta", "se", "ci_low", "ci_high")
## results <- matrix(nrow = sims, ncol = 8)
## colnames(results) <- colnames

#' 
#' ## Part 1. Basic MDE (clustering on divid because data is from C-RCT)
#' 
#' Loop to conduct experiments `sims` times over. Output is shown for `sims` = 1:
#' 

source("D:/R/J-PAL Power Tutorial/Clustered_SE.R") #make sure you change the path based on your system path
for (i in 1:sims){
  # We do a clustered random assignment using divid as the cluster
  simulated$treatment <- cluster_ra(clusters = simulated$divid)
  # View(simulated)
  
  # Do analysis (Simple regression)
  fit <- lm(post_totnorm ~ treatment, data = simulated, na.action = na.exclude)      
  # call for clustered SE  by divid
  output <- cluster_summary(fit, simulated$divid)
  print(output)
}

#' 
#' ## Part 2. Controlling for LDV
#' 
#' Loop to conduct experiments `sims` times over. Output is shown for `sims` = 1:
#' 

for (i in 1:sims) {
  # We do a clustered random assignment using divid as the cluster
  simulated$treatment <- cluster_ra(clusters = simulated$divid)
  
  # Do analysis (Simple regression)
  fit <- lm(post_totnorm ~ pre_totnorm + treatment, data = simulated, na.action
            = na.exclude)      
  # call for clsutered SE  by divid
  
  output <- cluster_summary(fit, simulated$divid)
  print(output)
}


#' 
#' Since the `balsakhi` dataset has many observations, we find that our simulated power estimates of MDE are very similar to those found through parametric estimates. 
#' To see this, compare Example 4 (parts 1 & 2) to Example 6 (parts 1 & 2). 
#' As you can see, it is much easier to cluster or control for covariates using this method; you simply run the regression specification that you intend to use at the analysis stage.
#' 
#' Similarly, if you wanted to estimate the (local) average treatment effect in the presence of partial take-up of the intervention, then you can simply make basic assumptions on the effective take-up rate, and run your two-stage least squares regression.
#' 
#' Generally speaking, parametric power calculations are great for back-of-the-envelope calculations of power, while non-parametric simulations provide more precise estimates of power when you have good baseline data.
#' 
#' 
#' ### Questions:
#' 
#' 1. How is "non-parametric" power different from "parametric" power?
#' 
#' 2. How do our parametric and non-parametric estimates of power compare? (Hint: Compare Example 4 to Example 6.)
#' 
#' 3. When would you want to run parametric power calculations?
#' 
#' 4. Non- paramteric simulations
#' 
#' 
#' #' Answers:
#' 
#' 1. Non-parametric power simulations do not require the assumption that beta coefficients take a normal distribution (which follows from the central limit theorem), while parametric power calculations do.
#' 
#' 2. Our power simulations in Example 6 find that we are powered to detect an ~.20 SD increase in test scores without controls and a ~.14 SD increase in test scores when including a lagged dependent variable. Using parametric methods, Example 4 illustrates these are .19 SD and .13 SD, respectively. Thus, we find very similar results using each method.
#' 
#' 3. We would want to run parametric power calculations when we want a quick, rough estimate of power, or when we do not have access to high quality baseline data.
#' 
#' 4. We would want to run non-parametric simulations when we have access to high-quality baseline data and have reason to believe parametric assumptions may be violated. For example, we would prefer to run simulations if our randomization is complex (i.e. multiple randomizations), have a small sample size, or are running complex specifications.
#' 
#' 
#' 
#' #' ### Hint for aggregating results of regression simluation
#' 
#' Example is shown to tabulate the results of `coeftest`. Others can be done similarly:
#' 
# Define a function that will extract coefficients during the simulation
extract_estimates <- function(output) {
  class(output) <- 'matrix'
  df_out <- data.frame(output)
  df_out$coefficient <- rownames(df_out)
  rownames(df_out) <- NULL
  return(df_out)
}

# simulation

# Standard significance level
alpha <- 0.05
# Number of simulations to conduct
sims <- 5

coefs <- data.frame()
for (i in 1:sims) {
  
  # We do a clustered random assignment using divid as the cluster
  simulated$treatment <- cluster_ra(clusters = simulated$divid)
  # View(simulated)
  
  # Do analysis (Simple regression)
  fit <- lm(post_totnorm ~ treatment, data = simulated, na.action = na.exclude)      
  # call for clsutered SE  by divid
  sim_output <- cluster_summary(fit, simulated$divid)
  sim_coef <- extract_estimates(sim_output[[1]])
  sim_coef$sim = i
  coefs <- rbind.data.frame(coefs, sim_coef)
}
coefs

#' 
#' Similar data frames for aggregation can be made for other objects returned by `super.cluster.fun`, 
#' namely `w`(wald test) and `ci`(confidence intervals). 
#' Instead of using a loop, the same functionality can be achieved through `apply` family of functions.

