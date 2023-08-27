# ---
# title: "Power Calculations - R lab"
# ---
# 

# 
# # Introduction
# 
# ## Why are power calculations important?
# 
# Power influences many design aspects, including what research questions to pursue, 
# how many treatment arms to employ, and even more fundamentally, whether or not to proceed with a potential research project. 
# For example, it may be that a remedial education program boosts tests scores by 20 percent 
# when comparing treatment and control groups, but due to limited power, 
# the RCT is unable to detect this true effect (with 95% confidence). 
# However, we can estimate whether a given design is likely to be able to detect a reasonable 
# effect size ex ante, allowing us to properly manage partner expectations and make the most of limited research resources.
#  
# This exercise will cover the conventional parametric method of calculating power for a RCT
# 

### Questions to consider before running power calculations

# 1. What is the main specification (e.g. regression) we plan to run? 
#    (It doesn't have to be fully baked, but the more "baked" it is, the more precise we can make your power estimates.)
# 
# 2. What do we expect to be the mean of the outcome in the control group?
# 
# 3. How about the standard deviation (SD) of the outcome in control group? 
# 
# 4. What sample sizes are feasible?
# 
# 5. What effect sizes could the intervention reasonably cause?
# 
# 6. What is the smallest, cost-effective effect size that we are interested in? 
#    (We often arrive at a reasonable answer to this question through discussions with 
#     partner organizations and literature reviews.)
# 


#### Setup #####

# Please install these packages if you don't have them:
# 
## install.packages(c("haven", "ICC", "randomizr", "multiwayvcov", "lmtest", "knitr","devtools", "Hmisc"), dependencies=TRUE, INSTALL_opts = c('--no-lock'))
## library(devtools)
## devtools::install_github('vikjam/pwrcalc')
## If you have trouble installing pwrcalc, you can download it manually by using the .tar.gz file provided. 
## To install it manually, go to "Packages", select "Install" and then "Package Archive" in the install menu. 

# 
# Once you installed the packages, load them to get started:
# 
library(haven)
library(ICC)
library(randomizr)
library(multiwayvcov)
library(lmtest)
library(knitr)
library(pwrcalc)
library(Hmisc)


# Let's start with a simple parametric example.
# 
# # Example 1. Basic Parametric Example
# 
# Before we get started make sure you've set your directory properly. 
# You can check your directory with `getwd` and if you need to change it you can set it with `setwd`:
# 

getwd()
setwd("C:\\Users\\sabhyag\\Documents\\Training_local\\R lab\\Power_R")  #change directory

# 
# The `balsakhi` dataset is provided with the package `pwrclc`. We'll use this dataset to estimate the control mean:
# 
data(balsakhi)

# 
# Let us view the `balsakhi `data that we have just loaded:


# View(balsakhi)

########################## 1. Basic Parametric Example ############################


# For all parametric power calculations, we'll assume a conventional 95% confidence interval and 80% power.

power = 0.8
alpha = 0.05
#
# What do we expect the mean and standard deviation of the outcome to be in the baseline group?

# Note: Since power calculations are usually done prior to a study, we often use baseline/pilot data on the study population, 
# or government statistics for a comparable population, to get an approximate for this outcome in the control group.
# 
# The mean at baseline can a good proxy for the control mean if we assume that the outcome for the control group is not different 
# at endline

baseline_mean <- mean(balsakhi$pre_totnorm, na.rm = TRUE)
baseline_sd <- sd(balsakhi$pre_totnorm, na.rm = TRUE)

# random variable takes a binomial distribution.)
# 
# Before proceeding lets check the values of baseline_mean and baseline_Sd:
# 
baseline_sd
baseline_mean

#### Calculate sample size given the minimum effect size #####

# One common use of the power calculations is to calculate the minimum required sample 
# size for a given effect size, baseline conditions and research design.


# Let's say, based on other studies, that we expect an effect size of a third of a standard deviation. 
# Now let's calculate the sample size given that we know the likely effect size:
# 
expected_effect <- baseline_sd / 3
treated_mean <- expected_effect + baseline_mean

# 
# Let us check the variables that we have:

treated_mean
baseline_mean
expected_effect
baseline_sd

# 
# We can take a look at the package `pwrclc` [documentation](http://pwrcalc.readthedocs.io/en/latest/?badge=latest) 
# now to understand the function `twomeans`. 
# The function calculates the sample sizes for two-sample test based on means and standard deviations of the two samples.

# The minimum required sample size will also depend on the split between the treatment and the control group. 
# This is specified as the nratio = treatment size/control size.

# This value is 1 if there are equal number of people in both groups and it increases as we allocate a higher proportion 
# to the treatment group. For instance, if the control group size is 1/2 of the treatment group, the nratio is 2.


nratio = 1

# Suppose we want to detect a difference of 4 between two groups (e.g., control and treatment). 
# For example, we anticipate the control group mean being 12 and the treatment group mean being 16. 
# In addition, suppose the standard deviation of each group is 5. We can calculate the sample size required with `pwrcalc`:

twomeans(m1 = 12, m2 = 16, sd = 5, nratio=1 )

# 
# Now, we can calculate the sample sizes for the `balsakhi` dataset:

base_model = twomeans(m1 = baseline_mean, m2 = treated_mean, sd = baseline_sd, 
                      nratio=nratio, power=power, sig.level = alpha)

base_model

# The command outputs the the sample size given the parameters.
# n1 and n2 are the control and treatment sizes respectively  
# "m1" and "m2" are the control and treatment means respectively

cat("We need a minimum treatment size of",base_model$n2,"and control size of", 
    base_model$n1, "to detect an effect of", 
    expected_effect, "with a probability of", 
    power,  "if the effect is true and the ratio of the treatment and control is",nratio)


#### Minimum effect size given the sample size ####

# Say, instead, we knew the sample size and wanted to calculate the Minimum Detectable Effect Size (MDE). 
# You can manually calculate the effect size:
# 
sample_n <- 1000                                                                # specify the total sample size
nratio = 1
p = nratio/(1+nratio)                                                           # The prop of the sample in the treatment group
t_power = qt(power, df=2*(sample_n-1))
t_alpha = qt(1-alpha/2, df=2*(sample_n-1))

                           
mde <- (t_power + t_alpha) * sqrt(1 /(p*(1-p))) * sqrt(1 / sample_n) * baseline_sd
mde = round(mde, digits=2)


cat("Given our sample size of",sample_n,
    "and ratio of treatment and control group as,",
    nratio, ",the effect needs to be higher than", 
    mde, "for us to detect it with a probability of",power)



### Some other questions to answer before calculating power:
#  
# 1. How do the sample size and MDE change when the different components of the power command change ?
# 2. Will our main specification include controls? 
# 3. Will this study be cluster-randomized?
# 4. Do we expect only part of the treatment group to take-up the intervention? 
# 

# We'll address each of these questions one at a time to see how they affect power.
# 

################ 2. Relationship between power and its components ##################

# 
# Now, let us get a better intuition on how a larger or smaller sample size
# affects our power to pick up an effect.
# 
# Say our anticipated effect size is smaller than originally thought; 
# how much larger would we need to make the sample in order to still pick up an effect?
#  
# Let's try an effect size that is half as large:

smaller_expected_effect <- expected_effect / 2
smaller_treated_mean <- smaller_expected_effect + baseline_mean
twomeans(m1 = baseline_mean, m2 = smaller_treated_mean, sd = baseline_sd, nratio=nratio, power=power, sig.level = alpha)

# 
# *Observation: The minimum sample required is four times as large.*
# 
# Let's try an effect size that is a third of the original:

smaller_expected_effect <- expected_effect / 3
smaller_treated_mean <- smaller_expected_effect + baseline_mean
twomeans(m1 = baseline_mean, m2 = smaller_treated_mean, sd = baseline_sd, nratio=nratio, power=power, sig.level = alpha)

# 
# *Observation: The minimum sample required is now nine times as large.*
# 
# **Remember: If our MDE decreases by a factor of X, the required sample size increases by the square of X!**
# 
# You can verify this from the other side i.e. look at the impact on the MDE of increasing your sample size by a factor of X. 
# Say X is 4:
# 

new_sample <- 4 * (sample_n)
new_mde <- (t_power + t_alpha) * sqrt(1 /(p*(1-p))) * sqrt(1 / new_sample) * baseline_sd
new_mde / mde

## The minimum detectable effect size decreases by half. 
## Similarly, what happens when the standard deviation increases? How does the sample size changes? How does the MDE change?
## Check by manually changing the values



################ 3. Parametric Power calculations with controls ##################

# Now, say we plan to control for baseline covariates in our main specification.  
# The inclusion of these controls will improve our power, since they explain
# some of the variance in our outcome. 
# 
# For example, including data on baseline characteristics like sex or age may explain some of the variance in the
# outcome. Note that we may not need/want to include covariates if the treatment and the control are randomly allocated.
# 
# To see how potential controls affect power, we would ideally have access to a sample data set (e.g. historical or pilot data).  
# With these data, we would want to regress the outcome on the covariates to evaluate how much 
# variance is explained by the set of covariates we plan to include.
# 
# From this regression, we are interested in the residual standard deviation of the outcome variables, 
# or the variance of the outcome that is NOT explained by controls. 
# This residual SD becomes the new SD we include in our parametric power calculations.


# Using `balsakhi` data, this would be:
# 

# We are using the math and verbal scores of the students at baseline as covariates

fit <- lm(pre_totnorm ~ pre_math+ pre_verb, data = balsakhi, subset = bal == 0)
summary(fit)

res_baseline_sd <- sd(summary(fit)$residuals, na.rm=TRUE)
res_baseline_sd 

# 
# If we knew the effect size and wanted to know the sample size needed:
# 
cov_model <- twomeans(m1 = baseline_mean, m2 = treated_mean, sd = res_baseline_sd, nratio=nratio, 
                      power=power, sig.level = alpha)

cat("We need a minimum treatment size of",cov_model$n2,
    "and control size of", cov_model$n1, "to detect an effect of", 
    expected_effect, "with a probability of", 
    power,  "if the effect is true and the ratio of the treatment and control is",nratio)


# Questions:
#   
# 3.1. What percent of the variance of study period test scores is explained by 
#       the covariates, pre_verb and pre_math? (Hint: Look at the R^2 statistic of the regression.)
# 3.2. How does this affect our sample size (compared to not including controls)?
# 3.3. How about our MDE?



############# 4. Parametric power calculation with partial take-up #############


# In randomized designs, it is common that there is partial take-up of the intervention.  For example, in 
# the Oregon Health Insurance Experiment, the offer to apply for health insurance was associated with only a 25 
# percentage point increase in take-up of health insurance. When take-up is not 100 percent,
# researchers are often interested in the answers to second stage questions, such as what the average effect of 
# becoming insured is on health care utilization.
# 
# Below, we provide code that adjusts command to take into account that only some individuals in the 
# treatment group take up the intervention.
# 
# The measure of take-up that we care about is "effective take-up", or the percentage of individuals 
# in the treatment group that takes up the intervention MINUS the percentage of individuals in the control group that takes up.


# Let us say 90% take up in the treatment group, and 10% do so in the control group. We then have an effective take-up rate of 80%:

takeup_treat = 0.9
takeup_control =  0.1 

tu = takeup_treat - takeup_control													                    #effective take-up
effect_tu = expected_effect*tu											                            #effect size after adjusting for take-up
treat_tu = baseline_mean + effect_tu                                            #treatment mean after adjusting for take-up

partial_model <- twomeans(m1 = baseline_mean, m2 = treat_tu, nratio=nratio, sd = baseline_sd, 
                          power=, sig.level = alpha)

# Here we assume that the standard deviation does not change with the treatment but 
# you can also specify different standard deviations for the control and treatment groups


# We need a higher sample size to have the same power because the expected effect has decreased by the take-up rate 


############### 6. Parametric power calculation for cluster RCTs  ##############

# Many designs randomize at the group level instead of at the individual level. For such designs, 
# we need to adjust our power calculations so that they
# incorporate the fact that individuals within the same group may be subject to similar shocks, 
# and thereby have correlated outcomes. Duflo et al.  "Using
# Randomization in Development Economics Research: A Toolkit." presents a modified parametric approach, 
# which takes into account the intra-cluster
# correlation (ICC) that arises from randomization at the group level.
# 
# We can think of cluster RCTs as follows:
#   
# - When ICC = 0, then our N is effectively the number of individuals in the study.
# 
# - When ICC = 1, then our N is effectively just the number of clusters.
# 
# - Usually the ICC lies somewhere between 0 and 1, requiring that we adjust our power calculations to account for this.
# 
# Below we adjust power estimates based on Duflo et al.'s model.
# 
# Note: This model assumes that all clusters in a treatment arm are of the same size and have the same number of individuals.  
# It's usually okay if this is violated in reality, but you would not want to use these adjustments
# if groups are dramatically different in size (e.g. group one has 10 individuals,
# group two has 1,000 individuals.) More on this model is explained in 
# Duflo et al.'s article  "Using Randomization in Development Economics Research: A Toolkit."
# 

# First, let's calculate the intra-cluster correlation (ICC) which measures how correlated the error terms of 
# individuals in the same cluster are:
# 
# Here the ICC is calculated from the baseline dataset but it can also be manually defined based on historical data, 
# other studies etc

baseline_subset <- subset(balsakhi, !is.na(pre_totnorm))                        # remove the NA values 

cluster_var_subset <- as.factor(baseline_subset$divid)                          # "divid" is the cluster variable                    
outcome_subset <- baseline_subset$pre_totnorm                                    
icc <- ICCest(cluster_var_subset, outcome_subset, data = baseline_subset)
rho <- icc$ICC                                                                

rho

#### Part 1: Calculating MDE ####

# Now, let's specify the number of individuals in each cluster:
cluster_size <- 53

# and the number of clusters (as documented in the Balsakhi experiment):
total_clusters <- 193

# Let us assume 95% confidence intervals and 80% power:
t_stat <- t_alpha + t_power 

# and 50% of the study population is assigned to treatment and 50% to control:
p <- .5

# Now, we have Duflo et al.'s power adjustment:
mde <- t_stat * sqrt(1 / (p * (1 - p) * total_clusters)) * sqrt(rho + (1 - rho) / cluster_size) * baseline_sd

# And lastly, the total sample size of our study and the number who are treated, respectively:
n <- total_clusters * cluster_size
treated <- n * p


#### Part 2: The number of clusters given cluster size and effect size ####


# For R, please refer to the `pwrclc` [documentation: http://pwrcalc.readthedocs.io/en/latest/?badge=latest) for the function 
# `clustered` that adjust for the number of individuals per cluster:
# 
cluster_number <- twomeans(m1 = baseline_mean, m2 = treated_mean, sd = baseline_sd, nratio=nratio, 
                           sig.level = alpha, power=power)%>%
  clustered(obsclus = cluster_size, rho = rho)

cat("Given the size of each cluster as", 
    cluster_size,"and ratio of the number of units in the treatment to control as", nratio,
    "we need a minimum of", cluster_number$`Minimum number of clusters`, 
    "clusters to detect an effect of", 
    expected_effect,"with a probability of", power, "if the effect is true")


# Adjusted n1 and n2 indicate the sample size in the control and the treatment group respectively. 
# sample size is the total number of units across the clusters


#### Part 3. Cluster size given the number of clusters and effect size ####

cluster_size_model <- twomeans(m1 = baseline_mean, m2 = treated_mean, sd = baseline_sd, 
                               power=power, nratio=nratio, sig.level= alpha)%>%
  clustered(numclus = total_clusters, rho = rho)


cat("Given", total_clusters,"clusters, and the ratio of units in the treatment and the control as", nratio,
    "the minimum size of each cluster
    should be", cluster_size_model$`Average per cluster`, "for us to detect an effect
    of",expected_effect, "with a probability of", power, "if the effect is true")



# Questions:
  
#  6.1. Why do we have to adjust power for clustering when running a cluster RCT?
  
#  6.2. Assuming ICC>0, does adding a new cluster of 5 individuals or adding 5 individuals to already-existing clusters give us more power to detect effects?
  


# Note that the above calculations assume a distribution of the average effect size. 
# Non-parametric power simulations do better than parametric power calculations when we have access to good data (historical, baseline, or pilot) on
# our study population. From these data, we can simulate a fake dataset that assumes the treatment has no effect and then see what effects we are
# powered to detect, by looking at the simulated 95% confidence interval around our null effect. We should expect that any effect greater than this
# confidence interval would be detected by our study.
# 
# In particular, power simulations do not require the assumption that the sampling distribution of your Beta coefficient(s) of interest takes a normal
# distribution in your (finite) sample. You may be particularly worried about this assumption (of parametric power calculations) if your sample is
# very small.
# 
# To do non-parametric power simulations we need to create a (reasonable) "fake" or simulated dataset. For example, if you have baseline data for the
# 3 months prior to a 12 month trial, then a reasonable way to expand this dataset would be to simply randomly draw days with replacement until you
# have 365 days in your dataset. Similarly, you could use historical data from the two years prior to the study to estimate the confidence interval
# around a null effect in the past year, with data on your outcome variable from two years ago serving as controls.


  
####### Answers ######

# 3.1. About 86 percent of the variation in pre_totnorm is explained by pre_verb and pre_math

# 3.2. Including controls reduces sample size by about 85 percent.

# 3.3. Reduces MDE to a lesser extent (~63%).


# 6.1. Assignment is only random at the cluster level; thus we must cluster our standard errors in our main specification. 
# To this end, our power calculations must also take this into account, since, by clustering in our main specification, 
# we will lose all precision gained from intra-cluster correlation in outcomes.

# 6.2. Adding 5 individuals to existing clusters will increase power by a lesser amount. 
# In the limiting case of ICC=1, adding 5 individuals to previous
# clusters would have no effect on power, while adding 5 individuals in a new cluster would increase our effective N by 1.


