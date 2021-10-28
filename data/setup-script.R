# Joshua Alley
# Script to load packages and manage conflicts in project for alliance treaty depth

# Run this at the start of every session


# load packages
library(MASS)
library(ggeffects)
library(effects)
library(haven)
library(tidyverse)
library(coefplot)
library(betareg)
library(brms)
library(bayesplot)
library(rstan)
library(sjstats)
library(arm)
library(bfa)
library(coda)
library(stargazer)
library(xtable)
library(caseMatch)
library(GJRM)
library(gridExtra)
library(conflicted)
library(democracyData) 
library(sn)
library(modelsummary)

# Look at conflicts 
conflict_scout()

# Set preferences
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("ngrps", "brms")
conflict_prefer("chol2inv", "Matrix")
conflict_prefer("coefplot", "coefplot") 
conflict_prefer("coefplot.default", "coefplot") 
conflict_prefer("expand", "tidyr") 
conflict_prefer("lag", "dplyr")
conflict_prefer("Position", "ggplot2")
conflict_prefer("rcond", "Matrix")
conflict_prefer("pack", "tidyr")
conflict_prefer("unpack", "tidyr")
conflict_prefer("display", "xtable")
conflict_prefer("traceplot", "coda")
conflict_prefer("collapse", "dplyr")
conflict_prefer("t2", "mgcv")
conflict_prefer("mm", "brms")
conflict_prefer("lmList", "nlme")
conflict_prefer("s", "mgcv") 
conflict_prefer("combine", "dplyr")
conflict_prefer("ar", "brms")
conflict_prefer("extract", "rstan")
conflict_prefer("sd", "sn")

# Set up RSTAN guidelines
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Set seed
set.seed(12)

# default ggplot theme
theme_set(theme_bw())


# Get beta regression predictions on outcome, not link scale
# Take these off the link function scale using linkinv from betareg package
linkinv <- function(eta) pmax(pmin(exp(-exp(-eta)), 1 - .Machine$double.eps), .Machine$double.eps)


