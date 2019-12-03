# Joshua Alley
# Texas A&M University
# Script to load packages and manage conflicts in project for alliance treaty depth

# Run this at the start of every session


# load packages
library(MASS)
library(tidyverse)
library(coefplot)
library(brms)
library(sjstats)
library(mediation)
library(conflicted)


# Look at conflicts 
conflict_scout()

# Set preferences
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("rename", "dplyr")


# Set up RSTAN guidelines
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# Set seed
set.seed(12)