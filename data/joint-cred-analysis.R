# Joshua Alley
# Analysis of multiple sources of alliance credibility

# data is loaded by the alliance-measures script

# glm model of unconditional military support
uncond.glm <- glm(uncond.milsup ~ avg.democ + econagg.dum +
                       fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only +
                       low.kap.sc + begyr + us.mem + ussr.mem,
                     family = binomial(link = "probit"),
                     data = atop.milsup)
summary(uncond.glm)


# glm model of economic issue linkages
linkage.glm <- glm(econagg.dum ~ avg.democ + uncond.milsup + latent.depth.mean +
                    fp.conc.index + num.mem + wartime + asymm +  asymm.cap + non.maj.only +
                    low.kap.sc + begyr + us.mem + ussr.mem,
                  family = binomial(link = "probit"),
                  data = atop.milsup)
summary(linkage.glm)



# Joint analysis of multiple sources of credibility

# May need to use a trivariate model from GJRM w/ issue linkages

# Set up unique dataframe
key.data <- select(atop.milsup, latent.depth.mean, deep.alliance, avg.democ, econagg.dum, uncond.milsup, 
                  fp.conc.index, num.mem, wartime, asymm,
                   low.kap.sc, begyr, asymm.cap, non.maj.only, milinst)
key.data[2:(ncol(key.data) - 1)] <- lapply(key.data[2:(ncol(key.data) - 1)], 
                                       function(x) rescale(x, binary.inputs = "0/1")) 

key.data$latent.depth.mean.rs <- (key.data$latent.depth.mean + 1) / (1 + max(key.data$latent.depth.mean) + .01)
summary(key.data$latent.depth.mean.rs)




### use GJRM instead: allows for correlated errors
# Bivariate model of unconditional military support and depth
uncond.formula <- uncond.milsup ~ avg.democ + econagg.dum + latent.depth.mean +
                 fp.conc.index + num.mem + wartime + asymm + asymm.cap + 
                  low.kap.sc + begyr

depth.formula <- latent.depth.mean.rs ~ avg.democ + econagg.dum + uncond.milsup +
                   fp.conc.index + num.mem + wartime + asymm + asymm.cap + 
                   low.kap.sc + begyr


copulas <- c("N", "C0", "C90", "C180", "C270", "J0", "J90", "J180", "J270",
               "G0", "G90", "G180", "G270", "F", "AMH", "FGM", "T", "PL", "HO")

# Create a list of models
gjrm.models <- vector(mode = "list", length = length(copulas))

# FISK (log-logistic), inverse gaussian, Dagum and beta distributions are best
# in terms of residual fit
# Beta has the lowest AIC. 
for(i in 1:length(copulas)){
gjrm.models[[i]]  <- gjrm(list(uncond.formula, depth.formula), data = key.data,
                   margins = c("probit", "BE"),
                   Model = "B",
                   BivD = copulas[i]
                   )
}
aic.gjrm <- lapply(gjrm.models, AIC)
aic.gjrm
lapply(gjrm.models, conv.check)

# NB for interpretation: rescaled continuous covariates (helps with substance)
# start with a normal copula: converged and lowest AIC
# this implies symmetric dependence in the errors.
copulas[1]
joint.gjrm <- gjrm.models[[1]] 
conv.check(joint.gjrm)
AIC(joint.gjrm)
summary(joint.gjrm)
post.check(joint.gjrm)

# AIC with T copula is also close
# start with a normal copula: converged and lowest AIC
copulas[17]
joint.gjrm2 <- gjrm.models[[17]] 
conv.check(joint.gjrm2)
AIC(joint.gjrm2)
summary(joint.gjrm2)

# The two models give very similar inferences about covariates in each model and
# the presence of correlated errors


## fit a model with the deep alliance dummy variable outcome 
### use GJRM instead: allows for correlated errors
# Bivariate model of unconditional military support and depth
depth.formula.dum <- deep.alliance ~ avg.democ + econagg.dum + uncond.milsup +
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + 
  low.kap.sc + begyr

# Create a list of models
gjrm.models.dum <- vector(mode = "list", length = length(copulas))

# FISK (log-logistic), inverse gaussian, Dagum and beta distributions are best
# in terms of residual fit
# Beta has the lowest AIC. 
for(i in 1:length(copulas)){
  gjrm.models.dum[[i]]  <- gjrm(list(uncond.formula, depth.formula.dum), data = key.data,
                            margins = c("probit", "probit"),
                            Model = "B",
                            BivD = copulas[i]
  )
}
aic.gjrm.dum <- lapply(gjrm.models.dum, AIC)
aic.gjrm.dum
lapply(gjrm.models.dum, conv.check)

# examine the results: 
copulas[1] # normal copula again minimizes AIC
joint.gjrm.dum <- gjrm.models.dum[[1]] 
conv.check(joint.gjrm.dum)
AIC(joint.gjrm.dum)
summary(joint.gjrm.dum)



# Trivariate model is not fitting well: no variation with different copulas
# and theta estimates are poor 
# can only get it to fit with Cholesky method for covariance matrix
depth.formula.tri <- deep.alliance ~ avg.democ + econagg.dum + uncond.milsup +
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only +
  low.kap.sc + begyr

linkage.formula <- econagg.dum ~ avg.democ + latent.depth.mean +
  fp.conc.index + num.mem + wartime + asymm +  asymm.cap + non.maj.only +
  low.kap.sc + begyr

# Create a list of models
gjrm.models.tri <- vector(mode = "list", length = length(copulas))

for(i in 1:length(copulas)){
  gjrm.models.tri[[i]]  <- gjrm(list(uncond.formula, depth.formula.tri, linkage.formula), data = key.data,
                            margins = c("logit", "logit", "logit"),
                            Model = "T", Chol = TRUE, penCor = "lasso",
                            BivD = copulas[i]
  )
}
lapply(gjrm.models.tri, AIC)
lapply(gjrm.models.tri, conv.check)

# NB for interpretation: rescaled continuous covariates (helps with substance)
# No difference in AIC or convergence across these models
copulas[1]
joint.gjrm.tri <- gjrm.models.tri[[1]] 
conv.check(joint.gjrm)
AIC(joint.gjrm.tri)
summary(joint.gjrm.tri)

