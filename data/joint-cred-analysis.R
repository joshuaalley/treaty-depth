# Joshua Alley
# Analysis of multiple sources of alliance credibility

# data is loaded by the alliance-measures script


# Set up unique dataframe
key.data <- select(atop.milsup, atopid, latent.depth.mean, econagg.dum, uncond.milsup, 
                   fp.conc.index, num.mem, wartime, asymm, deep.alliance, non.maj.only,
                   low.kap.sc, begyr, asymm.cap, mean.threat, 
                   dem.prop, joint.democ, avg.democ, max.democ, avg.democ.weight, max.democ.weight) %>%
            drop_na()
key.data$latent.depth.mean.rs <- (key.data$latent.depth.mean + 1) / (1 + max(key.data$latent.depth.mean) + .01)
summary(key.data$latent.depth.mean.rs)


# glm model of unconditional military support
uncond.glm <- glm(uncond.milsup ~ avg.democ +
                       fp.conc.index + num.mem + wartime + asymm +
                       asymm.cap + non.maj.only + mean.threat + 
                       low.kap.sc + begyr,
                     family = binomial(link = "probit"),
                     data = key.data)
summary(uncond.glm)


# glm model of economic issue linkages
linkage.glm <- glm(econagg.dum ~ avg.democ +
                    fp.conc.index + num.mem + wartime + asymm +  
                     asymm.cap + non.maj.only + mean.threat +
                    low.kap.sc + begyr,
                  family = binomial(link = "probit"),
                  data = key.data)
summary(linkage.glm)


# Use a beta regression with rescaled depth 
beta.reg.depth <- betareg(latent.depth.mean.rs ~ avg.democ +
                            fp.conc.index + num.mem + wartime + asymm + 
                            asymm.cap + non.maj.only + 
                            mean.threat + low.kap.sc + begyr, data = key.data)
summary(beta.reg.depth)



### Joint analysis of unconditional military support and depth
# use GJRM, as it allows for correlated errors

### Fit a model with average democracy scores

uncond.formula <- uncond.milsup ~ s(avg.democ) + econagg.dum + 
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only + 
  s(mean.threat) + low.kap.sc + s(begyr)

depth.formula <- latent.depth.mean.rs ~ s(avg.democ) + econagg.dum +
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only + 
  s(mean.threat) + low.kap.sc + s(begyr)

# model the dependence between the error terms as a function of start year
theta.formula <- ~ s(begyr)
eq.sigma <- ~ 1



copulas <- c("N", "C0", "C90", "C180", "C270", "J0", "J90", "J180", "J270",
             "G0", "G90", "G180", "G270", "F", "AMH", "FGM", "T", "PL", "HO")

# Create a list of models
gjrm.models <- vector(mode = "list", length = length(copulas))

# FISK (log-logistic), inverse gaussian, Dagum and beta distributions are best
# in terms of residual fit
# Beta has the lowest AIC.
for(i in 1:length(copulas)){
  gjrm.models[[i]]  <- gjrm(list(uncond.formula, depth.formula,
                                 eq.sigma, theta.formula), 
                            data = key.data,
                            margins = c("probit", "BE"),
                            Model = "B",
                            BivD = copulas[i]
  )
}
aic.gjrm <- lapply(gjrm.models, AIC)
aic.gjrm

# NB for interpretation: smoothed terms
copulas[18] 
joint.gjrm <- gjrm.models[[18]] 
conv.check(joint.gjrm)

AIC(joint.gjrm)
summary(joint.gjrm)
post.check(joint.gjrm)

# Plot smooths
plot(joint.gjrm, eq = 1, seWithMean = TRUE,
     shade = TRUE, pages = 1) # smoothed terms 
plot(joint.gjrm, eq = 2, seWithMean = TRUE,
     shade = TRUE, pages = 1) # smoothed terms 
plot(joint.gjrm, eq = 4, seWithMean = TRUE,
     shade = TRUE, pages = 1) # smoothed terms 

# Democracy smooths
# uncond milsup
plot(joint.gjrm, eq = 1, seWithMean = TRUE,
     shade = TRUE, select = 1,
     xlab = "Average Democracy"
)

# Depth
plot(joint.gjrm, eq = 2, seWithMean = TRUE,
     shade = TRUE, select = 1,
     xlab = "Average Democracy"
)




### fit a model with democratic proportion


# glm model of unconditional military support
uncond.glm.prop <- glm(uncond.milsup ~ dem.prop +
                    fp.conc.index + num.mem + wartime + asymm +
                    asymm.cap + non.maj.only + mean.threat + 
                    low.kap.sc + begyr,
                  family = binomial(link = "probit"),
                  data = key.data)
summary(uncond.glm.prop)


# glm model of economic issue linkages
linkage.glm.prop <- glm(econagg.dum ~ dem.prop +
                     fp.conc.index + num.mem + wartime + asymm +  
                     asymm.cap + non.maj.only + mean.threat +
                     low.kap.sc + begyr,
                   family = binomial(link = "probit"),
                   data = key.data)
summary(linkage.glm.prop)


# Use a beta regression with rescaled depth 
beta.reg.depth.prop <- betareg(latent.depth.mean.rs ~ dem.prop +
                            fp.conc.index + num.mem + wartime + asymm + 
                            asymm.cap + non.maj.only + 
                            mean.threat + low.kap.sc + begyr, data = key.data)
summary(beta.reg.depth.prop)





# set up model formulas 
uncond.formula.prop <- uncond.milsup ~ s(dem.prop) + econagg.dum + 
  fp.conc.index + num.mem + wartime + asymm + asymm.cap +
  s(mean.threat) + low.kap.sc + s(begyr)


depth.formula.prop <- latent.depth.mean.rs ~ s(dem.prop) + econagg.dum +
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + 
  s(mean.threat) + low.kap.sc + s(begyr)


# Create a list of models
gjrm.models.prop <- vector(mode = "list", length = length(copulas))

# Same model: probit and beta margins 
for(i in 1:length(copulas)){
  gjrm.models.prop[[i]]  <- gjrm(list(uncond.formula.prop, depth.formula.prop,
                                     eq.sigma, theta.formula), data = key.data,
                                margins = c("probit", "BE"),
                                Model = "B",
                                BivD = copulas[i]
  )
}
aic.gjrm.prop <- lapply(gjrm.models.prop, AIC)
aic.gjrm.prop

# examine the results: 
copulas[1] # copula that minimizes AIC and has best convergence
joint.gjrm.prop <- gjrm.models.prop[[1]] 
conv.check(joint.gjrm.prop)
AIC(joint.gjrm.prop)
summary(joint.gjrm.prop)


# plot all smoothed terms 
plot(joint.gjrm.prop, eq = 1, seWithMean = TRUE,
     shade = TRUE, pages = 1) # smoothed terms 
plot(joint.gjrm.prop, eq = 2, seWithMean = TRUE,
     shade = TRUE, pages = 1) # smoothed terms 
plot(joint.gjrm.prop, eq = 4, seWithMean = TRUE,
     shade = TRUE, pages = 1) # smoothed terms 


# Democracy smooths
# uncond milsup
plot(joint.gjrm.prop, eq = 1, seWithMean = TRUE,
     shade = TRUE, select = 1,
     xlab = "Proportion Democracies"
)
abline(h = 0)

# Depth
plot(joint.gjrm.prop, eq = 2, seWithMean = TRUE,
     shade = TRUE, select = 1,
     xlab = "Proportion Democracies"
)
abline(h = 0)



# Trivariate model is not fitting well: no variation with different copulas
# and theta estimates are poor 
# can only get it to fit with Cholesky method for covariance matrix
depth.formula.tri <- deep.alliance ~ s(avg.democ) + 
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + 
  s(mean.threat) + low.kap.sc + s(begyr)

linkage.formula <- econagg.dum ~ s(avg.democ) +
  fp.conc.index + num.mem + wartime + asymm +  asymm.cap +
  s(mean.threat) + low.kap.sc + s(begyr)

# Create a list of models
gjrm.models.tri <- vector(mode = "list", length = length(copulas))

for(i in 1:length(copulas)){
  gjrm.models.tri[[i]]  <- gjrm(list(uncond.formula, depth.formula.tri, linkage.formula), data = key.data,
                                margins = c("probit", "probit", "probit"),
                                Model = "T", Chol = TRUE, penCor = "lasso",
                                BivD = copulas[i]
  )
}
lapply(gjrm.models.tri, AIC)
lapply(gjrm.models.tri, conv.check)

# No difference in AIC or convergence across these models
copulas[1]
joint.gjrm.tri <- gjrm.models.tri[[1]] 
conv.check(joint.gjrm)
AIC(joint.gjrm.tri)
summary(joint.gjrm.tri)



# Plot smooths
plot(joint.gjrm.tri, eq = 1, seWithMean = TRUE,
     shade = TRUE, pages = 1) # smoothed terms 
plot(joint.gjrm.tri, eq = 2, seWithMean = TRUE,
     shade = TRUE, pages = 1) # smoothed terms 
plot(joint.gjrm.tri, eq = 3, seWithMean = TRUE,
     shade = TRUE, pages = 1) # smoothed terms 

# Democracy smooths
# uncond milsup
plot(joint.gjrm.tri, eq = 1, seWithMean = TRUE,
     shade = TRUE, select = 1,
     xlab = "Average Democracy"
)

# Depth
plot(joint.gjrm.tri, eq = 2, seWithMean = TRUE,
     shade = TRUE, select = 1,
     xlab = "Average Democracy"
)

# Issue linkages
plot(joint.gjrm.tri, eq = 3, seWithMean = TRUE,
     shade = TRUE, select = 1,
     xlab = "Average Democracy"
)


# switch off gjrm plots 
dev.off()
