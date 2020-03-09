# Joshua Alley
# Analysis of multiple sources of alliance credibility

# data is loaded by the alliance-measures script


# Set up unique dataframe
key.data <- select(atop.milsup, atopid, latent.depth.mean, avg.democ.weight, econagg.dum, uncond.milsup, 
                   fp.conc.index, num.mem, wartime, asymm, deep.alliance, non.maj.only,
                   low.kap.sc, begyr, asymm.cap, mean.threat, max.democ.weight, avg.democ) %>%
            drop_na()
key.data$latent.depth.mean.rs <- (key.data$latent.depth.mean + 1) / (1 + max(key.data$latent.depth.mean) + .01)
summary(key.data$latent.depth.mean.rs)

# transform democracy variables so smooths are not dependent on limited support
key.data$ihs.avg.democ <- asinh(key.data$avg.democ.weight)
key.data$ihs.max.democ <- asinh(key.data$max.democ.weight)
summary(key.data$ihs.max.democ)

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
copulas[17] 
joint.gjrm <- gjrm.models[[17]] 
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




### fit a model with maximum polity score
# set up model formulas 
uncond.formula.max <- uncond.milsup ~ s(ihs.max.democ) + econagg.dum + 
  fp.conc.index + num.mem + wartime + asymm + asymm.cap +
  s(mean.threat) + low.kap.sc + s(begyr)


depth.formula.max <- latent.depth.mean.rs ~ s(ihs.max.democ) + econagg.dum +
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + 
  s(mean.threat) + low.kap.sc + s(begyr)


# Create a list of models
gjrm.models.max <- vector(mode = "list", length = length(copulas))

# Same model: probit and beta margins 
for(i in 1:length(copulas)){
  gjrm.models.max[[i]]  <- gjrm(list(uncond.formula.max, depth.formula.max,
                                     eq.sigma, theta.formula), data = key.data,
                                margins = c("probit", "BE"),
                                Model = "B",
                                BivD = copulas[i]
  )
}
aic.gjrm.max <- lapply(gjrm.models.max, AIC)
aic.gjrm.max

# examine the results: 
copulas[16] # PL copula minimizes AIC and has best convergence
joint.gjrm.max <- gjrm.models.max[[16]] 
conv.check(joint.gjrm.max)
AIC(joint.gjrm.max)
summary(joint.gjrm.max)


# plot all smoothed terms 
plot(joint.gjrm.max, eq = 1, seWithMean = TRUE,
     shade = TRUE, pages = 1) # smoothed terms 
plot(joint.gjrm.max, eq = 2, seWithMean = TRUE,
     shade = TRUE, pages = 1) # smoothed terms 
plot(joint.gjrm.max, eq = 4, seWithMean = TRUE,
     shade = TRUE, pages = 1) # smoothed terms 

# Democracy smooths
# uncond milsup
plot(joint.gjrm.max, eq = 1, seWithMean = TRUE,
     shade = TRUE, select = 1,
     xlab = "Maximum Democracy"
)

# Depth
plot(joint.gjrm.max, eq = 2, seWithMean = TRUE,
     shade = TRUE, select = 1,
     xlab = "Maximum Democracy"
)

# errors
plot(joint.gjrm.max, eq = 4, seWithMean = TRUE,
     shade = TRUE, select = 1,
     xlab = "Maximum Democracy"
)





# Trivariate model is not fitting well: no variation with different copulas
# and theta estimates are poor 
# can only get it to fit with Cholesky method for covariance matrix
depth.formula.tri <- deep.alliance ~ s(ihs.avg.democ) + 
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + 
  s(mean.threat) + low.kap.sc + s(begyr)

linkage.formula <- econagg.dum ~ s(ihs.avg.democ) +
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
