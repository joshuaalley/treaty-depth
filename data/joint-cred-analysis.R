# Joshua Alley
# Analysis of multiple sources of alliance credibility

# data is loaded by the alliance-measures script


# Set up unique dataframe
key.data <- select(atop.milsup, atopid, latent.depth.mean, econagg.dum, uncond.milsup, 
                   fp.conc.index, num.mem, wartime, asymm, deep.alliance, non.maj.only,
                   low.kap.sc, begyr, post45, asymm.cap, mean.threat, maxcap.democ, 
                   dem.prop, joint.democ, avg.democ, maxcap.democ, maxcap.open,
                   maxcap.rec, maxcap.comp, maxcap.cons, maxcap.lied, us.mem, bilat) %>%
            drop_na()
key.data$latent.depth.mean.rs <- (key.data$latent.depth.mean + 1) / (1 + max(key.data$latent.depth.mean) + .01)
summary(key.data$latent.depth.mean.rs)
table(key.data$maxcap.lied)

# glm model of unconditional military support
# full democ
uncond.glm.dem <- glm(uncond.milsup ~ 
                    maxcap.democ + econagg.dum + 
                    fp.conc.index + num.mem + wartime + asymm +
                    asymm.cap + non.maj.only + mean.threat + 
                    low.kap.sc + post45,
                  family = binomial(link = "probit"),
                  data = key.data)
summary(uncond.glm.dem)

# glm model of unconditional military support
uncond.glm <- glm(uncond.milsup ~ 
                       maxcap.cons + maxcap.lied + 
                       econagg.dum + 
                       fp.conc.index + num.mem + wartime + asymm +
                       asymm.cap + non.maj.only + mean.threat + 
                       low.kap.sc + post45,
                     family = binomial(link = "probit"),
                     data = key.data)
summary(uncond.glm)


# glm model of economic issue linkages
linkage.glm <- glm(econagg.dum ~ 
                     maxcap.cons + maxcap.lied +
                    fp.conc.index + num.mem + wartime + asymm +  
                     asymm.cap + non.maj.only + mean.threat +
                    low.kap.sc + post45,
                  family = binomial(link = "probit"),
                  data = key.data)
summary(linkage.glm)


# Use a beta regression with rescaled depth 
beta.reg.depth <- betareg(latent.depth.mean.rs ~ 
                            maxcap.cons + maxcap.lied +
                            econagg.dum + 
                            fp.conc.index + num.mem + wartime + asymm + 
                            asymm.cap + non.maj.only + 
                            mean.threat + low.kap.sc + post45, data = key.data)
summary(beta.reg.depth)


### Joint analysis of unconditional military support and depth
# use GJRM, as it allows for correlated errors

### Fit a model with democratic instituions of most capable member

uncond.formula <- uncond.milsup ~ maxcap.cons + maxcap.lied + 
  econagg.dum + 
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only + 
  s(mean.threat) + low.kap.sc + s(begyr)

depth.formula <- latent.depth.mean.rs ~ maxcap.cons + maxcap.lied + 
  econagg.dum +
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only + 
  s(mean.threat) + low.kap.sc + s(begyr)

# model the dependence between the error terms as a function of start year
theta.formula <- ~ s(begyr) + maxcap.cons + maxcap.lied + num.mem
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
copulas[1] 
joint.gjrm <- gjrm(list(uncond.formula, depth.formula,
                        eq.sigma, theta.formula), 
                   data = key.data,
                   margins = c("probit", "BE"),
                   Model = "B",
                   BivD = "T"
)
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





### Model process with aggregate indicator

# Use a beta regression with rescaled depth 
beta.reg.depth.agg <- betareg(latent.depth.mean.rs ~ 
                            maxcap.democ + econagg.dum +
                            fp.conc.index + num.mem + wartime + asymm + 
                            asymm.cap + non.maj.only + 
                            mean.threat + low.kap.sc + post45, data = key.data)
summary(beta.reg.depth.agg)



# joint model
uncond.formula.agg <- uncond.milsup ~ maxcap.democ +
  econagg.dum + 
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only + 
  s(mean.threat) + low.kap.sc + s(begyr)

depth.formula.agg <- latent.depth.mean.rs ~ maxcap.democ + 
  econagg.dum +
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only + 
  s(mean.threat) + low.kap.sc + s(begyr)

theta.formula.agg <- ~ s(begyr) + maxcap.democ + num.mem

# Create a list of models
gjrm.models.agg <- vector(mode = "list", length = length(copulas))

# FISK (log-logistic), inverse gaussian, Dagum and beta distributions are best
# in terms of residual fit
# Beta has the lowest AIC.
for(i in 1:length(copulas)){
  gjrm.models.agg[[i]]  <- gjrm(list(uncond.formula.agg, depth.formula.agg,
                                     eq.sigma, theta.formula.agg), 
                                 data = key.data,
                                 margins = c("probit", "BE"),
                                 Model = "B",
                                 BivD = copulas[i]
  )
}
aic.gjrm.agg <- lapply(gjrm.models.agg, AIC)
aic.gjrm.agg

# NB for interpretation: smoothed terms
copulas[17] 
joint.gjrm.agg <- gjrm.models.agg[[17]] 
conv.check(joint.gjrm.agg)
AIC(joint.gjrm.agg)
summary(joint.gjrm.agg)


# Plot smooths
plot(joint.gjrm.agg, eq = 1, seWithMean = TRUE,
     shade = TRUE, pages = 1) # smoothed terms 
plot(joint.gjrm.agg, eq = 2, seWithMean = TRUE,
     shade = TRUE, pages = 1) # smoothed terms 
plot(joint.gjrm.agg, eq = 4, seWithMean = TRUE,
     shade = TRUE, pages = 1) # smoothed terms 





### Trivariate model with issue linkages
depth.formula.tri <- deep.alliance ~ 
  maxcap.lied + maxcap.cons +
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only +
  mean.threat + low.kap.sc + post45

uncond.formula.tri <- uncond.milsup ~ 
  maxcap.lied + maxcap.cons +
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only +
  mean.threat + low.kap.sc + post45

linkage.formula <- econagg.dum ~
  maxcap.lied + maxcap.cons +
  fp.conc.index + num.mem + wartime + asymm +  asymm.cap + non.maj.only +
  mean.threat + low.kap.sc + post45

joint.gjrm.tri <- gjrm(list(uncond.formula.tri, depth.formula.tri, 
                                     linkage.formula), data = key.data,
                                margins = c("probit", "probit", "probit"),
                                Model = "T", 
                                BivD = "N")

# No difference in AIC or convergence across copulas
conv.check(joint.gjrm.tri)
AIC(joint.gjrm.tri)
summary(joint.gjrm.tri)



# switch off gjrm plots 
dev.off()

