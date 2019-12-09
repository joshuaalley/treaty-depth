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
                    fp.conc.index + num.mem + wartime + asymm + asymm.cap +
                    low.kap.sc + begyr + us.mem + ussr.mem,
                  family = binomial(link = "probit"),
                  data = atop.milsup)
summary(linkage.glm)



# Joint analysis of multiple sources of credibility

# May need to use a trivariate sample selection model from GJRM (Instrument, though?)
# Mixed trivariate model may be possible in BRMS, but no error correlation
# In general, the design needs to think about alliances that were not formed. Poast k-ad. stuff. 

# Set up unique dataframe
brms.data <- select(atop.milsup, latent.depth.mean, avg.democ, econagg.dum, uncond.milsup, 
                  fp.conc.index, num.mem, wartime, asymm,
                   low.kap.sc, begyr, asymm.cap, non.maj.only)
brms.data[2:ncol(brms.data)] <- lapply(brms.data[2:ncol(brms.data)], 
                                       function(x) rescale(x, binary.inputs = "0/1")) 

# Use brms
# set up model formulas and priors
# depth model
bf.depth <- brmsformula(latent.depth.mean ~ avg.democ + econagg.dum + uncond.milsup +
                 fp.conc.index + num.mem + wartime + asymm +
                 low.kap.sc + begyr + asymm.cap + non.maj.only,
                 #+ (1 | p | gr(begyr, dist = "gaussian")),
                 center = TRUE) + skew_normal()
depth.priors <- set_prior("normal(0, 1)", class = "b", resp = "latentdepthmean") 
              #  set_prior("normal(0,1)", class = "Intercept", resp = "latentdepthmean") 
              #  set_prior("normal(0,.5)", class = "sd", group = "begyr", resp = "latentdepthmean")

# Unconditional military support model  
bf.uncond <- brmsformula(uncond.milsup ~ avg.democ + econagg.dum + 
                  fp.conc.index + num.mem + wartime + asymm +
                  low.kap.sc + begyr + asymm.cap + non.maj.only,
                  #+ (1 | p | gr(begyr, dist = "gaussian")), 
                  center = TRUE) + bernoulli(link = "logit")
uncond.priors <-  set_prior("student_t(7, 0, 3)", class = "b", resp = "uncondmilsup") 
                 #  set_prior("student_t(7, 0, 3)", class = "Intercept", resp = "uncondmilsup") 
                 # set_prior("normal(0,.5)", class = "sd", group = "begyr", resp = "uncondmilsup")


# Fit the model
full.priors <- depth.priors + uncond.priors
brm.multivar <- brm(bf.depth + bf.uncond +
                    set_rescor(FALSE), 
                    data = brms.data,
                    prior = full.priors,
                    chains = 2, cores = 2)
# brm.multivar <- add_criterion(brm.multivar, "loo")
pp_check(brm.multivar, resp = "latentdepthmean")
summary(brm.multivar)

mediation(brm.multivar, treatment = "avg.democ", prob = .9)
mediation(brm.multivar, treatment = "num.mem", prob = .9)
mediation(brm.multivar, treatment = "asymm.cap", prob = .9)
mediation(brm.multivar, treatment = "non.maj.only", prob = .9)


# use the mediation package instead: (this is not working well)
# only accepts binary outputs or lm: rather deep dummy than non-robust regression 
med.out <- mediate(depth.reg.super, uncond.glm, treat = "non.maj.only", mediator = "uncond.milsup",
                   control.value = -7, treat.value = .61,
                   robustSE = TRUE, sims = 100)
summary(med.out)
sens.out <- medsens(med.out, rho.by = 0.1, effect.type = "indirect", sims = 100)
summary(sens.out)


med.out <- mediate(depth.glm, uncond.glm, treat = "non.maj.only", mediator = "uncond.milsup",
                   control.value = 0, treat.value = 1,
                   robustSE = TRUE, sims = 100)
summary(med.out)
