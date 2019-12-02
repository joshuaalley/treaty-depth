# Joshua Alley
# Analysis of multiple sources of alliance credibility

# data is loaded by the depth-analysis script

# load packages
library(brms)
library(sjstats)

# Logit model of unconditional military support
uncond.glm <- glm(uncond.milsup ~ avg.democ + econagg.dum + latent.depth.mean +
                       fp.conc.index + num.mem + wartime + asymm +
                       low.kap.sc + begyr + us.mem + ussr.mem,
                     family = binomial(link = "logit"),
                     data = atop.milsup)
summary(uncond.glm)


# logit model of economic issue linkages
linkage.glm <- glm(econagg.dum ~ avg.democ + uncond.milsup + latent.depth.mean +
                    fp.conc.index + num.mem + wartime + asymm +
                    low.kap.sc + begyr + us.mem + ussr.mem,
                  family = binomial(link = "logit"),
                  data = atop.milsup)
summary(linkage.glm)



# Joint analysis of multiple sources of credibility

# May need to use a trivariate sample selection model from GJRM (Instrument, though?)
# Mixed trivariate model may be possible in BRMS, but no error correlation
# In general, the design needs to think about alliances that were not formed. Poast k-ad. stuff. 

# Use brms
# set up model formulas and priors
# depth model
bf.depth <- brmsformula(latent.depth.mean ~ avg.democ + econagg.dum + uncond.milsup +
                 fp.conc.index + num.mem + wartime + asymm +
                 low.kap.sc + begyr + asymm.cap,
                 #+ (1 | p | gr(begyr, dist = "gaussian")),
                 center = TRUE) + skew_normal()
depth.priors <- set_prior("normal(0,1)", class = "b", resp = "latentdepthmean") 
              #  set_prior("normal(0,1)", class = "Intercept", resp = "latentdepthmean") 
              #  set_prior("normal(0,.5)", class = "sd", group = "begyr", resp = "latentdepthmean")

# Unconditional military support model  
bf.uncond <- brmsformula(uncond.milsup ~ avg.democ + econagg.dum + 
                  fp.conc.index + num.mem + wartime + asymm +
                  low.kap.sc + begyr + asymm.cap,
                  #+ (1 | p | gr(begyr, dist = "gaussian")), 
                  center = TRUE) + bernoulli(link = "logit")
uncond.priors <-  set_prior("student_t(7, 0, 3)", class = "b", resp = "uncondmilsup") 
                 #  set_prior("student_t(7, 0, 3)", class = "Intercept", resp = "uncondmilsup") 
                 # set_prior("normal(0,.5)", class = "sd", group = "begyr", resp = "uncondmilsup")

# Linkage model 
bf.linkage <- brmsformula(econagg.dum ~ avg.democ + uncond.milsup + 
                   fp.conc.index + num.mem + wartime + asymm +
                   low.kap.sc + begyr + us.mem + ussr.mem,
                   #+ (1 | p | gr(begyr, dist = "gaussian")),
                   center = FALSE) + bernoulli(link = "logit")
linkage.priors <- set_prior("student_t(7, 0, 3)", class = "b", resp = "econaggdum") +
                  set_prior("student_t(7, 0, 3)", class = "Intercept", resp = "econaggdum") 
                #  set_prior("normal(0,.5)", class = "sd", group = "begyr", resp = "econaggdum")

# Fit the model
full.priors <- depth.priors + uncond.priors
brm.multivar <- brm(bf.depth + bf.uncond +
                    set_rescor(FALSE), 
                    data = atop.milsup,
                    prior = full.priors,
                    chains = 2, cores = 2)
# brm.multivar <- add_criterion(brm.multivar, "loo")
pp_check(brm.multivar, resp = "latentdepthmean")
summary(brm.multivar)

mediation(brm.multivar, prob = .9)
