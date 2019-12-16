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

# May need to use a trivariate sample selection model from GJRM (Instrument, though?)
# Mixed trivariate model may be possible in BRMS, but no error correlation
# In general, the design needs to think about alliances that were not formed. Poast k-ad. stuff. 

# Set up unique dataframe
brms.data <- select(atop.milsup, latent.depth.mean, deep.alliance, avg.democ, econagg.dum, uncond.milsup, 
                  fp.conc.index, num.mem, wartime, asymm,
                   low.kap.sc, begyr, asymm.cap, non.maj.only)
brms.data[2:ncol(brms.data)] <- lapply(brms.data[2:ncol(brms.data)], 
                                       function(x) rescale(x, binary.inputs = "0/1")) 

brms.data$latent.depth.mean.rs <- (brms.data$latent.depth.mean + 1) / (1 + max(brms.data$latent.depth.mean) + .01)
summary(brms.data$latent.depth.mean.rs)

# Use brms
# set up model formulas and priors
# depth model
bf.depth <- brmsformula(latent.depth.mean.rs ~ avg.democ + econagg.dum + uncond.milsup +
                 fp.conc.index + num.mem + wartime + asymm +
                 low.kap.sc + begyr + non.maj.only,
                 #+ (1 | p | gr(begyr, dist = "gaussian")),
                 center = TRUE) + Beta(link = "logit", link_phi = "log")
depth.priors <- set_prior("normal(0, 1)", class = "b", resp = "latentdepthmeanrs") 

# Unconditional military support model  
bf.uncond <- brmsformula(uncond.milsup ~ avg.democ + econagg.dum + 
                  fp.conc.index + num.mem + wartime + asymm +
                  low.kap.sc + begyr + non.maj.only,
                  #+ (1 | p | gr(begyr, dist = "gaussian")), 
                  center = TRUE) + bernoulli(link = "logit")
uncond.priors <-  set_prior("student_t(7, 0, 3)", class = "b", resp = "uncondmilsup") 

# Fit the model
full.priors <- depth.priors + uncond.priors
brm.multivar <- brm(bf.depth + bf.uncond +
                    set_rescor(FALSE), 
                    data = brms.data,
                    prior = full.priors,
                    chains = 2, cores = 2)
# brm.multivar <- add_criterion(brm.multivar, "loo", reloo = TRUE)
pp_check(brm.multivar, resp = "latentdepthmeanrs")
summary(brm.multivar)

mediation(brm.multivar, treatment = "avg.democ", prob = .9)
mediation(brm.multivar, treatment = "num.mem", prob = .9)
# mediation(brm.multivar, treatment = "asymm.cap", prob = .9)
mediation(brm.multivar, treatment = "non.maj.only", prob = .9)


# use the mediation package instead: (this is not working well)
# only accepts binary outputs or lm: rather deep dummy than non-robust regression 
# can't use sensitivity analysis here, however. 
med.out <- mediate(depth.glm, uncond.glm, treat = "non.maj.only", mediator = "uncond.milsup",
                   control.value = 0, treat.value = 1,
                   robustSE = TRUE, sims = 100)
summary(med.out)



# fit the model with a dummy indicator of depth
# depth model
bf.depth.dum <- brmsformula(deep.alliance ~ avg.democ + econagg.dum + uncond.milsup +
                          fp.conc.index + num.mem + wartime + asymm +
                          low.kap.sc + begyr + non.maj.only,
                        #+ (1 | p | gr(begyr, dist = "gaussian")),
                        center = TRUE) + bernoulli(link = "logit")
depth.priors.dum <- set_prior("student_t(7, 0, 3)", class = "b", resp = "deepalliance") 

# Unconditional military support model  
bf.uncond <- brmsformula(uncond.milsup ~ avg.democ + econagg.dum + 
                           fp.conc.index + num.mem + wartime + asymm +
                           low.kap.sc + begyr + non.maj.only,
                         #+ (1 | p | gr(begyr, dist = "gaussian")), 
                         center = TRUE) + bernoulli(link = "logit")
uncond.priors <-  set_prior("student_t(7, 0, 3)", class = "b", resp = "uncondmilsup") 

# Fit the model
full.priors.dum <- depth.priors.dum + uncond.priors
brm.multivar.dum <- brm(bf.depth.dum + bf.uncond +
                      set_rescor(FALSE), 
                    data = brms.data,
                    prior = full.priors.dum,
                    chains = 2, cores = 2)
# brm.multivar <- add_criterion(brm.multivar, "loo", reloo = TRUE)
summary(brm.multivar.dum)

mediation(brm.multivar.dum, treatment = "avg.democ", prob = .9)
mediation(brm.multivar.dum, treatment = "num.mem", prob = .9)
# mediation(brm.multivar.dum, treatment = "asymm.cap", prob = .9)
mediation(brm.multivar.dum, treatment = "non.maj.only", prob = .9)



# tabulate the results
# latent depth mean
med.res.mean <- mediation(brm.multivar, treatment = "non.maj.only", prob = .9)
xtable(med.res.mean) # will probably plot instead
med.mean.plot  <- as_tibble(med.res.mean) %>% filter(effect == "direct" | effect == "indirect" | effect == "total") %>%
                    ggplot(aes(y = effect, x = value)) +
                     geom_vline(xintercept = 0) +
                     geom_point(size = 3) +
                     geom_errorbarh(aes(xmin = hdi.low, xmax = hdi.high,
                      height = .1), size = 1) + 
                     labs(x = "Estimated Effect", y = "Effect Type") +
                     ggtitle("Mean Latent Depth") +
                     theme_bw()
med.mean.plot


# Deep alliance dummy 
med.res.dum <- mediation(brm.multivar.dum, treatment = "non.maj.only", prob = .9)
xtable(med.res.dum) # will probably plot instead
med.dum.plot  <- as_tibble(med.res.dum) %>% filter(effect == "direct" | effect == "indirect" | effect == "total") %>%
  ggplot(aes(y = effect, x = value)) +
  geom_vline(xintercept = 0) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = hdi.low, xmax = hdi.high,
                     height = .1), size = 1) + 
  labs(x = "Estimated Effect", y = "Effect Type") +
  ggtitle("Deep Alliance Dummy") +
  theme_bw()
med.dum.plot

multiplot.ggplot(med.mean.plot, med.dum.plot)
