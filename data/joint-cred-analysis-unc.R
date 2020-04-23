# Joshua Alley
# Texas A&M University
# Analysis of Treaty depth accounting for uncertainty in the latent measure




# Use brms
# set up model formulas and priors
# depth model
bf.depth <- brmsformula(latent.depth.mean.rs ~ maxcap.democ +  econagg.dum +
                          fp.conc.index + num.mem + wartime + 
                          asymm + asymm.cap + non.maj.only +
                          mean.threat + low.kap.sc + begyr,
                        center = TRUE) + Beta(link = "logit", link_phi = "log")
depth.priors <- set_prior("normal(0, 1)", class = "b", resp = "latentdepthmeanrs") 

# Unconditional military support model  
bf.uncond <- brmsformula(uncond.milsup ~ maxcap.democ + econagg.dum + 
                           fp.conc.index + num.mem + wartime + 
                           asymm + asymm.cap + non.maj.only +
                           mean.threat + low.kap.sc + begyr,
                         center = TRUE) + bernoulli(link = "logit")
uncond.priors <- set_prior("student_t(7, 0, 3)", class = "b", resp = "uncondmilsup")

               
# Fit the model
full.priors <- depth.priors + uncond.priors
brm.multivar <- brm(bf.depth + bf.uncond +
                      set_rescor(FALSE), 
                    data = key.data,
                    prior = full.priors,
                    chains = 2, cores = 2)
# brm.multivar <- add_criterion(brm.multivar, "loo", reloo = TRUE)
pp_check(brm.multivar, resp = "latentdepthmeanrs")
summary(brm.multivar)


# Plot marginal effects of average democracy
plot(conditional_effects(brm.multivar,
                        effects = "maxcap.democ",
                        method = "fitted",
                        theme = "bw"),
                     ask = FALSE)



# Use brms to analyze multiple datasets and combine the results. 

# create multiple datasets with results from the latent depth analysis


# create an empty list of dataframes with all the variables
brms.data.key <- select(atop.milsup, maxcap.democ, econagg.dum, uncond.milsup, 
                                     fp.conc.index, num.mem, wartime, asymm,
                                     asymm.cap, non.maj.only,
                                     low.kap.sc, begyr, mean.threat) 

# Create a list of dataframes and bind them to alliance vars
brms.data.unc <- vector(mode = "list", length = ncol(post.score[1, , ]))

for(i in 1:ncol(post.score[1, , ])){
  brms.data.unc[[i]] <- cbind.data.frame(brms.data.key, post.score[1, , i]) 
  colnames(brms.data.unc[[i]])[13] <- "latent.depth"
}

# rescale depth column:
brms.data.unc.rs <- lapply(brms.data.unc, function(data){
  cbind.data.frame(data, (data[, 13] + 10) / (10 + max(data[, 13]) + .01))
}
)

# rename depth column
for(i in 1:length(brms.data.unc.rs)){
  colnames(brms.data.unc.rs[[i]])[14] <- "latent.depth.rs"
}

# sample 500 at random for an initial run
brms.data.unc.short <- sample(brms.data.unc.rs, size = 500, replace = FALSE)

### run model: brm_multiple
# uses priors and formula from earlier model

# depth model: update formula
bf.depth.unc <- brmsformula(latent.depth.rs ~ maxcap.democ + econagg.dum + 
                          fp.conc.index + num.mem + wartime + asymm + 
                            asymm.cap + non.maj.only + mean.threat +
                          low.kap.sc + begyr,
                        #+ (1 | p | gr(begyr, dist = "gaussian")),
                        center = TRUE) + Beta(link = "logit", link_phi = "log")
depth.priors.unc <- set_prior("normal(0, 1)", class = "b", resp = "latentdepthrs") 
# same uncond milsup model and priors as earlier analysis
priors.unc <- depth.priors.unc + uncond.priors

# run the model on 500 of the 1000 datasets
# 1000 datasets has 12+ hour run time
system.time(
brm.multivar.unc <- brm_multiple(bf.depth.unc + bf.uncond +
                      set_rescor(FALSE), 
                    data = brms.data.unc.short,
                    prior = priors.unc,
                    chains = 2, cores = 2)
)

# Examine results
pp_check(brm.multivar.unc, resp = "latentdepthrs")
summary(brm.multivar.unc)

# rhat warning is the result of non-overlapping chains in the submodels
round(brm.multivar.unc$rhats, 2) # rhats are fine in the submodels
summary(brm.multivar.unc$rhats) # summary by parameters
max(brm.multivar.unc$rhats) # maximum rhat is not problematic


# Summarize brms results with uncertainty
summary.brms <- summary(brm.multivar.unc)
summary.brms


# Plot key intervals
color_scheme_set(scheme = "gray")
intervals.unc <- mcmc_areas(brm.multivar.unc,
                                pars = c("b_latentdepthrs_maxcap.democ",
                                         "b_uncondmilsup_maxcap.democ"),
                                prob = .9,
                                point_est = "median"
                                )
intervals.unc +
  vline_0(linetype = 2) + 
  scale_x_continuous(limits = c(-.1, .05)) +
  scale_y_discrete(
    labels = c(b_latentdepthrs_maxcap.democ = "Latent Depth", 
            b_uncondmilsup_maxcap.democ = "Unconditional Support")
    ) +
  ggtitle("Effect of Alliance Leader Polity") +
  labs(x = "Estimate", y = "") +
  theme_bw()
ggsave("appendix/results-unc-depth.png", height = 6, width = 8)
