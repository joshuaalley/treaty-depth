# Joshua Alley
# Texas A&M University
# Analysis of Treaty depth accounting for uncertainty in the latent measure


# Use brms to analyze multiple datasets and combine the results. 

# create multiple datasets with results from the latent depth analysis


# create an empty list of dataframes with all the variables
brms.data.key <- select(atop.milsup, avg.democ, econagg.dum, uncond.milsup, 
                                     fp.conc.index, num.mem, wartime, asymm,
                                     low.kap.sc, begyr, non.maj.only) 

# Create a list of dataframes 
brms.data.unc <- vector(mode = "list", length = ncol(post.score[1, , ]))

for(i in 1:ncol(post.score[1, , ])){
  brms.data.unc[[i]] <- cbind.data.frame(brms.data.key, post.score[1, , i]) 
  colnames(brms.data.unc[[i]])[11] <- "latent.depth"
}

# rescale depth column:
brms.data.unc.rs <- lapply(brms.data.unc, function(data){
  cbind.data.frame(data, (data[, 11] + 10) / (10 + max(data[, 11]) + .01))
}
)

# rename depth column
for(i in 1:length(brms.data.unc.rs)){
  colnames(brms.data.unc.rs[[i]])[12] <- "latent.depth.rs"
}

# sample 10 at random for an initial run
brms.data.unc.short <- sample(brms.data.unc.rs, size = 500, replace = FALSE)

### run model: brm_multiple
# uses priors and formula from earlier model

# depth model: update formula
bf.depth.unc <- brmsformula(latent.depth.rs ~ avg.democ + econagg.dum + uncond.milsup +
                          fp.conc.index + num.mem + wartime + asymm +
                          low.kap.sc + begyr + non.maj.only,
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

mediation(brm.multivar.unc, treatment = "non.maj.only", prob = .9)


