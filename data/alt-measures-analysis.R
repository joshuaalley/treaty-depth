# Joshua Alley
# Texas A&M University
# Analysis with Leeds and Anac meaure of institutionalization
# same variable in Mattes 2012


# Plot the two variables against each other
ggplot(atop.milsup, aes(x = as.ordered(milinst), y = latent.depth.mean)) +
  geom_boxplot() +
  geom_jitter(alpha = .5) +
  theme_bw()
 

# Correlation between my latent measure and milinst
cor.test(atop.milsup$latent.depth.mean, atop.milsup$milinst)
# Strong positive correlation

# add ordering to the variable
atop.milsup$milinst <- as.ordered(atop.milsup$milinst)


# Fit the same model specification on an ordinal outcome
milinst.m1 <- polr(milinst ~ avg.democ + econagg.dum + uncond.milsup +
                    fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only +
                    low.kap.sc + us.mem + ussr.mem,
                  data = atop.milsup)
summary(milinst.m1)



# Specify milinst formula and set variable to ordered
bf.milinst <- brmsformula(milinst ~ avg.democ + econagg.dum + uncond.milsup +
                          fp.conc.index + num.mem + wartime + asymm +
                          low.kap.sc + begyr + asymm.cap + non.maj.only,
                        center = TRUE) + cumulative(link = "logit", threshold = "flexible")
milinst.priors <- set_prior("normal(0, 1)", class = "b", resp = "milinst") 

brms.data$milinst <- as.ordered(brms.data$milinst)

# Keep the same unconditional military support model  

# Fit the model
joint.milinst.priors <- milinst.priors + uncond.priors
brm.milinst <- brm(bf.milinst + bf.uncond +
                      set_rescor(FALSE), 
                    data = brms.data,
                    prior = joint.milinst.priors,
                    chains = 2, cores = 2)
summary(brm.milinst)

mediation(brm.milinst, treatment = "non.maj.only", prob = .9)



### Look at the Benson and Clinton results
benson.clinton.2016 <- read.csv("data/benson-clinton-scores.csv", row.names = FALSE)
head(benson.clinton.2016)

# combine with my data
benson.clinton.comp <- left_join(atop.milsup, benson.clinton.2016, by = "atopid")

# rescale variables to facilitate comparison
benson.clinton.comp$latent.depth.rs <- rescale(benson.clinton.comp$latent.depth.mean)
benson.clinton.comp$Depth.score.rs <- rescale(benson.clinton.comp$Depth.score)

# correlation is strong and positive
cor.test(benson.clinton.comp$latent.depth.mean, benson.clinton.comp$Depth.score)

# compare summary statistics
summary(benson.clinton.comp$latent.depth.rs)
summary(benson.clinton.comp$Depth.score.rs)

# plot the meaures individually, then together
hist.bfa <- ggplot(benson.clinton.comp, aes(x = latent.depth.rs)) +
  geom_histogram() +
  ggtitle("Alley")
hist.bc16 <- ggplot(benson.clinton.comp, aes(x = Depth.score.rs)) +
  geom_histogram() +
  ggtitle("Benson and Clinton")
multiplot.ggplot(hist.bfa, hist.bc16)

# Scatter plot
ggplot(benson.clinton.comp, aes(x = Depth.score.rs, y = latent.depth.rs)) +
  geom_point() +
  theme_bw()


# Analysis with the Benson and Clinton measure
# need a new dataframe 
# Set up unique dataframe
brms.data.bc <- select(benson.clinton.comp, Depth.score, avg.democ, econagg.dum, uncond.milsup, 
                    fp.conc.index, num.mem, wartime, asymm,
                    low.kap.sc, begyr, asymm.cap, non.maj.only)
brms.data.bc[2:ncol(brms.data.bc)] <- lapply(brms.data.bc[2:ncol(brms.data.bc)], 
                                       function(x) rescale(x, binary.inputs = "0/1")) 

brms.data.bc$depthscore.rs.max <- (brms.data.bc$Depth.score + 1) / (1 + max(brms.data.bc$Depth.score, na.rm = TRUE) + .01)
summary(brms.data.bc$depthscore.rs.max)

# Specify bc depth formula
bf.bc <- brmsformula(depthscore.rs.max ~ avg.democ + econagg.dum + uncond.milsup +
                            fp.conc.index + num.mem + wartime + asymm +
                            low.kap.sc + begyr + asymm.cap + non.maj.only,
                          center = TRUE) + Beta(link = "logit", link_phi = "log")
bc.priors <- set_prior("normal(0, 1)", class = "b", resp = "depthscorersmax") 


# Keep the same unconditional military support model  

# Fit the model
joint.bc.priors <- bc.priors + uncond.priors
brm.bcdepth <- brm(bf.bc + bf.uncond +
                     set_rescor(FALSE), 
                   data = brms.data.bc,
                   prior = joint.bc.priors,
                   chains = 2, cores = 2)
summary(brm.bcdepth)

mediation(brm.bcdepth, treatment = "non.maj.only", prob = .9)
