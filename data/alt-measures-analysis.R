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

# Fit the same model specification on an ordinal outcome
milinst.m1 <- polr(as.ordered(milinst) ~ max.democ + econagg.dum + uncond.milsup +
                    fp.conc.index + num.mem + wartime + asymm + asymm.cap + mean.threat +
                    low.kap.sc,
                  data = atop.milsup)
summary(milinst.m1)


# Set up the key formula and data
table(atop.milsup$milinst)
atop.milsup$high.milinst <- ifelse(atop.milsup$milinst == 2, 1, 0)
table(atop.milsup$high.milinst)


# Define formulas 
milinst.formula <- high.milinst ~ s(max.democ) + econagg.dum +
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + s(mean.threat) +
  low.kap.sc + s(begyr)

uncond.formula.milinst <- uncond.milsup ~ s(max.democ) + econagg.dum +
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + s(mean.threat) +
  low.kap.sc + s(begyr)


# Create a list of models
gjrm.models.milinst <- vector(mode = "list", length = length(copulas))

# Cannot fit an ordinal version with this version of gjrm
# Dummy for high 
for(i in 1:length(copulas)){
  gjrm.models.milinst[[i]]  <- gjrm(list(uncond.formula, milinst.formula,
                                        theta.formula.max), 
                                    data = atop.milsup,
                            margins = c("probit", "probit"),
                            Model = "B",
                            BivD = copulas[i]
  )
}
aic.milinst <- lapply(gjrm.models.milinst, AIC)
aic.milinst
lapply(gjrm.models.milinst, conv.check)

# Summarize results
copulas[16] # normal remains the best
joint.gjrm.milinst <- gjrm.models.milinst[[16]] 
conv.check(joint.gjrm.milinst)
AIC(joint.gjrm.milinst)
summary(joint.gjrm.milinst)



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
key.data.bc <- select(benson.clinton.comp, Depth.score, max.democ, econagg.dum, uncond.milsup, 
                    fp.conc.index, num.mem, wartime, asymm,
                    low.kap.sc, begyr, asymm.cap, mean.threat)
key.data.bc$depthscore.rs.max <- (key.data.bc$Depth.score + 1) / (1 + max(key.data.bc$Depth.score, na.rm = TRUE) + .01)
summary(key.data.bc$depthscore.rs.max)

# Specify bc depth formula
bcdepth.formula <- depthscore.rs.max ~ s(max.democ) + econagg.dum +
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + s(mean.threat) +
  low.kap.sc + s(begyr)

# Alternative unconditional formula
bcuncond.formula <- uncond.milsup ~ s(max.democ) + econagg.dum +
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + s(mean.threat) +
  low.kap.sc + s(begyr)

# Create a list of models
gjrm.models.bcdepth <- vector(mode = "list", length = length(copulas))

# fit the models with different copulas
# Use a logit link for unconditional military support: probit gives huge standard errors
for(i in 1:length(copulas)){
  gjrm.models.bcdepth[[i]]  <- gjrm(list(bcuncond.formula, bcdepth.formula,
                                         eq.sigma, theta.formula.max), 
                                    data = key.data.bc,
                                    margins = c("logit", "BE"),
                                    Model = "B",
                                    BivD = copulas[i]
  )
}
aic.bcdepth <- lapply(gjrm.models.bcdepth, AIC)
aic.bcdepth
lapply(gjrm.models.bcdepth, conv.check)

# Summarize results
copulas[14] # normal remains the best
joint.gjrm.bc <- gjrm.models.bcdepth[[14]] 
conv.check(joint.gjrm.bc)
AIC(joint.gjrm.bc)
summary(joint.gjrm.bc)
# residual fit is really poor because Benson and Clinton measure has more outliers
post.check(joint.gjrm.bc) 

# plot smooths 
plot(joint.gjrm.bc, eq = 1, seWithMean = TRUE,
     shade = TRUE, pages = 1) # smoothed terms 
plot(joint.gjrm.bc, eq = 2, seWithMean = TRUE,
     shade = TRUE, pages = 1) # smoothed terms 

