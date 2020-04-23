# Joshua Alley
# Texas A&M University





# Analysis with Leeds and Anac meaure of institutionalization
# same variable in Mattes 2012
# Plot the two variables against each other
ggplot(atop.milsup, aes(x = as.ordered(milinst), y = latent.depth.mean)) +
  geom_boxplot() +
  geom_jitter(alpha = .5) +
  labs(x = "Military Institutionalization", y = "Latent Depth") +
  theme_bw()
ggsave("appendix/milinst-comp.png", height = 6, width = 8) 


# Correlation between my latent measure and milinst
cor.test(atop.milsup$latent.depth.mean, atop.milsup$milinst)
# Strong positive correlation

# Fit the same model specification on an ordinal outcome
milinst.m1 <- polr(as.ordered(milinst) ~ maxcap.democ + econagg.dum + uncond.milsup +
                    fp.conc.index + num.mem + wartime + asymm + asymm.cap + mean.threat +
                    low.kap.sc,
                  data = atop.milsup)
summary(milinst.m1)


# Set up the key formula and data
table(atop.milsup$milinst)
atop.milsup$high.milinst <- ifelse(atop.milsup$milinst > 1, 1, 0)
table(atop.milsup$high.milinst)


key.data.milinst <- select(atop.milsup, atopid, latent.depth.mean, econagg.dum, uncond.milsup, 
                   fp.conc.index, num.mem, wartime, asymm, deep.alliance, non.maj.only,
                   low.kap.sc, begyr, asymm.cap, mean.threat, milinst, high.milinst,
                   dem.prop, joint.democ, maxcap.democ, max.democ, avg.democ.weight, max.democ.weight) %>%
                   drop_na()


# Define formulas 
milinst.formula <- high.milinst ~ maxcap.democ + econagg.dum + 
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only + 
  s(mean.threat) + low.kap.sc + s(begyr)


uncond.formula.milinst <- uncond.milsup ~ maxcap.democ + econagg.dum + 
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only + 
  s(mean.threat) + low.kap.sc + s(begyr)



# Create a list of models
gjrm.models.milinst <- vector(mode = "list", length = length(copulas))

# Cannot fit an ordinal version with this version of gjrm
# Dummy for high 
for(i in 1:length(copulas)){
  gjrm.models.milinst[[i]]  <- gjrm(list(uncond.formula, milinst.formula,
                                        theta.formula), 
                                    data = key.data.milinst,
                            margins = c("probit", "probit"),
                            Model = "B",
                            BivD = copulas[i]
  )
}
aic.milinst <- lapply(gjrm.models.milinst, AIC)
aic.milinst

# Summarize results
copulas[1] # best copula by AIC and convergence
joint.gjrm.milinst <- gjrm.models.milinst[[1]] 
conv.check(joint.gjrm.milinst)
AIC(joint.gjrm.milinst)
summary(joint.gjrm.milinst)


# plot smooths 
plot(joint.gjrm.milinst, eq = 1, seWithMean = TRUE,
     shade = TRUE, pages = 1) # smoothed terms 
plot(joint.gjrm.milinst, eq = 2, seWithMean = TRUE,
     shade = TRUE, pages = 1) # smoothed terms 


### Look at the Benson and Clinton results
benson.clinton.2016 <- read.csv("data/benson-clinton-scores.csv")
head(benson.clinton.2016)

# combine with my data
benson.clinton.comp <- left_join(atop.milsup, benson.clinton.2016, by = "atopid")

# rescale variables to facilitate comparison
benson.clinton.comp$latent.depth.rs <- rescale(benson.clinton.comp$latent.depth.mean)
benson.clinton.comp$Depth.score.rs <- rescale(benson.clinton.comp$Depth.score)

# correlation is strong and positive
cor.test(benson.clinton.comp$latent.depth.mean, benson.clinton.comp$Depth.score)

# compare summary statistics: more outliers
summary(benson.clinton.comp$latent.depth.rs)
summary(benson.clinton.comp$Depth.score.rs)

# plot the meaures individually, then together
hist.bfa <- ggplot(benson.clinton.comp, aes(x = latent.depth.rs)) +
  geom_histogram() +
  ggtitle("Alley")
hist.bc16 <- ggplot(benson.clinton.comp, aes(x = Depth.score.rs)) +
  geom_histogram() +
  ggtitle("Benson and Clinton")
grid.arrange(hist.bfa, hist.bc16)

# Scatter plot
ggplot(benson.clinton.comp, aes(x = Depth.score.rs, y = latent.depth.rs)) +
  geom_point() +
  theme_bw()


# Analysis with the Benson and Clinton measure
# need a new dataframe 
# Set up unique dataframe
key.data.bc <- select(benson.clinton.comp, Depth.score, maxcap.democ, econagg.dum, uncond.milsup, 
                    fp.conc.index, num.mem, wartime, asymm, non.maj.only,
                    low.kap.sc, begyr, asymm.cap, mean.threat) %>%
                    drop_na()
key.data.bc$depthscore.rs.max <- (key.data.bc$Depth.score + 1) / (1 + max(key.data.bc$Depth.score, na.rm = TRUE) + .01)
summary(key.data.bc$depthscore.rs.max)


# univariate: beta regression
beta.reg.bcdepth <- betareg(depthscore.rs.max ~ maxcap.democ + 
                            fp.conc.index + num.mem + wartime + asymm + 
                            asymm.cap + non.maj.only + 
                            mean.threat + low.kap.sc + begyr, data = key.data.bc)
summary(beta.reg.bcdepth)


## Bivariate model
# Specify bc depth formula
bcdepth.formula <- depthscore.rs.max ~ maxcap.democ +  
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only + 
  s(mean.threat) + low.kap.sc + s(begyr)

# Alternative unconditional formula
bcuncond.formula <- uncond.milsup ~ maxcap.democ + 
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only + 
  s(mean.threat) + low.kap.sc + s(begyr)

# Create a list of models
gjrm.models.bcdepth <- vector(mode = "list", length = length(copulas))

# fit the models with different copulas
# Use a logit link for unconditional military support: probit gives huge standard errors
for(i in 1:length(copulas)){
  gjrm.models.bcdepth[[i]]  <- gjrm(list(bcuncond.formula, bcdepth.formula,
                                         eq.sigma, theta.formula), 
                                    data = key.data.bc,
                                    margins = c("probit", "BE"),
                                    Model = "B",
                                    BivD = copulas[i]
  )
}
aic.bcdepth <- lapply(gjrm.models.bcdepth, AIC)
aic.bcdepth

# Summarize results
copulas[17] # best copula for AIC and covergence 
joint.gjrm.bc <- gjrm.models.bcdepth[[17]] 
conv.check(joint.gjrm.bc)
AIC(joint.gjrm.bc)
summary(joint.gjrm.bc)
# residual fit is poor because Benson and Clinton measure has more outliers
post.check(joint.gjrm.bc) 

# plot smooths 
plot(joint.gjrm.bc, eq = 1, seWithMean = TRUE,
     shade = TRUE, pages = 1) # smoothed terms 
plot(joint.gjrm.bc, eq = 2, seWithMean = TRUE,
     shade = TRUE, pages = 1) # smoothed terms 





### Plot/summarize the results

# Start with univariate models

# substantive predictions from single-equation model: military institutionalization
margins.milinst <- ggeffect(milinst.m1, terms = c("maxcap.democ"),
                            interval = "confidence")
plot.milinst.sep <- ggplot(margins.milinst, aes(x = x, y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = .1) +
  facet_wrap(~ response.level, labeller = labeller(
    response.level = c(X0 = "None",
                       X1 = "Moderate",
                       X2 = "High"))
    ) +
  labs(x = "Alliance Leader Democracy",
       y = "Predicted Marginal Effect") +
  ggtitle("Military Institutionalization") +
  theme_bw()
plot.milinst.sep


# substantive predictions from single-equation model: benson and clinton depth
margins.beta.bc <- ggeffect(beta.reg.bcdepth, terms = c("maxcap.democ"),
                         interval = "confidence")
plot.sep.bc <- ggplot(margins.beta.bc, aes(x = x, y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = .1) +
  labs(x = "Alliance Leader Democracy",
       y = "Predicted Marginal Effect on Treaty Depth") +
  ggtitle("Benson and Clinton Depth") +
  theme_bw()
plot.sep.bc


# Combine the plots
grid.arrange(plot.milinst.sep, plot.sep.bc, ncol = 2)
results.alt.measures.sep <- arrangeGrob(plot.milinst.sep, plot.sep.bc, 
                                    ncol = 2)
ggsave("appendix/results-alt-measures-sep.png", results.alt.measures.sep,
       height = 6, width = 8) #save file


# military institutionalization
joint.pred.milinst <- predict(joint.gjrm.milinst, eq = 2,
                            type = "response", 
                            se.fit = TRUE,
                            newdata = sim.data.mcap)

pred.milinst.democ <- cbind.data.frame(joint.pred.milinst$fit, 
                                     joint.pred.milinst$se.fit,
                                     sim.data.mcap$maxcap.democ)
colnames(pred.milinst.democ) <- c("pred", "se", "maxcap.democ")

# calculate lower and upper bounds of 95% CI
pred.milinst.democ$lower <- pred.milinst.democ$pred - 2*pred.milinst.democ$se
pred.milinst.democ$upper <- pred.milinst.democ$pred + 2*pred.milinst.democ$se

# get predictions and unc back on response scale
pred.milinst.democ$response <- linkinv(pred.milinst.democ$pred)
pred.milinst.democ$lower.res <- linkinv(pred.milinst.democ$lower)
pred.milinst.democ$upper.res <- linkinv(pred.milinst.democ$upper)


# plot predicted milinst values
plot.milinst <- ggplot(pred.milinst.democ, aes(x = maxcap.democ, y = response)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower.res,
                    ymax = upper.res),
                width = .1, size = 1) +
  labs(y = "Pr(High Military Institutionalization)",
       x = "POLITY Score: Most Capable State") +
  ggtitle("Military Institutionalization") +
  theme_bw()
plot.milinst



# Benson and Clintron latent depth
joint.pred.bcdepth <- predict(joint.gjrm.bc, eq = 2,
                            type = "response", 
                            se.fit = TRUE,
                            newdata = sim.data.mcap)

pred.bcdepth.democ <- cbind.data.frame(joint.pred.bcdepth$fit, 
                                     joint.pred.bcdepth$se.fit,
                                     sim.data.mcap$maxcap.democ)
colnames(pred.bcdepth.democ) <- c("pred", "se", "maxcap.democ")


# calculate lower and upper bounds of 95% CI
pred.bcdepth.democ$lower <- pred.bcdepth.democ$pred - 2*pred.bcdepth.democ$se
pred.bcdepth.democ$upper <- pred.bcdepth.democ$pred + 2*pred.bcdepth.democ$se

# get predictions and unc back on response scale
pred.bcdepth.democ$response <- linkinv(pred.bcdepth.democ$pred)
pred.bcdepth.democ$lower.res <- linkinv(pred.bcdepth.democ$lower)
pred.bcdepth.democ$upper.res <- linkinv(pred.bcdepth.democ$upper)



plot.bcdepth <- ggplot(pred.bcdepth.democ, aes(x = maxcap.democ, y = response)) +
  geom_hline(yintercept = 0) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower.res,
                    ymax = upper.res),
                width = .1, size = 1) +
  labs(x = "Alliance Leader Polity Score", y = "Predicted Treaty Depth") +
  ggtitle("Treaty Depth: Benson and Clinton") +
  theme_bw()
plot.bcdepth


# combine plots and export
grid.arrange(plot.milinst, plot.bcdepth, ncol = 2)
results.alt.measures <- arrangeGrob(plot.milinst, plot.bcdepth,
                            ncol = 2)
ggsave("appendix/results-alt-measures.png", results.alt.measures,
       height = 6, width = 8) #save file
