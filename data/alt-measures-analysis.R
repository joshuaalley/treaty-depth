# Joshua Alley
# analysis of other treaty depth measures





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
milinst.m1 <- polr(as.ordered(milinst) ~ maxcap.lied + maxcap.cons +
                     econagg.dum + 
                    fp.conc.index + num.mem + wartime + asymm + asymm.cap + mean.threat +
                    low.kap.sc,
                  data = atop.milsup)
summary(milinst.m1)



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
                    low.kap.sc, begyr, asymm.cap, mean.threat,
                    maxcap.lied, maxcap.cons) %>%
                    drop_na()
key.data.bc$depthscore.rs.max <- (key.data.bc$Depth.score + 1) / (1 + max(key.data.bc$Depth.score, na.rm = TRUE) + .01)
summary(key.data.bc$depthscore.rs.max)


# univariate: beta regression
beta.reg.bcdepth <- betareg(depthscore.rs.max ~ 
                            maxcap.lied + maxcap.cons +
                            fp.conc.index + num.mem + wartime + asymm + 
                            asymm.cap + non.maj.only + 
                            mean.threat + low.kap.sc + begyr, data = key.data.bc)
summary(beta.reg.bcdepth)


### Plot/summarize the results

# Start with univariate models

# substantive predictions from single-equation model: military institutionalization
margins.milinst <- ggeffect(milinst.m1, terms = c("maxcap.lied"),
                            interval = "confidence")
plot.milinst.sep <- ggplot(margins.milinst, aes(x = factor(x), y = predicted)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
              width = .1) +
  facet_wrap(~ response.level, labeller = labeller(
    response.level = c(X0 = "None",
                       X1 = "Moderate",
                       X2 = "High"))
    ) +
  labs(x = "Alliance Leader Electoral Democracy",
       y = "Predicted Marginal Effect") +
  ggtitle("Military Institutionalization") +
  theme_bw()
plot.milinst.sep


# substantive predictions from single-equation model: benson and clinton depth
margins.beta.bc <- ggeffect(beta.reg.bcdepth, terms = c("maxcap.lied"),
                         interval = "confidence")
plot.sep.bc <- ggplot(margins.beta.bc, aes(x = factor(x), y = predicted)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = .1) +
  labs(x = "Alliance Leader Electoral Democracy",
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



