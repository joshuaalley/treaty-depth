# Joshua Alley
# Bivariate depth model robustness checks
# must run after joint-cred-analysis.R 

# add a control for US membership
uncond.formula.us <- uncond.milsup ~ maxcap.cons + maxcap.lied +
  econagg.dum + 
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only + 
  s(mean.threat) + low.kap.sc + s(begyr) + us.mem

depth.formula.us <- latent.depth.mean.rs ~ maxcap.cons + maxcap.lied +
  econagg.dum +
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only + 
  s(mean.threat) + low.kap.sc + s(begyr) + us.mem


gjrm.us <- gjrm(list(uncond.formula.us, depth.formula.us,
                               eq.sigma, theta.formula), 
                          data = key.data,
                          margins = c("probit", "BE"),
                          Model = "B",
                          BivD = "N"
                      ) 
conv.check(gjrm.us)
summary(gjrm.us)


# dummy of high LIED (> 4)
key.data$maxcap.liedh <- ifelse(key.data$maxcap.lied >= 4, 1, 0)
# formulas with alt IV
uncond.formula.lih <- uncond.milsup ~ maxcap.cons + maxcap.liedh +
  econagg.dum + 
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only + 
  s(mean.threat) + low.kap.sc + s(begyr) + us.mem

depth.formula.lih <- latent.depth.mean.rs ~ maxcap.cons + maxcap.liedh +
  econagg.dum +
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only + 
  s(mean.threat) + low.kap.sc + s(begyr) + us.mem

theta.formula.lih <- ~ s(begyr) + maxcap.cons + maxcap.liedh

gjrm.lih <- gjrm(list(uncond.formula.lih, depth.formula.lih,
                     eq.sigma, theta.formula.lih), 
                data = key.data,
                margins = c("probit", "BE"),
                Model = "B",
                BivD = "N"
) 
conv.check(gjrm.lih)
summary(gjrm.lih)




### fit a model with democratic proportion

# contrast these variables
ggplot(atop.milsup, aes(x = factor(maxcap.lied), y = prop.open)) +
  geom_boxplot() +
  geom_jitter(alpha = .85) +
  theme_bw()



# Set up unique dataframe
key.data.prop <- select(atop.milsup, atopid, latent.depth.mean, econagg.dum, uncond.milsup, 
                        fp.conc.index, num.mem, wartime, asymm, deep.alliance, non.maj.only,
                        low.kap.sc, begyr, post45, asymm.cap, mean.threat, 
                        dem.prop, joint.democ, avg.democ, 
                        prop.open, prop.cons) %>%
  drop_na()
key.data.prop$latent.depth.mean.rs <- (key.data.prop$latent.depth.mean + 1) / (1 + max(key.data.prop$latent.depth.mean) + .01)
summary(key.data.prop$latent.depth.mean.rs)

summary(key.data.prop$prop.open)

# glm model of unconditional military support
uncond.glm.prop <- glm(uncond.milsup ~ 
                         prop.open + prop.cons +
                         econagg.dum +
                         fp.conc.index + num.mem + wartime + asymm +
                         asymm.cap + non.maj.only + mean.threat + 
                         low.kap.sc + post45,
                       family = binomial(link = "probit"),
                       data = key.data.prop)
summary(uncond.glm.prop)


# glm model of economic issue linkages
linkage.glm.prop <- glm(econagg.dum ~ 
                          prop.open + prop.cons +  
                          fp.conc.index + num.mem + wartime + asymm +  
                          asymm.cap + non.maj.only + mean.threat +
                          low.kap.sc + post45,
                        family = binomial(link = "probit"),
                        data = key.data.prop)
summary(linkage.glm.prop)


# Use a beta regression with rescaled depth 
beta.reg.depth.prop <- betareg(latent.depth.mean.rs ~ 
                                 prop.open + prop.cons +
                                 econagg.dum +
                                 fp.conc.index + num.mem + wartime + asymm + 
                                 asymm.cap + non.maj.only + 
                                 mean.threat + low.kap.sc + post45, data = key.data.prop)
summary(beta.reg.depth.prop)




# set up model formulas 
uncond.formula.prop <- uncond.milsup ~ 
  prop.cons + prop.open +
  econagg.dum + 
  fp.conc.index + num.mem + wartime + asymm + 
  asymm.cap + non.maj.only +
  s(mean.threat) + low.kap.sc + s(begyr)


depth.formula.prop <- latent.depth.mean.rs ~ 
  prop.cons + prop.open + econagg.dum +
  fp.conc.index + num.mem + wartime + asymm + 
  asymm.cap + non.maj.only +
  s(mean.threat) + low.kap.sc + s(begyr)

theta.formula.prop <- ~ s(begyr) + prop.open + prop.cons 

# Same model: probit and beta margins 
joint.gjrm.prop  <- gjrm(list(uncond.formula.prop, depth.formula.prop,
                                      eq.sigma, theta.formula.prop), data = key.data.prop,
                                 margins = c("probit", "BE"),
                                 Model = "B",
                                 BivD =  "T" # better AIC than normal
                          )

# examine the results: 
conv.check(joint.gjrm.prop)
AIC(joint.gjrm.prop)
summary(joint.gjrm.prop)



### consider alternative meaures of democracy/competition

# Start with polity

uncond.formula.pol <- uncond.milsup ~ maxcap.cons + maxcap.rec + maxcap.comp +
  econagg.dum + 
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only + 
  s(mean.threat) + low.kap.sc + s(begyr)

depth.formula.pol <- latent.depth.mean.rs ~ maxcap.cons + maxcap.rec + maxcap.comp +
  econagg.dum +
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only + 
  s(mean.threat) + low.kap.sc + s(begyr)

# model the dependence between the error terms as a function of start year
theta.formula.pol <- ~ s(begyr) + maxcap.cons + maxcap.rec 
eq.sigma <- ~ 1

# Fit the model 
joint.gjrm.pol <- gjrm(list(uncond.formula.pol, depth.formula.pol,
                                 eq.sigma, theta.formula.pol), 
                            data = key.data,
                            margins = c("probit", "BE"),
                            Model = "B",
                            BivD = "N"
                  )
conv.check(joint.gjrm.pol)
summary(joint.gjrm.pol)
post.check(joint.gjrm.pol)


### Model with contestation and inclusiveness
# cuts sample to 1950
key.data.poly <- select(atop.milsup, atopid, latent.depth.mean, econagg.dum, uncond.milsup, 
                        fp.conc.index, num.mem, wartime, asymm, deep.alliance, non.maj.only,
                        low.kap.sc, begyr, post45, asymm.cap, mean.threat,
                        maxcap.cont.std, maxcap.inc.std, maxcap.cons, us.mem) %>%
  drop_na() %>%
  filter(maxcap.inc.std != 0) # zeros are NA
key.data.poly$latent.depth.mean.rs <- (key.data.poly$latent.depth.mean + 1) / (1 + max(key.data$latent.depth.mean) + .01)
summary(key.data.poly$latent.depth.mean.rs)
summary(key.data.poly$maxcap.cont.std)

uncond.formula.poly <- uncond.milsup ~ maxcap.cons + 
  maxcap.cont.std + maxcap.inc.std +
  econagg.dum + 
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + 
  mean.threat + low.kap.sc + begyr + us.mem

depth.formula.poly <- latent.depth.mean.rs ~ maxcap.cons + 
  maxcap.cont.std + maxcap.inc.std +
  econagg.dum +
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + 
  mean.threat + low.kap.sc + begyr + us.mem

# no theta eqn- too little data for convergence
# fit the model
gjrm.poly <- gjrm(list(uncond.formula.poly, depth.formula.poly), 
                data = key.data.poly,
                margins = c("probit", "BE"),
                Model = "B",
                BivD = "N"
) 
AIC(gjrm.poly)
conv.check(gjrm.poly)
summary(gjrm.poly)


# Analysis with Vdem polyarchy
key.data.poly.vdem <- select(atop.milsup, atopid, latent.depth.mean, econagg.dum, uncond.milsup, 
                        fp.conc.index, num.mem, wartime, asymm, deep.alliance, non.maj.only,
                        low.kap.sc, begyr, post45, asymm.cap, mean.threat,
                        maxcap.poly, maxcap.cons, us.mem) %>%
  drop_na()
key.data.poly.vdem$latent.depth.mean.rs <- (key.data.poly.vdem$latent.depth.mean + 1) / (1 + max(key.data$latent.depth.mean) + .01)
summary(key.data.poly.vdem$latent.depth.mean.rs)
summary(key.data.poly.vdem$maxcap.poly)

uncond.formula.poly.vdem <- uncond.milsup ~ maxcap.cons + maxcap.poly +
  econagg.dum + 
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only + 
  s(mean.threat) + low.kap.sc + s(begyr) 

depth.formula.poly.vdem <- latent.depth.mean.rs ~ maxcap.cons + maxcap.poly +
  econagg.dum +
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only + 
  s(mean.threat) + low.kap.sc + s(begyr)

theta.formula.poly.vdem <- ~ s(begyr) + maxcap.cons + maxcap.poly 

# fit the model
gjrm.poly.vdem <- gjrm(list(uncond.formula.poly.vdem, depth.formula.poly.vdem,
                       eq.sigma, theta.formula.poly.vdem), 
                  data = key.data.poly.vdem,
                  margins = c("probit", "BE"),
                  Model = "B",
                  BivD = "T"
) 
AIC(gjrm.poly.vdem)
conv.check(gjrm.poly.vdem)
summary(gjrm.poly.vdem)



### plot results


# Create a function to plot for all the models and variables

substance.plot <- function(model, label, destination){

  
# list of plots
depth.plots <- vector(mode = "list", length = length(models.comp))
uncond.plots <- vector(mode = "list", length = length(models.comp))
  
# for loop to plot  
for(i in 1:length(model)){

sim.data <- cbind.data.frame(
    x0 = rep(1, n = 2), # intercept
    maxcap.cons = rep(1, n = 2),
    par = c(min(model[[i]][["X1"]][, 3]), max(model[[i]][["X1"]][, 3])), # placeholder
    prop.cons = rep(1, n = 2), 
    maxcap.comp = rep(1, n = 2),
    econagg.dum = rep(0, n = 2),
    fp.conc.index = rep(0, n = 2), # no concessions
    num.mem = rep(2, n = 2), # bilateral
    wartime = rep(0, n = 2), # peacetime
    asymm = rep(0, n = 2), # symmetric obligations
    asymm.cap = rep(1, n = 2), # asymmetric cap
    non.maj.only = rep(0, n = 2),
    mean.threat = rep(median(key.data$mean.threat), n = 2),
    low.kap.sc = rep(median(key.data$low.kap.sc), n = 2),
    begyr = rep(median(key.data$begyr), n = 2),
    us.mem = rep(0, n = 2)
  )
  colnames(sim.data)[3] <- colnames(model[[i]][["X1"]])[3] # give proper name
  glimpse(sim.data)

# build out predictions for depth
pred.depth.mat <- predict(model[[i]], eq = 2,
                          type = "lpmatrix", 
                          newdata = sim.data)

# Create vectors of parameter replciates 
# similar to bootstrap with lm or rlm 
rmvn <- function(n,mu,sig) { ## MVN random deviates
  L <- mroot(sig);m <- ncol(L);
  t(mu + L%*%matrix(rnorm(m*n),m,n)) 
}

br <- rmvn(1000,coef(model[[i]]), model[[i]]$Vb) ## 1000 replicate param. vectors

dim(br[, (ncol(pred.depth.mat)+1):(ncol(pred.depth.mat) + ncol(pred.depth.mat))])
dim(t(pred.depth.mat))

# Calculate differences
sim.res <- data.frame(linkinv(br[, (ncol(pred.depth.mat)+1):(ncol(pred.depth.mat) + 
                                ncol(pred.depth.mat))] %*% t(pred.depth.mat)))
depth.diff <- rbind.data.frame(
  quantile(sim.res$X1, c(0.025, .975)),
  quantile(sim.res$X2, c(0.025, .975)),
  quantile(sim.res$X2 - sim.res$X1, c(0.025, .975))
)
colnames(depth.diff) <- c("lower", "upper")
depth.diff$scenario <- c("Low", "High",
                         "Difference")

# plot intervals
depth.intervals <- ggplot(depth.diff, aes(x = scenario, y = upper)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                width = .1, size = 1) +
  labs(y = "Predicted Treaty Depth",
       x = "Scenario") +
  ggtitle(label[[i]]) +
  theme_bw()
depth.intervals

# add to plot list
depth.plots[[i]] <- depth.intervals

# calculate variance from inverting link transformation 
res <- linkinv(br[, (ncol(pred.depth.mat)+1):(ncol(pred.depth.mat)+ ncol(pred.depth.mat))] %*% t(pred.depth.mat))
mean(res);var(res)



# repeat the process for unconditional military support
pred.uncond.mat <- predict(model[[i]], eq = 1,
                           type = "lpmatrix", 
                           newdata = sim.data)

dim(br[, 1:ncol(pred.uncond.mat)])
dim(t(pred.uncond.mat))

# Calculate differences
sim.res.uncond <- data.frame(pnorm(br[, 1:ncol(pred.uncond.mat)] %*% t(pred.uncond.mat)))
uncond.diff <- rbind.data.frame(
  quantile(sim.res.uncond$X1, c(0.025, .975)),
  quantile(sim.res.uncond$X2, c(0.025, .975)),
  quantile(sim.res.uncond$X2 - sim.res.uncond$X1, c(0.025, .975))
)
colnames(uncond.diff) <- c("lower", "upper")
uncond.diff$scenario <- c("Low", "High",
                         "Difference")
# plot intervals
uncond.intervals <- ggplot(uncond.diff, aes(x = scenario, y = upper)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                width = .1, size = 1) +
  labs(y = "Predicted Prob. of Unconditional Support",
       x = "Scenario") +
  ggtitle(label[[i]]) +
  theme_bw()
uncond.intervals

uncond.plots[[i]] <- uncond.intervals

} # end plot loop

# present results
# Combine plots
grid.arrange(depth.plots[[1]], depth.plots[[2]], depth.plots[[3]], 
             uncond.plots[[1]], uncond.plots[[2]], uncond.plots[[3]],
             nrow = 2)
results.all <- arrangeGrob(depth.plots[[1]], depth.plots[[2]], depth.plots[[3]],
                           uncond.plots[[1]], uncond.plots[[2]], uncond.plots[[3]],
                           nrow = 2)
ggsave(destination, results.all,
       height = 6, width = 8)

} # end function


# Inputs
models.comp = list(joint.gjrm.prop, joint.gjrm.pol, gjrm.poly.vdem)
labels.comp = c("Proportion Elec. Comp.", "Polity Elections.", "Polyarchy (VDem)")


# Apply the function 
substance.plot(model = models.comp, label = labels.comp,
               destination = "figures/results-other-democ.png")




