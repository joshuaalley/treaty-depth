# Joshua Alley
# Texas A&M University 
# Analysis of the sources of alliance treaty depth


# focus on the posterior mean in this script: include uncertainty in later scripts


### Analysis focusing on alliances with military support
# Set up unique dataframe
key.data <- select(atop.milsup, atopid, latent.depth.mean, econagg.dum, uncond.milsup, 
                   fp.conc.index, num.mem, wartime, asymm, deep.alliance, non.maj.only,
                   low.kap.sc, begyr, post45, asymm.cap, mean.threat, maxcap.democ, maxcap.liedh,
                   dem.prop, joint.democ, avg.democ, maxcap.comp, maxcap.lied, maxcap.open,
                   maxcap.rec, maxcap.cons, maxcap.liedh, us.mem, bilat) %>%
  drop_na()
key.data$latent.depth.mean.rs <- (key.data$latent.depth.mean + 1) / (1 + max(key.data$latent.depth.mean) + .01)
summary(key.data$latent.depth.mean.rs)
table(key.data$maxcap.lied)

# Regression of depth on other alliance characteristics
depth.reg.dem <- lm(latent.depth.mean.rs ~ maxcap.democ +
                  econagg.dum + uncond.milsup +
                  fp.conc.index + num.mem + wartime + asymm + 
                  asymm.cap + non.maj.only +
                  mean.threat +
                  low.kap.sc + post45,
                data = key.data)
summary(depth.reg.dem)
plot(density(depth.reg.dem$residuals))

# Regression of depth on other democratic characteristics
depth.reg <- lm(latent.depth.mean ~
                  maxcap.lied + maxcap.cons + 
                  econagg.dum + uncond.milsup +
                  fp.conc.index + num.mem + wartime + asymm + 
                  asymm.cap + non.maj.only +
                  mean.threat +
                  low.kap.sc + post45,
                data = atop.milsup)
summary(depth.reg)

# robust regression
depth.rlm <- rlm(latent.depth.mean ~
                  maxcap.lied + maxcap.cons + 
                  econagg.dum + uncond.milsup +
                  fp.conc.index + num.mem + wartime + asymm + 
                  asymm.cap + non.maj.only +
                  mean.threat +
                  low.kap.sc + post45,
                data = atop.milsup)
summary(depth.rlm)


### beta regression
# alliance leader polity
depth.breg.dem <- betareg(latent.depth.mean.rs ~ maxcap.democ +
                           econagg.dum + uncond.milsup +
                           fp.conc.index + num.mem + wartime + asymm + 
                           asymm.cap + non.maj.only +
                           mean.threat +
                           low.kap.sc + post45,
                         data = key.data)
summary(depth.breg.dem)


# LIED and constraints dummy
beta.reg.depth <- betareg(latent.depth.mean.rs ~ 
                            maxcap.lied + maxcap.cons + 
                            econagg.dum + uncond.milsup +
                            fp.conc.index + num.mem + wartime + asymm + 
                            asymm.cap + non.maj.only + 
                            mean.threat + low.kap.sc + post45, data = key.data)
summary(beta.reg.depth)


# before and after 1945
# before 
key.data.b45 <- filter(key.data, post45 == 0)
beta.depth.b45 <- betareg(latent.depth.mean.rs ~ 
                            maxcap.lied + maxcap.cons + 
                            econagg.dum + uncond.milsup +
                            fp.conc.index + num.mem + wartime + asymm + 
                            asymm.cap + non.maj.only +
                            mean.threat + low.kap.sc, data = key.data.b45)
summary(beta.depth.b45)

# after 1945
key.data.p45 <- filter(key.data, post45 == 1)
beta.depth.p45 <- betareg(latent.depth.mean.rs ~ 
                            maxcap.lied + maxcap.cons +
                            econagg.dum + uncond.milsup +
                            fp.conc.index + num.mem + wartime + asymm + 
                            asymm.cap + 
                            mean.threat + low.kap.sc, data = key.data.p45)
summary(beta.depth.p45)



# substantive predictions using ggeffects package
margins.beta <- ggpredict(beta.reg.depth, terms = c("maxcap.lied", "maxcap.cons"),
                         interval = "confidence")
plot(margins.beta)
# full plot
plot.depth.sep <- ggplot(margins.beta, aes(x = factor(x), y = predicted,
                          color = factor(group))) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                size = 1, width = .05) +
  scale_color_manual(values = c(`0` = "gray66",
                                `1` = "gray0"),
                     labels = c("Absent", "Present")
  )+
  labs(x = "Electoral Democracy",
       y = "Predicted Treaty Depth",
       color = "Executive Constraints") +
  theme_bw()
plot.depth.sep
ggsave("figures/results-depth.png", height = 6, width = 8)

### calculate substantive effects
# set up new data
sim.data <- cbind.data.frame(
  x0 = rep(1, n = 14), # intercept
  maxcap.lied = rep(seq(0, 6, by = 1), 2),
  maxcap.cons = c(rep(0, 7), rep(1, 7)),
  econagg.dum = rep(0, n = 14),
  uncond.milsup = rep(0, n = 14),
  fp.conc.index = rep(0, n = 14), # no concessions
  num.mem = rep(2, n = 14), # bilateral
  wartime = rep(0, n = 14), # peacetime
  asymm = rep(0, n = 14), # symmetric obligations
  asymm.cap = rep(1, n = 14), # asymmetric cap
  non.maj.only = rep(0, n = 14),
  mean.threat = rep(median(key.data$mean.threat), n = 14),
  low.kap.sc = rep(median(key.data$low.kap.sc), n = 14),
  post45 = rep(1, n = 14)
)
glimpse(sim.data)

# Bootstrap
n.bs <- 5000
bs.sim <- matrix(NA, nrow = n.bs, ncol = length(coef(beta.reg.depth))- 1)
for (i in 1:n.bs) {
  bs.samp <- sample(1:nrow(key.data), nrow(key.data), replace = T)
  bs.data <- key.data[bs.samp, ]
  bs.est <- betareg(latent.depth.mean.rs ~ 
                      maxcap.lied + maxcap.cons +
                      econagg.dum + uncond.milsup +
                      fp.conc.index + num.mem + wartime + asymm + 
                      asymm.cap + non.maj.only + 
                      mean.threat + low.kap.sc + post45, data = bs.data)
  bs.sim[i, ] <- coef(bs.est)[1:length(coef(bs.est))-1] # removes shape parameter
}
# multiply bootstrapped params by simulated data
# also inverts the link function
sub.est <- linkinv(as.matrix(bs.sim) %*% t(as.matrix(sim.data)))
sub.est <- as.data.frame(sub.est)


# calculate predicted values
depth.pred <-  data.frame(apply(sub.est, 2, function(x)
                 quantile(x, c(0.025, .975))))
depth.pred <- cbind(t(depth.pred), select(sim.data,
                                          maxcap.lied, maxcap.cons))
colnames(depth.pred)[1:2] <- c("lower", "upper")
# plot
ggplot(depth.pred, aes(x = factor(maxcap.lied), y = upper,
                       color = factor(maxcap.cons))) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                width = .1, size = 2,
                position = position_dodge(.5)) +
  scale_color_manual(values = c(`0` = "gray66",
                                `1` = "gray0"),
                     labels = c("Absent", "Present")
  )+
  labs(y = "Predicted Latent Treaty Depth",
       x = "Lexical Index of Electoral Democracy",
       color = "Executive Constraints") +
  ggtitle("Predicted Treaty Depth")


# calculate differences
depth.diff <- rbind.data.frame(
  apply(sub.est[2:ncol(sub.est)], 2, function(x)
    quantile(x - sub.est$V1, c(0.025, .975))
))
depth.diff <- cbind(t(depth.diff), select(sim.data[2:nrow(sim.data), ],
                                        maxcap.lied, maxcap.cons))
colnames(depth.diff)[1:2] <- c("lower", "upper")

# plot results
ggplot(depth.diff, aes(x = factor(maxcap.lied), y = upper,
                                  color = factor(maxcap.cons))) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                width = .1, size = 2,
                position = position_dodge(.5)) +
  scale_color_manual(values = c(`0` = "gray66",
                                  `1` = "gray0"),
                     labels = c("Absent", "Present")
                       )+
  labs(y = "Predicted Difference in Treaty Depth",
       x = "Lexical Index Change from Zero",
       color = "Executive Constraints") +
  ggtitle("Predicted Difference in Treaty Depth")



# Tabulate results
stargazer(list(depth.breg.dem, beta.reg.depth,
               beta.depth.b45, beta.depth.p45),
          style = "all2",
          dep.var.labels=c("Latent Depth (rescaled)"),
          covariate.labels=c(
            "Alliance Leader Polity Score",
            "Lexical Index of Democracy","Executive Constraints", 
            "Economic Issue Linkage", "Unconditional Support",
            "Foreign Policy Concessions", "Number of Members",
            "Wartime Alliance", "Asymmetric Obligations",
            "Asymmetric Capability", "Non-Major Only",
            "Average Threat",
            "Foreign Policy Disagreement", "Post 1945"
          ),
          keep.stat = c("n","ll"), ci=TRUE, 
          star.char = c("", "", ""),
          notes = "95\\% Confidence Intervals in Parentheses.", 
          notes.append = FALSE,
          label = c("tab:reg-est")
)


