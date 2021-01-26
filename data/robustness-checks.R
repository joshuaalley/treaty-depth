# Joshua Alley
# Bivariate depth model robustness checks
# must run after joint-cred-analysis.R 




# add a control for US membership
# LIED and constraints dummy
beta.reg.depth.us <- betareg(latent.depth.mean.rs ~ 
                            maxcap.lied + maxcap.cons + 
                            econagg.dum + uncond.milsup +
                            fp.conc.index + num.mem + wartime + asymm + 
                            asymm.cap + non.maj.only + 
                            mean.threat + low.kap.sc + post45 + us.mem, data = key.data)
summary(beta.reg.depth.us)

# post 45 alliances only
beta.reg.depth.us45 <- betareg(latent.depth.mean.rs ~ 
                               maxcap.lied + maxcap.cons + 
                               econagg.dum + uncond.milsup +
                               fp.conc.index + num.mem + wartime + asymm + 
                               asymm.cap + non.maj.only + 
                               mean.threat + low.kap.sc + us.mem, 
                             data = key.data.p45)
summary(beta.reg.depth.us45)


# tabulate results
# Tabulate results
stargazer(list(beta.reg.depth.us, beta.reg.depth.us45),
          style = "all2",
          dep.var.labels=c("Latent Depth (rescaled)"),
          covariate.labels=c(
            "Lexical Index of Democracy","Executive Constraints", 
            "Economic Issue Linkage", "Unconditional Support",
            "Foreign Policy Concessions", "Number of Members",
            "Wartime Alliance", "Asymmetric Obligations",
            "Asymmetric Capability", "Non-Major Only",
            "Average Threat",
            "Foreign Policy Disagreement", "Post 1945",
            "US Membership"
          ),
          keep.stat = c("n","ll"), ci=TRUE, 
          star.char = c("", "", ""),
          notes = "95\\% Confidence Intervals in Parentheses.", 
          notes.append = FALSE,
          label = c("tab:us-reg")
)



### alternative measures of electoral competition
key.data.rc <- select(atop.milsup, atopid, latent.depth.mean, econagg.dum, uncond.milsup, 
                   fp.conc.index, num.mem, wartime, asymm, deep.alliance, non.maj.only,
                   low.kap.sc, begyr, post45, asymm.cap, mean.threat, maxcap.democ, maxcap.lied,
                   dem.prop, prop.cons, joint.democ, avg.democ, maxcap.poly, maxcap.cons, maxcap.rec,
                   us.mem, bilat) %>%
  drop_na()
key.data.rc$latent.depth.mean.rs <- (key.data.rc$latent.depth.mean + 1) / (1 + max(key.data.rc$latent.depth.mean) + .01)
summary(key.data.rc$latent.depth.mean.rs)


# vdem polyarchy
beta.reg.poly <- betareg(latent.depth.mean.rs ~ 
                            maxcap.poly + maxcap.cons + 
                            econagg.dum + uncond.milsup +
                            fp.conc.index + num.mem + wartime + asymm + 
                            asymm.cap + non.maj.only + 
                            mean.threat + low.kap.sc + post45, data = key.data.rc)
summary(beta.reg.poly)


# POLITY recruitment dummy
beta.reg.rec <- betareg(latent.depth.mean.rs ~ 
                           maxcap.rec + maxcap.cons + 
                           econagg.dum + uncond.milsup +
                           fp.conc.index + num.mem + wartime + asymm + 
                           asymm.cap + non.maj.only + 
                           mean.threat + low.kap.sc + post45, data = key.data.rc)
summary(beta.reg.rec)


# Democratic proportion
beta.reg.prop <- betareg(latent.depth.mean.rs ~ 
                           dem.prop + maxcap.cons +
                          econagg.dum + uncond.milsup +
                          fp.conc.index + num.mem + wartime + asymm + 
                          asymm.cap + non.maj.only + 
                          mean.threat + low.kap.sc + post45, data = key.data.rc)
summary(beta.reg.prop)


# tabulate results


### plot results


# Create a function to plot for all the models and variables

substance.plot <- function(model, label, destination){

  
# list of plots
depth.plots <- vector(mode = "list", length = length(model))
  
# for loop to plot  
for(i in 1:length(model)){

sim.data <- cbind.data.frame(
    x0 = rep(1, n = 2), # intercept
    par = c(min(model[[i]]$model[, 2]), max(model[[i]]$model[, 2])), # placeholder
    maxcap.cons = rep(1, n = 2),
    econagg.dum = rep(0, n = 2),
    uncond.milsup = rep(0, n = 2),
    fp.conc.index = rep(0, n = 2), # no concessions
    num.mem = rep(2, n = 2), # bilateral
    wartime = rep(0, n = 2), # peacetime
    asymm = rep(0, n = 2), # symmetric obligations
    asymm.cap = rep(1, n = 2), # asymmetric cap
    non.maj.only = rep(0, n = 2),
    mean.threat = rep(median(key.data$mean.threat), n = 2),
    low.kap.sc = rep(median(key.data$low.kap.sc), n = 2),
    post45 = rep(1, n = 2)
  )
  colnames(sim.data)[2] <- colnames(model[[i]]$model)[2] # give proper name
  glimpse(sim.data)

# Create vectors of parameter replciates 
# similar to bootstrap with lm or rlm 
n.bs <- 5000


bs.sim <- matrix(NA, nrow = n.bs, ncol = length(coef(model[[i]]))- 1)
formula = model[[i]]$formula
for (j in 1:n.bs) { # index bs by j: i is present in overall loop
  bs.samp <- sample(1:nrow(key.data.rc), nrow(key.data.rc), replace = T)
  bs.data <- key.data.rc[bs.samp, ]
  bs.est <- betareg(formula, data = bs.data)
  bs.sim[j, ] <- coef(bs.est)[1:length(coef(bs.est))-1] # removes shape parameter
}
# multiply bootstrapped params by simulated data
# also inverts the link function
sub.est <- linkinv(as.matrix(bs.sim) %*% t(as.matrix(sim.data)))
sub.est <- as.data.frame(sub.est)
# calculate differences
depth.diff <- rbind.data.frame(
    quantile(sub.est$V1, c(0.025, .975)),
    quantile(sub.est$V2, c(0.025, .975)),
    quantile(sub.est$V2 - sub.est$V1, c(0.025, .975))
  )
colnames(depth.diff) <- c("lower", "upper")
depth.diff$scenario <- c("Low", "High", "Difference")

# plot intervals
depth.intervals <- ggplot(depth.diff, aes(x = scenario, y = upper)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                width = .1, size = 1) +
  labs(y = "Predicted Treaty Depth",
       x = "Electoral Democracy Value") +
  ggtitle(label[[i]]) +
  theme_bw()
depth.intervals

# add to plot list
depth.plots[[i]] <- depth.intervals


} # end plot loop

# present results
# Combine plots
grid.arrange(depth.plots[[1]], depth.plots[[2]], depth.plots[[3]],
             nrow = 2)
results.all <- arrangeGrob(depth.plots[[1]], depth.plots[[2]], depth.plots[[3]],
                           nrow = 2)
ggsave(destination, results.all,
       height = 6, width = 8)

} # end function


# Inputs
model.list = list(beta.reg.poly, beta.reg.rec, beta.reg.prop)
#formula.list = list(beta.reg.poly$formula, beta.reg.rec$formula, beta.reg.prop$formula)
labels.comp = c("Polyarchy (VDem)", "Polity Elections.", "Proportion Electoral Democracy")


# Apply the function 
substance.plot(model = model.list, 
               label = labels.comp,
               destination = "appendix/results-other-democ.png")








### try skew-t and skew-cauchy models
# not converging 
# use a skew-t model
depth.reg.skewt <- selm(latent.depth.mean ~ 
                          maxcap.lied + maxcap.cons + 
                          econagg.dum + uncond.milsup +
                          fp.conc.index + num.mem + wartime + asymm + 
                          asymm.cap + non.maj.only + 
                          mean.threat + low.kap.sc + post45,
                        data = atop.milsup,
                        family = "ST",
                        opt.method = "CG",
                        param.type = "pseudo-CP")
summary(depth.reg.skewt, "pseudo-CP")
# plot(depth.reg.skewt, param.type = "pseudo-CP")

# also fit a skew-cauchy model
depth.reg.skewc <- selm(latent.depth.mean ~ 
                          maxcap.lied + maxcap.cons +
                          econagg.dum + uncond.milsup +
                          fp.conc.index + num.mem + wartime + asymm + 
                          asymm.cap + non.maj.only + 
                          mean.threat + low.kap.sc + post45,
                        data = atop.milsup,
                        family = "SC",
                        opt.method = "SANN",
                        param.type = "pseudo-CP")
summary(depth.reg.skewc, "pseudo-CP")
plot(depth.reg.skewc, param.type = "pseudo-CP")



# Summarize results in a table for the appendix
# OLS only- unreliable so leave out
stargazer(list(depth.reg.dem, depth.reg),
          style = "all2",
          dep.var.labels=c("Latent Depth"),
          covariate.labels=c(
            "Alliance Leader Polity",
            "Executive Constraints", "Electoral Competition",
            "Economic Issue Linkage", "Unconditional Support",
            "Foreign Policy Concessions", "Number of Members",
            "Wartime Alliance", "Asymmetric Obligations",
            "Asymmetric Capability", "Non-Major Only",
            "Average Threat",
            "Foreign Policy Disagreement", "Post 1945"
          ),
          keep.stat = c("n"), ci=TRUE, 
          star.char = c("", "", ""),
          notes = "95\\% Confidence Intervals in Parentheses.", 
          notes.append = FALSE,
          label = c("tab:depth-alt-models-ols")
)


# Skew-t and Skew-cauchy models
sum.skewt <- summary(depth.reg.skewt, "pseudo-CP")
sum.skewc <- summary(depth.reg.skewc, "pseudo-CP")

dim(sum.skewc@param.table)

skew.res.tab <- as.data.frame(rbind(sum.skewt@param.table[1:13, 1:2],
                                    sum.skewc@param.table[1:13, 1:2]
))

skew.res.tab$variable <- rep(c("(Intercept)",  
                               "Executive Constraints", 
                               "Lexical Index of Democracy",
                               "Economic Issue Linkage", 
                               "FP Concessions", "Number of Members", 
                               "Wartime Alliances", "Asymmetric Obligations",
                               "Asymmetric Capability", "Non-Major Only", 
                               "Mean Threat",  "FP Disagreement", "Post 1945"),
                             2)
skew.res.tab$model = c(rep("Skew T", n = 13), rep("Skew Cauchy", n = 13))


# Plot the coefficient estimates
ggplot(skew.res.tab, aes(x = estimate, y = variable)) +
  facet_wrap(~ model) +
  geom_vline(xintercept = 0) +
  geom_point(size = 2) +
  geom_errorbarh(aes(
    xmin = estimate - 2*std.err,
    xmax = estimate + 2*std.err
  ), size = 1, height = .25) +
  ggtitle("Skew Models of Latent Treaty Depth") +
  labs(x = "Estimate", y = "Variable") +
  theme_bw() 
ggsave("appendix/skew-model-res.png", height = 6, width = 8)

