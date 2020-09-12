# Joshua Alley
# Texas A&M University
# Plot results for mediation analyses of latent depth


# Tabulate single equations
stargazer(list(beta.reg.depth.agg, beta.reg.depth,
               uncond.glm.dem, uncond.glm),
          style = "all2",
          dep.var.labels=c("Latent Depth (rescaled)","Unconditional Military Support"),
          covariate.labels=c(
            "Alliance Leader Polity Score",
            "Executive Constraints", " Lexical Index of Democracy",
            "Economic Issue Linkage",
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
          label = c("tab:separate-models")
)



### Lexical Index GJRM results 
summary.depth.gjrm <- summary(joint.gjrm)

# tabulate the results 
summary.depth.gjrm[["tableP1"]] # uncond milsup
summary.depth.gjrm[["tableP2"]] # depth 

# Combine all the results in a single table. 
# Start with unconditional table
uncond.tab <- as.data.frame(rbind(summary.depth.gjrm[["tableP1"]][, 1:2],
                                  summary.depth.gjrm[["tableNP1"]][, c(1, 3)]
))
uncond.tab$variable <- c("(Intercept)",  
                         "Executive Constraints", 
                         "Lexical Index of Democracy",
                         "Economic Issue Linkage", 
                         "FP Concessions", "Number of Members", 
                         "Wartime Alliances", "Asymmetric Obligations",
                         "Asymmetric Capability", "Non-Major Only", "FP Disagreement",
                         "s(Mean Threat)", "s(Start Year)")


# table for treaty depth
depth.tab <- as.data.frame(rbind(summary.depth.gjrm[["tableP2"]][, 1:2],
                                 summary.depth.gjrm[["tableNP2"]][, c(1, 3)]
))
depth.tab$variable <- c("(Intercept)",
                        "Executive Constraints",
                        "Lexical Index of Democracy",
                        "Economic Issue Linkage",
                        "FP Concessions", "Number of Members", 
                        "Wartime Alliances", "Asymmetric Obligations",
                        "Asymmetric Capability", "Non-Major Only", "FP Disagreement",
                        "s(Mean Threat)", "s(Start Year)")

joint.tab <- full_join(uncond.tab, depth.tab, by = "variable")
joint.tab <- as.data.frame(joint.tab[, c(3, 1, 2, 4, 5)])


# Tabulate all the equations together
# create a header
head.xtab <- list()
head.xtab$pos <- list(-1)
head.xtab$command <- paste0(paste0('& \\multicolumn{2}{c}{Uncond. Mil. Support} & \\multicolumn{2}{c}{Latent Depth}',
                                   collapse=''), '\\\\')



print(
  xtable(joint.tab, 
         caption = c("Results from a joint generalized regression model of treaty depth and unconditional military support in 
         offensive and defensive alliances from 1816 to 2007. 
                     All smoothed terms report the effective degrees of freedom and the chi-squared term. 
                     The unconditional military support model is a binomial GLM with a probit link function. 
                     The treaty depth model is a beta regression. 
                     I model the error correlation between the two processes with a T copula."),
         label = c("tab:gjrm-res"), auto = TRUE),
  add.to.row = head.xtab, 
  include.rownames = FALSE)


# substantive predictions from single-equation model
margins.beta <- ggeffect(beta.reg.depth, terms = c("maxcap.lied"),
                          interval = "confidence")
plot.depth.sep <- ggplot(margins.beta, aes(x = factor(x), y = predicted)) +
                   geom_point(size = 2) +
                   geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                               size = 1, width = .05) +
                  labs(x = "Competitive Elections",
                       y = "Predicted Treaty Depth") +
                   theme_bw()
plot.depth.sep


# marginal effects from uncond glm
margins.uncond <- ggeffect(uncond.glm, terms = c("maxcap.lied"),
                           interval = "confidence")
plot.uncond.sep <- ggplot(margins.uncond, aes(x = factor(x), y = predicted)) +
                    geom_point(size = 2) +
                    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                               size = 1, width = .05) +
                    labs(x = "Lexical Index of Democracy",
                    y = "Predicted Probability of Unconditional Support") +
                    theme_bw()
plot.uncond.sep


### Plot predictions with split democracy components

# set up new data
sim.data <- cbind.data.frame(
   x0 = rep(1, n = 7), # intercept
   maxcap.lied = seq(0, 6, by = 1),
   maxcap.cons = rep(1, n = 7),
   econagg.dum = rep(0, n = 7),
   fp.conc.index = rep(0, n = 7), # no concessions
   num.mem = rep(2, n = 7), # bilateral
   wartime = rep(0, n = 7), # peacetime
   asymm = rep(0, n = 7), # symmetric obligations
   asymm.cap = rep(1, n = 7), # asymmetric cap
   non.maj.only = rep(0, n = 7),
   mean.threat = rep(median(key.data$mean.threat), n = 7),
   low.kap.sc = rep(median(key.data$low.kap.sc), n = 7),
   begyr = rep(median(key.data$begyr), n = 7)
)
glimpse(sim.data)

# build out predictions for depth
pred.depth.mat <- predict(joint.gjrm, eq = 2,
                           type = "lpmatrix", 
                           newdata = sim.data)
pred.depth.mat %*% coef(joint.gjrm)[30:58]

# Create vectors of parameter replciates 
# similar to bootstrap with lm or rlm 
rmvn <- function(n,mu,sig) { ## MVN random deviates
  L <- mroot(sig);m <- ncol(L);
  t(mu + L%*%matrix(rnorm(m*n),m,n)) 
}

br <- rmvn(1000,coef(joint.gjrm), joint.gjrm$Vb) ## 1000 replicate param. vectors

dim(br[, 30:58])
dim(t(pred.depth.mat))

# Calculate differences
sim.res <- data.frame(linkinv(br[, 30:58] %*% t(pred.depth.mat)))
depth.diff <- rbind.data.frame(
               quantile(sim.res$X2 - sim.res$X1, c(0.025, .975)),
               quantile(sim.res$X3 - sim.res$X1, c(0.025, .975)),
               quantile(sim.res$X4 - sim.res$X1, c(0.025, .975)),
               quantile(sim.res$X5 - sim.res$X1, c(0.025, .975)),
               quantile(sim.res$X6 - sim.res$X1, c(0.025, .975)),
               quantile(sim.res$X7 - sim.res$X1, c(0.025, .975))
          )
colnames(depth.diff) <- c("lower", "upper")
depth.diff$scenario <- factor(seq(from = 1, to = 6, by = 1))

# plot intervals
depth.intervals <- ggplot(depth.diff, aes(x = scenario, y = upper)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                width = .1, size = 1) +
  labs(y = "Predicted Difference in Treaty Depth",
       x = "Lexical Index Change from Zero") +
  theme_bw()
depth.intervals


# calculate variance from inverting link transformation 
res <- linkinv(br[, 30:58] %*% t(pred.depth.mat))
mean(res);var(res)



# repeat the process for unconditional military support
pred.uncond.mat <- predict(joint.gjrm, eq = 1,
                            type = "lpmatrix", 
                            newdata = sim.data)

dim(br[, 1:29])
dim(t(pred.uncond.mat))

# Calculate differences
sim.res.uncond <- data.frame(pnorm(br[, 1:29] %*% t(pred.uncond.mat)))
uncond.diff <- rbind.data.frame(
  quantile(sim.res.uncond$X2 - sim.res.uncond$X1, c(0.025, .975)),
  quantile(sim.res.uncond$X3 - sim.res.uncond$X1, c(0.025, .975)),
  quantile(sim.res.uncond$X4 - sim.res.uncond$X1, c(0.025, .975)),
  quantile(sim.res.uncond$X5 - sim.res.uncond$X1, c(0.025, .975)),
  quantile(sim.res.uncond$X6 - sim.res.uncond$X1, c(0.025, .975)),
  quantile(sim.res.uncond$X7 - sim.res.uncond$X1, c(0.025, .975))
)
colnames(uncond.diff) <- c("lower", "upper")
uncond.diff$scenario <- factor(seq(from = 1, to = 6, by = 1))

# plot intervals
uncond.intervals <- ggplot(uncond.diff, aes(x = scenario, y = upper)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                width = .1, size = 1) +
  labs(y = "Predicted Difference in Pr(Uncond. Support)",
       x = "Lexical Index Change from Zero") +
  theme_bw()
uncond.intervals


# Combine plots
grid.arrange(depth.intervals, uncond.intervals,
             nrow = 2)
results.democ <- arrangeGrob(depth.intervals, uncond.intervals,
                             nrow = 2)
ggsave("figures/results-diff.png", results.democ, 
       height = 6, width = 8) #save file



# Predicted error correlations

# table of results
error.res <- as.data.frame(rbind(summary.depth.gjrm[["tableP4"]],
                        summary.depth.gjrm[["tableNP4"]])
                        ) # depth 
rownames(error.res) <- c("Intercept", 
                        "Executive Constraints", "Electoral Democracy",
                        "Number of Members",
                        "s(Start Year)")
xtable(error.res,
       caption = c("Error term correlation equation estimates from a joint generalized regression model of treaty depth and unconditional military support. 
                    Estimates are on the scale of $\\theta$, which is then converted into a Kendall's $\\tau$ correlation coefficient. 
                    "),
       label = c("tab:error-res"), auto = TRUE)


# Look at differences in tau
# set-up dataframe
tau.data <- cbind.data.frame(select(key.data, atopid, begyr,
                                    maxcap.lied, maxcap.cons), joint.gjrm$tau)
colnames(tau.data) <- c("atopid", "begyr",
                        "maxcap.lied", "maxcap.cons", "tau")
summary(tau.data$tau)


# plot tau against start year 
plot.tau.year <- ggplot(tau.data, aes(x = begyr, y = tau)) +
                    geom_point() +
                    labs(x = "Start Year", y = expression(hat(tau))) +
                    ggtitle("Start Year: Predicted Error Correlations") +
                    theme_bw()
plot.tau.year


# look at differences by elections
cor.test(tau.data$tau, tau.data$maxcap.lied)

plot.tau.elec <- ggplot(tau.data, aes(x = factor(maxcap.lied), y = tau)) +
                  geom_boxplot() +
                  geom_jitter() +
                  labs(x = "Electoral Democracy", y = expression(hat(tau))) +
                  ggtitle("Elections: Predicted Error Correlations") +
                   theme_bw()
plot.tau.elec


# differences in error correlations by executive constraints
plot.tau.cons <- ggplot(tau.data, aes(x = factor(maxcap.cons), y = tau)) +
  geom_boxplot() +
  geom_jitter() +
  labs(x = "Executive Constraints", y = expression(hat(tau))) +
  ggtitle("Executive Constraints: Predicted Error Correlations") +
  theme_bw()
plot.tau.cons







### Results for the Appendix ###



### tabulate results with US member controls and and LIED high dummy
varnames.par <- c("(Intercept)", 
                  "Executive Constraints",
                  "Electoral Democracy", 
                  "Economic Issue Linkage", 
                  "FP Concessions", "Number of Members", 
                  "Wartime Alliances", "Asymmetric Obligations",
                  "Asymmetric Capability", "Non-Major Only", "FP Disagreement",
                  "US Member")
uncond.tab.us <- summary(gjrm.us)[["tableP1"]][, 1:2]
depth.tab.us <- summary(gjrm.us)[["tableP2"]][, 1:2]
uncond.tab.lih <- summary(gjrm.lih)[["tableP1"]][, 1:2]
depth.tab.lih <- summary(gjrm.lih)[["tableP2"]][, 1:2]


rc.list <- list(uncond.tab.us, depth.tab.us, uncond.tab.lih, depth.tab.lih)
rc.list <- lapply(rc.list, function(x) 
          x = data.frame(x))
rc.list <- lapply(rc.list, function(x){ 
  x$variable <- varnames.par; return(x)})

# create a header
head.xtab <- list()
head.xtab$pos <- list(-1)
head.xtab$command <- paste0(paste0('& \\multicolumn{2}{c}{Uncond. Mil. Support} & \\multicolumn{2}{c}{Latent Depth}',
                                   collapse=''), '\\\\')

# tabulate US results
joint.tab.us <- full_join(rc.list[[1]], rc.list[[2]], by = "variable")
joint.tab.us <- as.data.frame(joint.tab.us[, c(3, 1, 2, 4, 5)])

print(
  xtable(joint.tab.us, 
         caption = c("Results from a joint generalized regression model of treaty depth and unconditional military support. 
                     This model includes a dummy variable to control for US alliance membership.
                     All smoothed terms report the effective degrees of freedom and the chi-squared term. 
                     The unconditional military support model is a binomial GLM with a probit link function. 
                     The treaty depth model is a beta regression. 
                     I model the error correlation between the two processes with a T copula."),
         label = c("tab:gjrm-res-us"), auto = TRUE),
  add.to.row = head.xtab, 
  include.rownames = FALSE)

# lexical index dummy
joint.tab.lih <- full_join(rc.list[[3]], rc.list[[4]], by = "variable")
joint.tab.lih <- as.data.frame(joint.tab.lih[, c(3, 1, 2, 4, 5)])

print(
  xtable(joint.tab.lih,  
         caption = c("Results from a joint generalized regression model of treaty depth and unconditional military support. 
                     This model replaces the ordinal lexical index of democracy with a dummy variable for states with a LIED score of four or higher.
                     All smoothed terms report the effective degrees of freedom and the chi-squared term. 
                     The unconditional military support model is a binomial GLM with a probit link function. 
                     The treaty depth model is a beta regression. 
                     I model the error correlation between the two processes with a T copula."),
         label = c("tab:gjrm-res-lih"), auto = TRUE),
  add.to.row = head.xtab, 
  include.rownames = FALSE)



### Polity Scores
# tabulate the results:
summary.depth.agg <- summary(joint.gjrm.agg)


# Combine all the results in a single table. 
# Start with unconditional table
uncond.tab.agg <- as.data.frame(rbind(summary.depth.agg[["tableP1"]][, 1:2],
                                       summary.depth.agg[["tableNP1"]][, c(1, 3)]
))
uncond.tab.agg$variable <- c("(Intercept)", 
                              "Most Capable Member Polity", 
                              "Economic Issue Linkage", 
                              "FP Concessions", "Number of Members", 
                              "Wartime Alliances", "Asymmetric Obligations",
                              "Asymmetric Capability", "Non-Major Only", "FP Disagreement",
                              "s(Mean Threat)", "s(Start Year)")


# table for treaty depth
depth.tab.agg <- as.data.frame(rbind(summary.depth.agg[["tableP2"]][, 1:2],
                                      summary.depth.agg[["tableNP2"]][, c(1, 3)]
))
depth.tab.agg$variable <- c("(Intercept)", 
                             "Most Capable Member Polity", 
                             "Economic Issue Linkage", 
                             "FP Concessions", "Number of Members", 
                             "Wartime Alliances", "Asymmetric Obligations",
                             "Asymmetric Capability", "Non-Major Only", "FP Disagreement",
                             "s(Mean Threat)", "s(Start Year)")

joint.tab.agg <- full_join(uncond.tab.agg, depth.tab.agg, by = "variable")
joint.tab.agg <- as.data.frame(joint.tab.agg[, c(3, 1, 2, 4, 5)])


# Tabulate all the equations together

print(
  xtable(joint.tab.agg, 
         caption = c("Results from a joint generalized regression model of treaty depth and unconditional military support. 
                     All smoothed terms report the effective degrees of freedom and the chi-squared term. 
                     The unconditional military support model is a binomial GLM with a probit link function. 
                     The treaty depth model is a beta regression. 
                     I model the error correlation between the two processes with a T copula."),
         label = c("tab:gjrm-res-agg"), auto = TRUE),
  add.to.row = head.xtab, 
  include.rownames = FALSE)



### Proportions
# tabulate the results:
summary.depth.prop <- summary(joint.gjrm.prop)


# Combine all the results in a single table. 
# Start with unconditional table
uncond.tab.prop <- as.data.frame(rbind(summary.depth.prop[["tableP1"]][, 1:2],
                                        summary.depth.prop[["tableNP1"]][, c(1, 3)]
))
uncond.tab.prop$variable <- c("(Intercept)", 
                              "Proportion Executive Constraints",
                              "Proportion Competitive Elections", 
                              "Economic Issue Linkage", 
                              "FP Concessions", "Number of Members", 
                              "Wartime Alliances", "Asymmetric Obligations",
                              "Asymmetric Capability", "Non-Major Only", "FP Disagreement",
                              "s(Mean Threat)", "s(Start Year)")


# table for treaty depth
depth.tab.prop <- as.data.frame(rbind(summary.depth.prop[["tableP2"]][, 1:2],
                                       summary.depth.prop[["tableNP2"]][, c(1, 3)]
))
depth.tab.prop$variable <- c("(Intercept)", 
                             "Proportion Executive Constraints",
                              "Proportion Competitive Elections", 
                              "Economic Issue Linkage", 
                              "FP Concessions", "Number of Members", 
                              "Wartime Alliances", "Asymmetric Obligations",
                              "Asymmetric Capability", "Non-Major Only", "FP Disagreement",
                              "s(Mean Threat)", "s(Start Year)")

joint.tab.prop <- full_join(uncond.tab.prop, depth.tab.prop, by = "variable")
joint.tab.prop <- as.data.frame(joint.tab.prop[, c(3, 1, 2, 4, 5)])


# Tabulate all the equations together

print(
  xtable(joint.tab.prop, 
         caption = c("Results from joint generalized regression model of treaty depth and unconditional military support. 
                     All smoothed terms report the effective degrees of freedom and the chi-squared term. 
                     The unconditional military support model is a binomial GLM with a probit link function. 
                     The treaty depth model is a beta regression. 
                     I model the error correlation between the two processes with a normal copula."),
         label = c("tab:gjrm-res-prop"), auto = TRUE),
  add.to.row = head.xtab, 
  include.rownames = FALSE)






### summarize results from a trivariate model

# new data for simulation
sim.data.tri <- cbind.data.frame(
  x0 = rep(1, n = 7), # intercept
  maxcap.lied = seq(0, 6, by = 1),
  maxcap.cons = rep(1, n = 7),
  fp.conc.index = rep(0, n = 7), # no concessions
  num.mem = rep(2, n = 7), # bilateral
  wartime = rep(0, n = 7), # peacetime
  asymm = rep(0, n = 7), # symmetric obligations
  asymm.cap = rep(1, n = 7), # asymmetric cap
  non.maj.only = rep(0, n = 7),
  mean.threat = rep(median(key.data$mean.threat), n = 7),
  low.kap.sc = rep(median(key.data$low.kap.sc), n = 7),
  post45 = rep(1, n = 7)
)

# start with treaty depth
pred.depth.tri <- predict(joint.gjrm.tri, eq = 2,
                            type = "response", 
                            se.fit = TRUE,
                            newdata = sim.data.tri)

pred.depth.tri <- cbind.data.frame(pred.depth.tri$fit, 
                                     pred.depth.tri$se.fit,
                                     sim.data.tri$maxcap.lied)
colnames(pred.depth.tri) <- c("pred", "se", "maxcap.lied")

# calculate lower and upper bounds of 95% CI
pred.depth.tri$lower <- pred.depth.tri$pred - 2*pred.depth.tri$se
pred.depth.tri$upper <- pred.depth.tri$pred + 2*pred.depth.tri$se

# get predictions and unc back on response scale
pred.depth.tri$response <- linkinv(pred.depth.tri$pred)
pred.depth.tri$lower.res <- linkinv(pred.depth.tri$lower)
pred.depth.tri$upper.res <- linkinv(pred.depth.tri$upper)


depth.plot.tri <- ggplot(pred.depth.tri, aes(x = maxcap.lied, y = response)) +
                     geom_point(size = 2) +
                     geom_errorbar(aes(ymin = lower.res,
                      ymax = upper.res),
                      width = .1, size = 1) +
                     labs(y = "",
                       x = "Alliance Leader Electoral Democracy") +
                     ggtitle("Deep Alliance") +
                     theme_bw()
depth.plot.tri


# Now for unconditional military support
pred.uncond.tri <- predict(joint.gjrm.tri, eq = 1,
                          type = "response", 
                          se.fit = TRUE,
                          newdata = sim.data.tri)

pred.uncond.tri <- cbind.data.frame(pred.uncond.tri$fit, 
                                   pred.uncond.tri$se.fit,
                                   sim.data.tri$maxcap.lied)
colnames(pred.uncond.tri) <- c("pred", "se", "maxcap.lied")

# calculate lower and upper bounds of 95% CI
pred.uncond.tri$lower <- pred.uncond.tri$pred - 2*pred.uncond.tri$se
pred.uncond.tri$upper <- pred.uncond.tri$pred + 2*pred.uncond.tri$se

# get predictions and unc back on response scale
pred.uncond.tri$response <- linkinv(pred.uncond.tri$pred)
pred.uncond.tri$lower.res <- linkinv(pred.uncond.tri$lower)
pred.uncond.tri$upper.res <- linkinv(pred.uncond.tri$upper)


uncond.plot.tri <- ggplot(pred.uncond.tri, aes(x = maxcap.lied, y = response)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower.res,
                    ymax = upper.res),
                width = .1, size = 1) +
  labs(y = "",
       x = "Alliance Leader Electoral Democracy") +
  ggtitle("Uncond. Support") +
  theme_bw()
uncond.plot.tri


# Last, issue linkages
pred.issue.tri <- predict(joint.gjrm.tri, eq = 3,
                           type = "response", 
                           se.fit = TRUE,
                           newdata = sim.data.tri)

pred.issue.tri <- cbind.data.frame(pred.issue.tri$fit, 
                                    pred.issue.tri$se.fit,
                                    sim.data.tri$maxcap.lied)
colnames(pred.issue.tri) <- c("pred", "se", "maxcap.lied")

# calculate lower and upper bounds of 95% CI
pred.issue.tri$lower <- pred.issue.tri$pred - 2*pred.issue.tri$se
pred.issue.tri$upper <- pred.issue.tri$pred + 2*pred.issue.tri$se

# get predictions and unc back on response scale
pred.issue.tri$response <- linkinv(pred.issue.tri$pred)
pred.issue.tri$lower.res <- linkinv(pred.issue.tri$lower)
pred.issue.tri$upper.res <- linkinv(pred.issue.tri$upper)


issue.plot.tri <- ggplot(pred.issue.tri, aes(x = maxcap.lied, y = response)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower.res,
                    ymax = upper.res),
                width = .1, size = 1) +
  labs(y = "",
       x = "Alliance Leader Electoral Democracy") +
  ggtitle("Issue Linkage") +
  theme_bw()
issue.plot.tri

# joint.plot
grid.arrange(depth.plot.tri, uncond.plot.tri, issue.plot.tri,
             ncol = 3)
results.tri <- arrangeGrob(depth.plot.tri, uncond.plot.tri, issue.plot.tri,
                             ncol = 3)
ggsave("appendix/pred-trivar.png", results.tri,
       height = 6, width = 8) #save file
