# Joshua Alley
# Texas A&M University
# Plot results for mediation analyses of latent depth


# Tabulate single equations
stargazer(list(beta.reg.depth, uncond.glm),
          style = "all2",
          dep.var.labels=c("Latent Depth (rescaled)","Unconditional Military Support"),
          covariate.labels=c(
            "Alliance Leader Polity Score",
            "Foreign Policy Concessions", "Number of Members",
            "Wartime Alliance", "Asymmetric Obligations",
            "Asymmetric Capability", "Non-Major Only",
            "Average Threat",
            "Foreign Policy Disagreement", "Start Year"
          ),
          keep.stat = c("n","ll"), ci=TRUE, 
          star.char = c("", "", ""),
          notes = "95\\% Confidence Intervals in Parentheses.", 
          notes.append = FALSE,
          label = c("tab:separate-models")
)



# results with average democracy in alliance
# tabulate the results 
summary.depth.gjrm <- summary(joint.gjrm)
summary.depth.gjrm[["tableP1"]] # uncond milsup
summary.depth.gjrm[["tableP2"]] # depth 

# Combine all the results in a single table. 
# Start with unconditional table
uncond.tab <- as.data.frame(rbind(summary.depth.gjrm[["tableP1"]][, 1:2],
                                  summary.depth.gjrm[["tableNP1"]][, c(1, 3)]
))
uncond.tab$variable <- c("(Intercept)",  "Alliance Leader Polity", "Economic Issue Linkage", 
                         "FP Concessions", "Number of Members", 
                         "Wartime Alliances", "Asymmetric Obligations",
                         "Asymmetric Capability", "Non-Major Only", "FP Disagreement",
                         "s(Mean Threat)", "s(Start Year)")


# table for treaty depth
depth.tab <- as.data.frame(rbind(summary.depth.gjrm[["tableP2"]][, 1:2],
                                 summary.depth.gjrm[["tableNP2"]][, c(1, 3)]
))
depth.tab$variable <- c("(Intercept)", "Alliance Leader POLITY", "Economic Issue Linkage",
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
         caption = c("Results from joint generalized regression model of treaty depth and unconditional military support. 
                     All smoothed terms report the effective degrees of freedom and the chi-squared term. 
                     The unconditional military support model is a binomial GLM with a probit link function. 
                     The treaty depth model is a beta regression. 
                     I model the error correlation between the two processes with a T copula."),
         label = c("tab:gjrm-res"), auto = TRUE),
  add.to.row = head.xtab, 
  include.rownames = FALSE)



### Predictions from model with polity of most capable state

sim.data.mcap <- cbind.data.frame(
  x0 = rep(1, n = 21), # intercept
  maxcap.democ = seq(-10, 10, by = 1),
  econagg.dum = rep(0, n = 21),
  fp.conc.index = rep(0, n = 21), # no concessions
  num.mem = rep(2, n = 21), # bilateral
  wartime = rep(0, n = 21), # peacetime
  asymm = rep(0, n = 21), # symmetric obligations
  asymm.cap = rep(1, n = 21), # asymmetric cap
  non.maj.only = rep(0, n = 21),
  mean.threat = rep(median(key.data$mean.threat), n = 21),
  low.kap.sc = rep(median(key.data$low.kap.sc), n = 21),
  begyr = rep(median(key.data$begyr), n = 21)
)
glimpse(sim.data.mcap)


# substantive predictions from single-equation model
margins.beta <- ggeffect(beta.reg.depth, terms = c("maxcap.democ"),
                          interval = "confidence")
plot.depth.sep <- ggplot(margins.beta, aes(x = x, y = predicted)) +
                   geom_line() +
                   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                               alpha = .1) +
                  labs(x = "Alliance Leader Democracy",
                       y = "Predicted Marginal Effect on Treaty Depth") +
                   theme_bw()
plot.depth.sep


# marginal effects from uncond glm
margins.uncond <- ggeffect(uncond.glm, terms = c("maxcap.democ"),
                           interval = "confidence")
plot.uncond.sep <- ggplot(margins.uncond, aes(x = x, y = predicted)) +
                    geom_line() +
                    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                                alpha = .1) +
                    labs(x = "Alliance Leader Democracy",
                    y = "Predicted Marginal Effect on Pr(Unconditional Support)") +
                    theme_bw()
plot.uncond.sep

# combine plots and export
grid.arrange(plot.uncond.sep, plot.depth.sep,
             ncol = 2)
results.democ <- arrangeGrob(plot.uncond.sep, plot.depth.sep,
                             ncol = 2)
ggsave("figures/results-democ-max.png", results.democ, 
       height = 6, width = 8) #save file


# build out predictions for depth from the joint model ()
pred.depth.mcap <- predict(joint.gjrm, eq = 2,
                            type = "response", 
                            se.fit = TRUE,
                            newdata = sim.data.mcap)

pred.depth.mcap <- cbind.data.frame(pred.depth.mcap$fit, 
                                     pred.depth.mcap$se.fit,
                                     sim.data.mcap$maxcap.democ)
colnames(pred.depth.mcap) <- c("pred", "se", "maxcap.democ")

# calculate lower and upper bounds of 95% CI
pred.depth.mcap$lower <- pred.depth.mcap$pred - 2*pred.depth.mcap$se
pred.depth.mcap$upper <- pred.depth.mcap$pred + 2*pred.depth.mcap$se

# get predictions and unc back on response scale
pred.depth.mcap$response <- linkinv(pred.depth.mcap$pred)
pred.depth.mcap$lower.res <- linkinv(pred.depth.mcap$lower)
pred.depth.mcap$upper.res <- linkinv(pred.depth.mcap$upper)


# plot predicted depth values
plot.depth <- ggplot(pred.depth.mcap, aes(x = maxcap.democ, y = response)) +
                geom_point(size = 2) +
                geom_errorbar(aes(ymin = lower.res,
                    ymax = upper.res),
                    width = .1, size = 1) +
               labs(y = "Rescaled Latent Depth",
                    x = "Alliance Leader Polity") +
               theme_bw()
plot.depth


# Plot for unconditional military support 
pred.uncond.mcap <- predict(joint.gjrm, eq = 1,
                           type = "response", 
                           se.fit = TRUE,
                           newdata = sim.data.mcap)

pred.uncond.mcap <- cbind.data.frame(pred.uncond.mcap$fit, 
                                    pred.uncond.mcap$se.fit,
                                    sim.data.mcap$maxcap.democ)
colnames(pred.uncond.mcap) <- c("pred", "se", "maxcap.democ")

# calculate lower and upper bounds of 95% CI
pred.uncond.mcap$lower <- pred.uncond.mcap$pred - 2*pred.uncond.mcap$se
pred.uncond.mcap$upper <- pred.uncond.mcap$pred + 2*pred.uncond.mcap$se

# get predictions and unc back on response scale
pred.uncond.mcap$response <- linkinv(pred.uncond.mcap$pred)
pred.uncond.mcap$lower.res <- linkinv(pred.uncond.mcap$lower)
pred.uncond.mcap$upper.res <- linkinv(pred.uncond.mcap$upper)


# plot predicted probability of unconditional military support
plot.uncond <- ggplot(pred.uncond.mcap, aes(x = maxcap.democ, y = response)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower.res,
                    ymax = upper.res),
                width = .1, size = 1) +
  labs(y = "Predicted Pr(Unconditional Military Support)",
       x = "Alliance Leader Polity") +
  theme_bw()
plot.uncond






### Tablulate split models




# substantive predictions from single-equation model
margins.beta.split <- ggeffect(beta.reg.depth.split, 
                         terms = ~ maxcap.rec + maxcap.comp + maxcap.cons)
margins.beta.split
ggplot(margins.beta.split, aes(x = x, y = predicted, colour = group)) +
  stat_smooth(method = "lm", se = FALSE) +
  facet_wrap(~facet, ncol = 2) +
  labs(
    y = get_y_title(margins.beta.split),
    x = get_x_title(margins.beta.split),
    colour = get_legend_title(margins.beta.split)
  )

# marginal effects of unconditional military support
margins.uncond.split <- ggpredict(uncond.glm.split, 
                               terms = ~ maxcap.rec + maxcap.comp + maxcap.cons)
margins.uncond.split
ggplot(margins.uncond.split, aes(x = x, y = predicted, colour = group)) +
  stat_smooth(method = "lm", se = FALSE) +
  facet_wrap(~facet, ncol = 2) +
  labs(
    y = get_y_title(margins.beta.split),
    x = get_x_title(margins.beta.split),
    colour = get_legend_title(margins.beta.split)
  )



# tabulate the results 
summary.depth.split <- summary(joint.gjrm.split)


# Combine all the results in a single table. 
# Start with unconditional table
uncond.tab.split <- as.data.frame(rbind(summary.depth.split[["tableP1"]][, 1:2],
                                  summary.depth.split[["tableNP1"]][, c(1, 3)]
))
uncond.tab.split$variable <- c("(Intercept)", 
                         "Competitive Elections", "Political Competition",
                         "Executive Constraints",
                         "Economic Issue Linkage", 
                         "FP Concessions", "Number of Members", 
                         "Wartime Alliances", "Asymmetric Obligations",
                         "Asymmetric Capability", "Non-Major Only", "FP Disagreement",
                         "s(Mean Threat)", "s(Start Year)")


# table for treaty depth
depth.tab.split <- as.data.frame(rbind(summary.depth.split[["tableP2"]][, 1:2],
                                 summary.depth.split[["tableNP2"]][, c(1, 3)]
))
depth.tab.split$variable <- c("(Intercept)", 
                        "Competitive Elections", "Political Competition",
                        "Executive Constraints",
                        "Economic Issue Linkage", 
                        "FP Concessions", "Number of Members", 
                        "Wartime Alliances", "Asymmetric Obligations",
                        "Asymmetric Capability", "Non-Major Only", "FP Disagreement",
                        "s(Mean Threat)", "s(Start Year)")

joint.tab.split <- full_join(uncond.tab.split, depth.tab.split, by = "variable")
joint.tab.split <- as.data.frame(joint.tab.split[, c(3, 1, 2, 4, 5)])


# Tabulate all the equations together
# create a header
head.xtab <- list()
head.xtab$pos <- list(-1)
head.xtab$command <- paste0(paste0('& \\multicolumn{2}{c}{Uncond. Mil. Support} & \\multicolumn{2}{c}{Latent Depth}',
                                   collapse=''), '\\\\')



print(
  xtable(joint.tab.split, 
         caption = c("Results from joint generalized regression model of treaty depth and unconditional military support. 
                     All smoothed terms report the effective degrees of freedom and the chi-squared term. 
                     The unconditional military support model is a binomial GLM with a probit link function. 
                     The treaty depth model is a beta regression. 
                     I model the error correlation between the two processes with a T copula."),
         label = c("tab:gjrm-res-split"), auto = TRUE),
  add.to.row = head.xtab, 
  include.rownames = FALSE)



### Plot predictions with split POLITY components
# proportion of states with maximal democ

# set up new data
sim.data <- cbind.data.frame(
   x0 = rep(1, n = 5), # intercept
   maxcap.rec = c(0, 1, 1, 1, 1),
   maxcap.comp = c(0, 0, 1, 0, 1),
   maxcap.cons = c(0, 0, 0, 1, 1),
   econagg.dum = rep(0, n = 5),
   fp.conc.index = rep(0, n = 5), # no concessions
   num.mem = rep(2, n = 5), # bilateral
   wartime = rep(0, n = 5), # peacetime
   asymm = rep(0, n = 5), # symmetric obligations
   asymm.cap = rep(1, n = 5), # asymmetric cap
   non.maj.only = rep(0, n = 5),
   mean.threat = rep(median(key.data$mean.threat), n = 5),
   low.kap.sc = rep(median(key.data$low.kap.sc), n = 5),
   begyr = rep(median(key.data$begyr), n = 5)
)
glimpse(sim.data)

# build out predictions for depth
# Take these off the link function scale using linkinv from betareg package
linkinv <- function(eta) pmax(pmin(exp(-exp(-eta)), 1 - .Machine$double.eps), .Machine$double.eps)

pred.depth.split <- predict(joint.gjrm.split, eq = 2,
                           type = "response", 
                           se.fit = TRUE,
                           newdata = sim.data)

pred.depth.split <- cbind.data.frame(pred.depth.split$fit, 
                                    pred.depth.split$se.fit,
                                    sim.data$maxcap.rec,
                                    sim.data$maxcap.comp,
                                    sim.data$maxcap.cons,
                                    factor(seq(1:5))
                                    )
colnames(pred.depth.split) <- c("pred", "se", "maxcap.rec",
                               "maxcap.comp", "maxcap.cons",
                               "scenario")

# calculate lower and upper bounds of 95% CI
pred.depth.split$lower <- pred.depth.split$pred - 2*pred.depth.split$se
pred.depth.split$upper <- pred.depth.split$pred + 2*pred.depth.split$se

# get predictions and unc back on response scale
pred.depth.split$response <- linkinv(pred.depth.split$pred)
pred.depth.split$lower.res <- linkinv(pred.depth.split$lower)
pred.depth.split$upper.res <- linkinv(pred.depth.split$upper)


ggplot(pred.depth.split, aes(x = scenario, y = response)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower.res,
                ymax = upper.res),
                width = .1, size = 1) +
  scale_x_discrete(breaks=c("1", "2", "3", "4", "5"), 
                   labels=c("None","Election", "Election & Comp.",
                            "Election & Const.", "All")) +
  labs(y = "Rescaled Latent Depth",
       x = "Democracy Components") +
  theme_bw()



# repeat the process for unconditional military support
pred.uncond.split <- predict(joint.gjrm.split, eq = 1,
                            type = "response", 
                            se.fit = TRUE,
                            newdata = sim.data)

pred.uncond.split <- cbind.data.frame(pred.uncond.split$fit, 
                                     pred.uncond.split$se.fit,
                                     sim.data$maxcap.rec,
                                     sim.data$maxcap.comp,
                                     sim.data$maxcap.cons,
                                     factor(seq(1:5))
)
colnames(pred.uncond.split) <- c("pred", "se", "maxcap.rec",
                                "maxcap.comp", "maxcap.cons",
                                "scenario")

# calculate lower and upper bounds of 95% CI
pred.uncond.split$lower <- pred.uncond.split$pred - 2*pred.uncond.split$se
pred.uncond.split$upper <- pred.uncond.split$pred + 2*pred.uncond.split$se

# get predictions and unc back on response scale
pred.uncond.split$response <- linkinv(pred.uncond.split$pred)
pred.uncond.split$lower.res <- linkinv(pred.uncond.split$lower)
pred.uncond.split$upper.res <- linkinv(pred.uncond.split$upper)


ggplot(pred.uncond.split, aes(x = scenario, y = response)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower.res,
                    ymax = upper.res),
                width = .1, size = 1) +
  scale_x_discrete(breaks=c("1", "2", "3", "4", "5"), 
                   labels=c("None","Election", "Election & Comp",
                            "Election & Const", "All")) +
  labs(y = "Probability of Unconditional Military Support",
       x = "Democracy Components") +
  theme_bw()



### Results for the Appendix


# split models with proportion of democracies
stargazer(list(beta.reg.depth.prop, uncond.glm.prop),
          style = "all2",
          dep.var.labels=c("Latent Depth (rescaled)","Unconditional Military Support"),
          covariate.labels=c(
            "Proportion of Democracies",
            "Foreign Policy Concessions", "Number of Members",
            "Wartime Alliance", "Asymmetric Obligations",
            "Asymmetric Capability", "Non-Major Only",
            "Average Threat",
            "Foreign Policy Disagreement", "Start Year"
          ),
          keep.stat = c("n","ll"), ci=TRUE, 
          star.char = c("", "", ""),
          notes = "95\\% Confidence Intervals in Parentheses.", 
          notes.append = FALSE,
          label = c("tab:separate-models-prop")
)


# build out predictions from joint model
pred.depth.prop <- predict(joint.gjrm.prop, eq = 2,
                            type = "iterms", 
                            se.fit = TRUE)

pred.depth.prop <- cbind.data.frame(pred.depth.prop$fit[, "s(dem.prop)"], 
                                     pred.depth.prop$se.fit[, "s(dem.prop)"],
                                     key.data$dem.prop)
colnames(pred.depth.prop) <- c("pred", "se", "dem.prop")

plot.depth.prop <- ggplot(pred.depth.prop, aes(x = dem.prop, y = pred)) +
  geom_hline(yintercept = 0) +
  geom_rug(sides = "b", alpha = 1/2, position = "jitter") +
  geom_line() +
  geom_ribbon(aes(ymin = (pred - 2*se), 
                  ymax = (pred + 2*se) 
  ), alpha = .5
  ) +
  labs(x = "Proportion of Democracies", y = "Predicted Change in Treaty Depth") +
  ggtitle("Treaty Depth") +
  theme_bw()
plot.depth.prop


# unconditional military support 
pred.uncond.prop <- predict(joint.gjrm.prop, eq = 1,
                             type = "iterms", se.fit = TRUE)


pred.uncond.prop <- cbind.data.frame(pred.uncond.prop$fit[, "s(dem.prop)"], 
                                     pred.uncond.prop$se.fit[, "s(dem.prop)"],
                                      key.data$dem.prop)
colnames(pred.uncond.prop) <- c("pred", "se", "dem.prop")

plot.uncond.prop <- ggplot(pred.uncond.prop, aes(x = dem.prop, y = pred)) +
  geom_rug(sides = "b", alpha = 1/2, position = "jitter") +
  geom_line() +
  geom_ribbon(aes(ymin = (pred - 2*se), 
                  ymax = (pred + 2*se) 
  ), alpha = .5
  ) +
  labs(x = "Proportion of Democracies", y = "Predicted Probability") +
  ggtitle("Unconditional Military Support") +
  theme_bw()
plot.uncond.prop


# combine plots and export
grid.arrange(plot.uncond.prop, plot.depth.prop,
             ncol = 2)
results.prop <- arrangeGrob(plot.uncond.prop, plot.depth.prop,
                            ncol = 2)
ggsave("appendix/results-prop.png", results.prop,
       height = 6, width = 8) #save file

# Predicted error correlations
joint.pred.error <- predict(joint.gjrm.prop, eq = 4,
                            type = "iterms", se.fit = TRUE)


# plot by year 
pred.error.year <- cbind.data.frame(joint.pred.error$fit[, "s(begyr)"], 
                                    joint.pred.error$se.fit[, "s(begyr)"],
                                    key.data$begyr)
colnames(pred.error.year) <- c("pred", "se", "begyr")

plot.error.year <- filter(pred.error.year, begyr >= 1850) %>% # filter out crazy predictions
  ggplot( aes(x = begyr, y = pred)) +
  geom_hline(yintercept = 0) +
  geom_rug(sides = "b", alpha = 1/2, position = "jitter") +
  geom_line() +
  geom_ribbon(aes(ymin = (pred - 2*se), 
                  ymax = (pred + 2*se) 
  ), alpha = .5
  ) +
  labs(x = "Start Year of the Alliance", y = expression(hat(theta))) +
  ggtitle("Start Year of the Alliance: Smoothed Term") +
  theme_bw()
plot.error.year



# Look at differences in tau
# set-up dataframe
tau.data <- cbind.data.frame(select(key.data, atopid, begyr), joint.gjrm$tau)
colnames(tau.data) <- c("atopid", "begyr", "tau")



# plot tau against start year 
plot.tau.year <- filter(tau.data, begyr >= 1850) %>%
  ggplot(aes(x = begyr, y = tau)) +
  geom_jitter() +
  labs(x = "Start Year of the Alliance", y = expression(hat(tau))) +
  ggtitle("Start Year: Predicted Error Correlations") +
  theme_bw()
plot.tau.year


# plot the two together
grid.arrange(plot.error.year, plot.tau.year,
             nrow = 2)
results.error <- arrangeGrob(plot.error.year, plot.tau.year,
                             nrow = 2)
ggsave("appendix/results-error.png", results.error,
       height = 6, width = 8) #save file



### summarize results from a trivariate model

# new data for simulation
sim.data.tri <- cbind.data.frame(
  x0 = rep(1, n = 21), # intercept
  maxcap.democ = seq(-10, 10, by = 1),
  fp.conc.index = rep(0, n = 21), # no concessions
  num.mem = rep(2, n = 21), # bilateral
  wartime = rep(0, n = 21), # peacetime
  asymm = rep(0, n = 21), # symmetric obligations
  asymm.cap = rep(1, n = 21), # asymmetric cap
  non.maj.only = rep(0, n = 21),
  mean.threat = rep(median(key.data$mean.threat), n = 21),
  low.kap.sc = rep(median(key.data$low.kap.sc), n = 21),
  begyr = rep(median(key.data$begyr), n = 21)
)

# start with treaty depth
pred.depth.tri <- predict(joint.gjrm.tri, eq = 2,
                            type = "response", 
                            se.fit = TRUE,
                            newdata = sim.data.tri)

pred.depth.tri <- cbind.data.frame(pred.depth.tri$fit, 
                                     pred.depth.tri$se.fit,
                                     sim.data.tri$maxcap.democ)
colnames(pred.depth.tri) <- c("pred", "se", "maxcap.democ")

# calculate lower and upper bounds of 95% CI
pred.depth.tri$lower <- pred.depth.tri$pred - 2*pred.depth.tri$se
pred.depth.tri$upper <- pred.depth.tri$pred + 2*pred.depth.tri$se

# get predictions and unc back on response scale
pred.depth.tri$response <- linkinv(pred.depth.tri$pred)
pred.depth.tri$lower.res <- linkinv(pred.depth.tri$lower)
pred.depth.tri$upper.res <- linkinv(pred.depth.tri$upper)


depth.plot.tri <- ggplot(pred.depth.tri, aes(x = maxcap.democ, y = response)) +
                     geom_point(size = 2) +
                     geom_errorbar(aes(ymin = lower.res,
                      ymax = upper.res),
                      width = .1, size = 1) +
                     labs(y = "",
                       x = "Alliance Leader Polity") +
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
                                   sim.data.tri$maxcap.democ)
colnames(pred.uncond.tri) <- c("pred", "se", "maxcap.democ")

# calculate lower and upper bounds of 95% CI
pred.uncond.tri$lower <- pred.uncond.tri$pred - 2*pred.uncond.tri$se
pred.uncond.tri$upper <- pred.uncond.tri$pred + 2*pred.uncond.tri$se

# get predictions and unc back on response scale
pred.uncond.tri$response <- linkinv(pred.uncond.tri$pred)
pred.uncond.tri$lower.res <- linkinv(pred.uncond.tri$lower)
pred.uncond.tri$upper.res <- linkinv(pred.uncond.tri$upper)


uncond.plot.tri <- ggplot(pred.uncond.tri, aes(x = maxcap.democ, y = response)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower.res,
                    ymax = upper.res),
                width = .1, size = 1) +
  labs(y = "",
       x = "Alliance Leader Polity") +
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
                                    sim.data.tri$maxcap.democ)
colnames(pred.issue.tri) <- c("pred", "se", "maxcap.democ")

# calculate lower and upper bounds of 95% CI
pred.issue.tri$lower <- pred.issue.tri$pred - 2*pred.issue.tri$se
pred.issue.tri$upper <- pred.issue.tri$pred + 2*pred.issue.tri$se

# get predictions and unc back on response scale
pred.issue.tri$response <- linkinv(pred.issue.tri$pred)
pred.issue.tri$lower.res <- linkinv(pred.issue.tri$lower)
pred.issue.tri$upper.res <- linkinv(pred.issue.tri$upper)


issue.plot.tri <- ggplot(pred.issue.tri, aes(x = maxcap.democ, y = response)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower.res,
                    ymax = upper.res),
                width = .1, size = 1) +
  labs(y = "",
       x = "Alliance Leader Polity") +
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
