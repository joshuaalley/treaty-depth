# Joshua Alley
# Texas A&M University
# Plot results for mediation analyses of latent depth


# Tabulate single equations
stargazer(list(beta.reg.depth, uncond.glm),
          style = "all2",
          dep.var.labels=c("Latent Depth (rescaled)","Unconditional Military Support"),
          covariate.labels=c(
            "Average Polity Score",
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



# results with max democracy in alliance
# tabulate the results 
summary.depth.gjrm <- summary(joint.gjrm)
summary.depth.gjrm[["tableP1"]] # uncond milsup
summary.depth.gjrm[["tableP2"]] # depth 

# Combine all the results in a single table. 
# Start with unconditional table
uncond.tab <- as.data.frame(rbind(summary.depth.gjrm[["tableP1"]][, 1:2],
                                  summary.depth.gjrm[["tableNP1"]][, c(1, 3)]
))
uncond.tab$variable <- c("(Intercept)", "Economic Issue Linkage", 
                         "FP Concessions", "Number of Members", 
                         "Wartime Alliances", "Asymmetric Obligations",
                         "Asymmetric Capability", "Non-Major Only", "FP Disagreement",
                         "s(Avg. Polity Score)", "s(Mean Threat)", "s(Start Year)")


# table for treaty depth
depth.tab <- as.data.frame(rbind(summary.depth.gjrm[["tableP2"]][, 1:2],
                                 summary.depth.gjrm[["tableNP2"]][, c(1, 3)]
))
depth.tab$variable <- c("(Intercept)", "Economic Issue Linkage",
                        "FP Concessions", "Number of Members", 
                        "Wartime Alliances", "Asymmetric Obligations",
                        "Asymmetric Capability", "Non-Major Only", "FP Disagreement",
                        "s(Avg. Polity Score)", "s(Mean Threat)", "s(Start Year)")

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
                     The error correlation between the two processes is modeled with a Plackett copula."),
         label = c("tab:gjrm-res"), auto = TRUE),
  add.to.row = head.xtab, 
  include.rownames = FALSE)



### Predictions from model with average democracy

# build out predictions
joint.pred.depth <- predict(joint.gjrm, eq = 2,
                                     type = "iterms", 
                            se.fit = TRUE)

pred.depth.democ <- cbind.data.frame(joint.pred.depth$fit[, "s(avg.democ)"], 
                          joint.pred.depth$se.fit[, "s(avg.democ)"],
                          key.data$avg.democ)
colnames(pred.depth.democ) <- c("pred", "se", "avg.democ")

plot.depth <- ggplot(pred.depth.democ, aes(x = avg.democ, y = pred)) +
                   geom_hline(yintercept = 0) +
                   geom_rug(sides = "b", alpha = 1/2, position = "jitter") +
                   geom_line() +
                   geom_ribbon(aes(ymin = (pred - 2*se), 
                              ymax = (pred + 2*se) 
                               ), alpha = .5
                       ) +
               labs(x = "Average Polity Score", y = "Predicted Change in Treaty Depth") +
               ggtitle("Treaty Depth") +
               theme_bw()
plot.depth


# unconditional military support 
joint.pred.uncond <- predict(joint.gjrm, eq = 1,
                                      type = "iterms", se.fit = TRUE)


pred.uncond.democ <- cbind.data.frame(joint.pred.uncond$fit[, "s(avg.democ)"], 
                                     joint.pred.uncond$se.fit[, "s(avg.democ)"],
                                     key.data$avg.democ)
colnames(pred.uncond.democ) <- c("pred", "se", "avg.democ")

plot.uncond <- ggplot(pred.uncond.democ, aes(x = avg.democ, y = pred)) +
                     geom_rug(sides = "b", alpha = 1/2, position = "jitter") +
                     geom_line() +
                     geom_ribbon(aes(ymin = (pred - 2*se), 
                       ymax = (pred + 2*se) 
                       ), alpha = .5
                     ) +
                labs(x = "Average Polity Score", y = "Predicted Probability") +
                ggtitle("Unconditional Military Support") +
                theme_bw()
plot.uncond


# combine plots and export
grid.arrange(plot.uncond, plot.depth,
             ncol = 2)
results.democ <- arrangeGrob(plot.uncond, plot.depth,
                             ncol = 2)
ggsave("figures/results-democ-avg.png", results.democ, 
       height = 6, width = 8) #save file


# Predicted error correlations
joint.pred.error <- predict(joint.gjrm, eq = 4,
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


# plot tau: estimated corrlated between the two equations
boxplot(joint.gjrm.max$tau, ylim=c(-1, 1), 
        main = expression(hat(tau)))


# Look at differences in tau
# set-up dataframe
tau.data <- cbind.data.frame(select(key.data, atopid, avg.democ, begyr), joint.gjrm$tau)
colnames(tau.data) <- c("atopid", "avg.democ", "begyr", "tau")


# plot tau against democracy
plot.tau <-  ggplot(tau.data, aes(x= avg.democ, y = tau)) +
        geom_jitter() +
        labs(x = "Average Polity Score", y = expression(hat(tau)))  +
        ggtitle("Avg. Democracy: Predicted Error Correlations") +
        theme_bw()
plot.tau

# plot tau against start year 
plot.tau.year <- filter(tau.data, begyr >= 1850) %>%
        ggplot(aes(x= begyr, y = tau)) +
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
ggsave("figures/results-error.png", results.error,
       height = 6, width = 8) #save file



### Plot predictions for openenss

# set up new data
sim.data <- cbind.data.frame(
   x0 = rep(1, n = 3), # intercept
   maxcap.open = c(0, 1, 2),
   econagg.dum = rep(0, n = 3),
   fp.conc.index = rep(0, n = 3), # no concessions
   num.mem = rep(2, n = 3), # bilateral
   wartime = rep(0, n = 3), # peacetime
   asymm = rep(0, n = 3), # symmetric obligations
   asymm.cap = rep(1, n = 3), # asymmetric cap
   non.maj.only = rep(0, n = 3),
   mean.threat = rep(median(key.data$mean.threat), n = 3),
   low.kap.sc = rep(median(key.data$low.kap.sc), n = 3),
   begyr = rep(median(key.data$begyr), n = 3)
)
glimpse(sim.data)

# build out predictions for depth
# Take these off the link function scale using linkinv from betareg package
linkinv <- function(eta) pmax(pmin(exp(-exp(-eta)), 1 - .Machine$double.eps), .Machine$double.eps)

pred.depth.open <- predict(joint.gjrm.open, eq = 2,
                           type = "response", 
                           se.fit = TRUE,
                           newdata = sim.data)

pred.depth.open <- cbind.data.frame(pred.depth.open$fit, 
                                    pred.depth.open$se.fit,
                                    sim.data$maxcap.open)
colnames(pred.depth.open) <- c("pred", "se", "maxcap.open")

# calculate lower and upper bounds of 95% CI
pred.depth.open$lower <- pred.depth.open$pred - 2*pred.depth.open$se
pred.depth.open$upper <- pred.depth.open$pred + 2*pred.depth.open$se

# get predictions and unc back on response scale
pred.depth.open$response <- linkinv(pred.depth.open$pred)
pred.depth.open$lower.res <- linkinv(pred.depth.open$lower)
pred.depth.open$upper.res <- linkinv(pred.depth.open$upper)


ggplot(pred.depth.open, aes(x = as.factor(maxcap.open), y = response)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower.res,
                ymax = upper.res),
                width = .1, size = 1) +
  labs(y = "Rescaled Latent Depth",
       x = "Political Openess of Most Capable State") +
  theme_bw()


# predictions for unconditional military support
pred.uncond.open <- predict(joint.gjrm.open, eq = 1,
                           type = "response", 
                           se.fit = TRUE,
                           newdata = sim.data)

pred.uncond.open <- cbind.data.frame(pred.uncond.open$fit, 
                                    pred.uncond.open$se.fit,
                                    sim.data$maxcap.open)
colnames(pred.uncond.open) <- c("pred", "se", "maxcap.open")


ggplot(pred.uncond.open, aes(x = as.factor(maxcap.open), y = pred)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = pred - 2*se,
                    ymax = pred + 2*se),
                width = .1, size = 1) +
  theme_bw()



### Results for the Appendix


# build out predictions
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



### plot results for model with democracy of the most capable member
# build out predictions
pred.depth.mcap <- predict(joint.gjrm.mcap, eq = 2,
                           type = "iterms", 
                           se.fit = TRUE)

pred.depth.mcap <- cbind.data.frame(pred.depth.mcap$fit[, "s(maxcap.democ)"], 
                                    pred.depth.mcap$se.fit[, "s(maxcap.democ)"],
                                    key.data$maxcap.democ)
colnames(pred.depth.mcap) <- c("pred", "se", "maxcap.democ")

plot.depth.mcap <- ggplot(pred.depth.mcap, aes(x = maxcap.democ, y = pred)) +
  geom_hline(yintercept = 0) +
  geom_rug(sides = "b", alpha = 1/2, position = "jitter") +
  geom_line() +
  geom_ribbon(aes(ymin = (pred - 2*se), 
                  ymax = (pred + 2*se) 
  ), alpha = .5
  ) +
  labs(x = "Polity Score of Most Capable Alliance Member", y = "Predicted Change in Treaty Depth") +
  ggtitle("Treaty Depth") +
  theme_bw()
plot.depth.mcap


# unconditional military support 
pred.uncond.mcap <- predict(joint.gjrm.mcap, eq = 1,
                            type = "iterms", se.fit = TRUE)


pred.uncond.mcap <- cbind.data.frame(pred.uncond.mcap$fit[, "s(maxcap.democ)"], 
                                     pred.uncond.mcap$se.fit[, "s(maxcap.democ)"],
                                     key.data$maxcap.democ)
colnames(pred.uncond.mcap) <- c("pred", "se", "maxcap.democ")

plot.uncond.mcap <- ggplot(pred.uncond.mcap, aes(x = maxcap.democ, y = pred)) +
  geom_rug(sides = "b", alpha = 1/2, position = "jitter") +
  geom_line() +
  geom_ribbon(aes(ymin = (pred - 2*se), 
                  ymax = (pred + 2*se) 
  ), alpha = .5
  ) +
  labs(x = "Polity Score of Most Capable Alliance Member", y = "Predicted Probability") +
  ggtitle("Unconditional Military Support") +
  theme_bw()
plot.uncond.mcap


# combine plots 
grid.arrange(plot.uncond.mcap, plot.depth.mcap,
             ncol = 2)



