# Joshua Alley
# Texas A&M University
# Plot results for mediation analyses of latent depth


# Tabulate single equations
stargazer(list(beta.reg.depth, uncond.glm),
          style = "all2",
          dep.var.labels=c("Latent Depth (rescaled)","Unconditional Military Support"),
          covariate.labels=c(
            "Maximum Democracy",
            "Foreign Policy Concessions", "Number of Members",
            "Wartime Alliance", "Asymmetric Obligations",
            "Asymmetric Capability",
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
summary.depth.gjrm <- summary(joint.gjrm.max)
summary.depth.gjrm[["tableP1"]] # uncond milsup
summary.depth.gjrm[["tableP2"]] # depth 

# Combine all the results in a single table. 
# Start with unconditional table
uncond.tab <- as.data.frame(rbind(summary.depth.gjrm[["tableP1"]][, 1:2],
                                  summary.depth.gjrm[["tableNP1"]][, c(1, 3)]
))
uncond.tab$variable <- c("(Intercept)", "Economic Issue Linkage", # "Latent Depth", "Uncond. Mil. Support"
                         "FP Concessions", "Number of Members", 
                         "Wartime Alliances", "Asymmetric Obligations",
                         "Asymmetric Capability", "FP Disagreement",
                         "s(Avg. Democracy)", "s(Mean Threat)", "s(Start Year)")


# table for treaty depth
depth.tab <- as.data.frame(rbind(summary.depth.gjrm[["tableP2"]][, 1:2],
                                 summary.depth.gjrm[["tableNP2"]][, c(1, 3)]
))
depth.tab$variable <- c("(Intercept)", "Economic Issue Linkage", # "Latent Depth", "Uncond. Mil. Support"
                        "FP Concessions", "Number of Members", 
                        "Wartime Alliances", "Asymmetric Obligations",
                        "Asymmetric Capability", "FP Disagreement",
                        "s(Avg. Democracy)", "s(Mean Threat)", "s(Start Year)")

joint.tab <- full_join(uncond.tab, depth.tab, by = "variable")
joint.tab <- as.data.frame(joint.tab[, c(3, 1, 2, 4, 5)])


# Tabulate all the equations together
# create a header
head.xtab <- list()
head.xtab$pos <- list(-1)
head.xtab$command <- paste0(paste0('& \\multicolumn{2}{c}{Uncond. Mil. Support} & \\multicolumn{2}{c}{Latent Depth}',
                                   collapse=''), '\\')



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


# build out predictions for smoothed terms 
joint.pred.depth.max <- predict(joint.gjrm.max, eq = 2,
                                type = "iterms", 
                                se.fit = TRUE)

pred.depth.max <- cbind.data.frame(joint.pred.depth.max$fit[, "s(max.democ)"], 
                                   joint.pred.depth.max$se.fit[, "s(max.democ)"],
                                   key.data$max.democ)
colnames(pred.depth.max) <- c("pred", "se", "max.democ")

plot.depth.max <- ggplot(pred.depth.max, aes(x = max.democ, y = pred)) +
  geom_hline(yintercept = 0) +
  geom_rug(sides = "b", alpha = 1/2, position = "jitter") +
  geom_line() +
  geom_ribbon(aes(ymin = (pred - 2*se), 
                  ymax = (pred + 2*se) 
  ), alpha = .5
  ) +
  labs(x = "Maximum Democracy", y = "Predicted Change in Treaty Depth") +
  ggtitle("Treaty Depth") +
  theme_bw()
plot.depth.max


# unconditional military support 
joint.pred.uncond.max <- predict(joint.gjrm.max, eq = 1,
                                 type = "iterms", se.fit = TRUE)


pred.uncond.max <- cbind.data.frame(joint.pred.uncond.max$fit[, "s(max.democ)"], 
                                    joint.pred.uncond.max$se.fit[, "s(max.democ)"],
                                    key.data$max.democ)
colnames(pred.uncond.max) <- c("pred", "se", "max.democ")

plot.uncond.max <- ggplot(pred.uncond.max, aes(x = max.democ, y = pred)) +
  geom_hline(yintercept = 0) +
  geom_rug(sides = "b", alpha = 1/2, position = "jitter") +
  geom_line() +
  geom_ribbon(aes(ymin = (pred - 2*se), 
                  ymax = (pred + 2*se) 
  ), alpha = .5
  ) +
  labs(x = "Maximum Democracy", y = "Predicted Probability") +
  ggtitle("Unconditional Military Support") +
  theme_bw()
plot.uncond.max


# combine plots and export
grid.arrange(plot.uncond.max, plot.depth.max,
             ncol = 2)
results.democ.max <- arrangeGrob(plot.uncond.max, plot.depth.max,
                                 ncol = 2)
ggsave("figures/results-democ-max.png", results.democ.max, 
       height = 6, width = 8) #save file


# Predicted error correlations
joint.pred.error.max <- predict(joint.gjrm.max, eq = 4,
                                type = "iterms", se.fit = TRUE)


# plot by democracy
pred.error.max <- cbind.data.frame(joint.pred.error.max$fit[, "s(max.democ)"], 
                                   joint.pred.error.max$se.fit[, "s(max.democ)"],
                                   key.data$max.democ)
colnames(pred.error.max) <- c("pred", "se", "max.democ")

plot.error.max <- ggplot(pred.error.max, aes(x = max.democ, y = pred)) +
  geom_hline(yintercept = 0) +
  geom_rug(sides = "b", alpha = 1/2, position = "jitter") +
  geom_line() +
  geom_ribbon(aes(ymin = (pred - 2*se), 
                  ymax = (pred + 2*se) 
  ), alpha = .5
  ) +
  labs(x = "Maximum Democracy", y = expression(hat(theta))) +
  ggtitle("Maximum Democracy: Smoothed Term") +
  theme_bw()
plot.error.max


# plot by year 
pred.error.year <- cbind.data.frame(joint.pred.error.max$fit[, "s(begyr)"], 
                                   joint.pred.error.max$se.fit[, "s(begyr)"],
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
tau.data <- cbind.data.frame(select(key.data, atopid, max.democ, begyr), joint.gjrm.max$tau)
colnames(tau.data) <- c("atopid", "max.democ", "begyr", "tau")

# plot tau against democracy
plot.tau.max <- ggplot(tau.data, aes(x= max.democ, y = tau)) +
                  geom_jitter() +
                  labs(x = "Maximum Democracy", y = expression(hat(tau)))  +
                  ggtitle("Max. Democracy: Predicted Error Correlations") +
                  geom_smooth(method = "loess") +
                  theme_bw()
plot.tau.max

# plot tau against start year 
plot.tau.year <- filter(tau.data, begyr >= 1850) %>%
                 ggplot(aes(x= begyr, y = tau)) +
                   geom_jitter() +
                   labs(x = "Start Year of the Alliance", y = expression(hat(tau))) +
                   ggtitle("Start Year: Predicted Error Correlations") +
                   theme_bw()
plot.tau.year


# plot the two together
grid.arrange(plot.error.max, plot.error.year, 
             plot.tau.max, plot.tau.year,
             nrow = 2, ncol = 2)
results.error <- arrangeGrob(plot.error.max, plot.error.year, 
                             plot.tau.max, plot.tau.year,
                             nrow = 2, ncol = 2)
ggsave("figures/results-error.png", results.error,
       height = 6, width = 8) #save file





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
               labs(x = "Average Democracy", y = "Predicted Change in Treaty Depth") +
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
                labs(x = "Average Democracy", y = "Predicted Probability") +
                ggtitle("Unconditional Military Support") +
                theme_bw()
plot.uncond


# combine plots and export
grid.arrange(plot.uncond, plot.depth,
             ncol = 2)
results.democ <- arrangeGrob(plot.uncond, plot.depth,
                             ncol = 2)
ggsave("appendix/results-democ-avg.png", results.democ, 
       height = 6, width = 8) #save file









### Results for the Appendix



# Summarize brms results with uncertainty
summary.brms <- summary(brm.multivar.unc)
summary.brms


