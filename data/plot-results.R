# Joshua Alley
# Texas A&M University
# Plot results for mediation analyses of latent depth


# Tabulate single equations
stargazer(list(beta.reg.depth, uncond.glm),
          style = "all2",
          dep.var.labels=c("Latent Depth (rescaled)","Unconditional Military Support"),
          covariate.labels=c(
            "Average Democracy",
            "Foreign Policy Concessions", "Number of Members",
            "Wartime Alliance", "Asymmetric Obligations",
            "Asymmetric Capability", "Non-Major Power Only",
            "Average Threat",
            "Foreign Policy Disagreement", "Start Year"
          ),
          keep.stat = c("n","ll"), ci=TRUE, 
          star.char = c("", "", ""),
          notes = "95% Confidence Intervals in Parentheses.", 
          notes.append = FALSE,
          label = c("tab:separate-models")
)

# Look at the results: depth variable
summary.depth.gjrm <- summary(joint.gjrm)
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
                        "Asymmetric Capability", "Non-Major Only", "FP Disagreement",
                        "s(Avg. Democracy)", "s(Mean Threat)", "s(Start Year)")


# table for treaty depth
depth.tab <- as.data.frame(rbind(summary.depth.gjrm[["tableP2"]][, 1:2],
                    summary.depth.gjrm[["tableNP2"]][, c(1, 3)]
                     ))
depth.tab$variable <- c("(Intercept)", "Economic Issue Linkage", # "Latent Depth", "Uncond. Mil. Support"
                        "FP Concessions", "Number of Members", 
                        "Wartime Alliances", "Asymmetric Obligations",
                        "Asymmetric Capability", "Non-Major Only", "FP Disagreement",
                        "s(Avg. Democracy)", "s(Mean Threat)", "s(Start Year)")

joint.tab <- full_join(uncond.tab, depth.tab, by = "variable")
joint.tab <- as.data.frame(joint.tab[, c(3, 1, 2, 4, 5)])


# Tabulate all the equations together
stargazer(joint.tab, summary = FALSE,
          rownames = FALSE,
          style = "io")
print(
  xtable(joint.tab,
       caption = c("Results from joint generalized regression model of treaty depth and unconditional military support. 
       All smoothed terms report the effective degrees of freedom and the chi-squared term. 
       The unconditional military support model is a binomial GLM with a probit link function. 
       The treaty depth model is a beta regression. 
       The error correlation between the two processes is modeled with a T copula."),
       label = c("tab:gjrm-res"), auto = TRUE),
include.rownames = FALSE)


# plot tau: estimated corrlated between the two equations
boxplot(joint.gjrm$tau, ylim=c(-1, 1), 
        main = expression(hat(tau)))



# results with max democracy in alliance
summary.max.gjrm <- summary(joint.gjrm.max)
summary.max.gjrm[["tableP1"]] # uncond milsup
summary.max.gjrm[["tableP2"]] # depth 



# build out predictions
joint.pred.depth <- predict(joint.gjrm, eq = 2,
                                     type = "iterms", 
                            se.fit = TRUE)

pred.depth.democ <- cbind.data.frame(joint.pred.depth$fit[, "s(avg.democ)"], 
                          joint.pred.depth$se.fit[, "s(avg.democ)"],
                          key.data$avg.democ)
colnames(pred.depth.democ) <- c("pred", "se", "avg.democ")

plot.depth <- ggplot(pred.depth.democ, aes(x = avg.democ, y = pred)) +
                   geom_point() + 
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
                     geom_point() +
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
ggsave("figures/results-democ.png", results.democ, 
       height = 6, width = 8) #save file


# look at Taus by group
# Low democracy
par(mfrow = c(2,2))
boxplot(joint.gjrm$tau[key.data$avg.democ < 6], ylim=c(-1, 1), 
        main=expression(hat(tau)), xlab="Low Democracy")

# high democracy
boxplot(joint.gjrm$tau[key.data$avg.democ >= 6], ylim=c(-1, 1), 
        main=expression(hat(tau)), xlab="High Democracy")


# before 1950
boxplot(joint.gjrm$tau[key.data$begyr < 1946], ylim=c(-1, 1), 
        main=expression(hat(tau)), xlab="Before 1946")

# after 1950
boxplot(joint.gjrm$tau[key.data$begyr >= 1946], ylim=c(-1, 1), 
        main=expression(hat(tau)), xlab="After 1946")

dev.off()


# Summarize brms results with uncertainty
summary.brms <- summary(brm.multivar.unc)
summary.brms


