# Joshua Alley
# Texas A&M University
# Plot results for mediation analyses of latent depth


# Tabulate single equations
stargazer(list(beta.reg.depth, uncond.glm),
          style = "all2")

# Look at the results: depth variable
summary.depth.gjrm <- summary(joint.gjrm)
summary.depth.gjrm[["tableP1"]] # uncond milsup
summary.depth.gjrm[["tableP2"]] # depth 

# Combine all the results in a single table. 
# Start with unconditional table
uncond.tab <- as.data.frame(rbind(summary.depth.gjrm[["tableP1"]][, 1:2],
                               summary.depth.gjrm[["tableNP1"]][, c(1, 3)]
                               ))
uncond.tab$variable <- rownames(uncond.tab)

# table for treaty depth
depth.tab <- as.data.frame(rbind(summary.depth.gjrm[["tableP2"]][, 1:2],
                    summary.depth.gjrm[["tableNP2"]][, c(1, 3)]
                     ))
depth.tab$variable <- rownames(depth.tab)

joint.tab <- full_join(uncond.tab, depth.tab, by = "variable")
joint.tab <- as.data.frame(joint.tab[, c(3, 1, 2, 4, 5)])


# Tabulate all the equations together
stargazer(joint.tab, summary = FALSE,
          style = "io")
xtable(joint.tab)

# plot tau: estimated corrlated between the two equations
boxplot(joint.gjrm$tau, ylim=c(-1, 1), 
        main = expression(hat(tau)))

# results with deep alliance dummy 
summary.dum.gjrm <- summary(joint.gjrm.dum)
summary.dum.gjrm[["tableP1"]] # uncond milsup
summary.dum.gjrm[["tableP2"]] # depth 



# build out predictions
joint.pred.depth <- predict(joint.gjrm, eq = 2,
                                     type = "iterms", 
                            se.fit = TRUE)

pred.depth.democ <- cbind.data.frame(joint.pred.depth$fit[, "s(avg.democ)"], 
                          joint.pred.depth$se.fit[, "s(avg.democ)"],
                          joint.gjrm$dataset$avg.democ)
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
                                     joint.gjrm$dataset$avg.democ)
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
boxplot(joint.gjrm$tau[joint.gjrm$dataset$avg.democ < 4 ,], ylim=c(-1, 1), 
        main=expression(hat(tau)), xlab="Low Democracy")

# high democracy
boxplot(joint.gjrm$tau[joint.gjrm$dataset$avg.democ >= 5 ,], ylim=c(-1, 1), 
        main=expression(hat(tau)), xlab="High Democracy")


# before 1950
boxplot(joint.gjrm$tau[joint.gjrm$dataset$begyr < 1946 ,], ylim=c(-1, 0), 
        main=expression(hat(tau)), xlab="Before 1946")

# after 1950
boxplot(joint.gjrm$tau[joint.gjrm$dataset$begyr >= 1946 ,], ylim=c(-1, 0), 
        main=expression(hat(tau)), xlab="After 1946")


# Summarize brms results with uncertainty
summary.brms <- summary(brm.multivar.unc)
summary.brms


