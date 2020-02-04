# Joshua Alley
# Texas A&M University
# Plot results for mediation analyses of latent depth


# Look at the results: depth variable
summary.depth.gjrm <- summary(joint.gjrm)
summary.depth.gjrm[["tableP1"]] # uncond milsup
summary.depth.gjrm[["tableP2"]] # depth 

xtable(summary.depth.gjrm[["tableP1"]])
xtable(summary.depth.gjrm[["tableP2"]])

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
boxplot(joint.gjrm$tau[joint.gjrm$dataset < 4 ,], ylim=c(-1, 1), 
        main=expression(hat(tau)), xlab="Low Democracy")

# high democracy
boxplot(joint.gjrm$tau[joint.gjrm$dataset >= 5 ,], ylim=c(-1, 1), 
        main=expression(hat(tau)), xlab="High Democracy")


# Summarize brms results with uncertainty
summary.brms <- summary(brm.multivar.unc)
summary.brms


