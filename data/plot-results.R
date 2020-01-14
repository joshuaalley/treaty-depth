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
2*sd(atop.milsup$avg.democ, na.rm = TRUE)
.43 / sd(key.data$latent.depth.mean.rs) 


# Summarize brms results with uncertainty
summary(brm.multivar.unc)



