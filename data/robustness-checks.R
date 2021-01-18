# Joshua Alley
# Bivariate depth model robustness checks
# must run after joint-cred-analysis.R 

# add a control for US membership




### alternative measures of electoral competition



### plot results


# Create a function to plot for all the models and variables

substance.plot <- function(model, label, destination){

  
# list of plots
depth.plots <- vector(mode = "list", length = length(models.comp))
  
# for loop to plot  
for(i in 1:length(model)){

sim.data <- cbind.data.frame(
    x0 = rep(1, n = 2), # intercept
    maxcap.cons = rep(1, n = 2),
    par = c(min(model[[i]][["X1"]][, 3]), max(model[[i]][["X1"]][, 3])), # placeholder
    prop.cons = rep(1, n = 2), 
    maxcap.comp = rep(1, n = 2),
    maxcap.inc.std = rep(median(key.data.poly$maxcap.inc.std), na.rm = TRUE),
    econagg.dum = rep(0, n = 2),
    fp.conc.index = rep(0, n = 2), # no concessions
    num.mem = rep(2, n = 2), # bilateral
    wartime = rep(0, n = 2), # peacetime
    asymm = rep(0, n = 2), # symmetric obligations
    asymm.cap = rep(1, n = 2), # asymmetric cap
    non.maj.only = rep(0, n = 2),
    mean.threat = rep(median(key.data$mean.threat), n = 2),
    low.kap.sc = rep(median(key.data$low.kap.sc), n = 2),
    begyr = rep(median(key.data$begyr), n = 2),
    us.mem = rep(0, n = 2)
  )
  colnames(sim.data)[3] <- colnames(model[[i]][["X1"]])[3] # give proper name
  glimpse(sim.data)

# build out predictions for depth
pred.depth.mat <- predict(model[[i]], eq = 2,
                          type = "lpmatrix", 
                          newdata = sim.data)

# Create vectors of parameter replciates 
# similar to bootstrap with lm or rlm 
rmvn <- function(n,mu,sig) { ## MVN random deviates
  L <- mroot(sig);m <- ncol(L);
  t(mu + L%*%matrix(rnorm(m*n),m,n)) 
}

br <- rmvn(1000,coef(model[[i]]), model[[i]]$Vb) ## 1000 replicate param. vectors

dim(br[, (ncol(pred.depth.mat)+1):(ncol(pred.depth.mat) + ncol(pred.depth.mat))])
dim(t(pred.depth.mat))

# Calculate differences
sim.res <- data.frame(linkinv(br[, (ncol(pred.depth.mat)+1):(ncol(pred.depth.mat) + 
                                ncol(pred.depth.mat))] %*% t(pred.depth.mat)))
depth.diff <- rbind.data.frame(
  quantile(sim.res$X1, c(0.025, .975)),
  quantile(sim.res$X2, c(0.025, .975)),
  quantile(sim.res$X2 - sim.res$X1, c(0.025, .975))
)
colnames(depth.diff) <- c("lower", "upper")
depth.diff$scenario <- c("Low", "High",
                         "Difference")

# plot intervals
depth.intervals <- ggplot(depth.diff, aes(x = scenario, y = upper)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                width = .1, size = 1) +
  labs(y = "Predicted Treaty Depth",
       x = "Scenario") +
  ggtitle(label[[i]]) +
  theme_bw()
depth.intervals

# add to plot list
depth.plots[[i]] <- depth.intervals

# calculate variance from inverting link transformation 
res <- linkinv(br[, (ncol(pred.depth.mat)+1):(ncol(pred.depth.mat)+ ncol(pred.depth.mat))] %*% t(pred.depth.mat))
mean(res);var(res)


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
models.comp = list(joint.gjrm.prop, joint.gjrm.pol, gjrm.poly.vdem)
labels.comp = c("Proportion Electoral Democracy", "Polity Elections.", "Polyarchy (VDem)")


# Apply the function 
substance.plot(model = models.comp, label = labels.comp,
               destination = "figures/results-other-democ.png")




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

