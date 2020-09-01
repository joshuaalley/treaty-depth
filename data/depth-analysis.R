# Joshua Alley
# Texas A&M University 
# Analysis of the sources of alliance treaty depth


# focus on the posterior mean in this script: include uncertainty in later scripts


### Analysis focusing on alliances with military support


# Regression of depth on other alliance characteristics
depth.reg.dem <- lm(latent.depth.mean ~ maxcap.democ +
                  econagg.dum + 
                  fp.conc.index + num.mem + wartime + asymm + 
                  asymm.cap + non.maj.only +
                  mean.threat +
                  low.kap.sc + post45,
                data = atop.milsup)
summary(depth.reg.dem)
plot(density(depth.reg.dem$residuals))

# Regression of depth on other democratic characteristics
depth.reg <- lm(latent.depth.mean ~
                  maxcap.cons + maxcap.lied +
                  econagg.dum + 
                  fp.conc.index + num.mem + wartime + asymm + 
                  asymm.cap + non.maj.only +
                  mean.threat +
                  low.kap.sc + post45,
                data = atop.milsup)
summary(depth.reg)


# Very skewed residuals: try skew models
# use a skew-t model
depth.reg.skewt <- selm(latent.depth.mean ~ 
                          maxcap.cons + maxcap.lied +
                          econagg.dum +
                          fp.conc.index + num.mem + wartime + asymm + 
                          asymm.cap + non.maj.only + 
                          mean.threat + low.kap.sc + post45,
                        data = atop.milsup,
                        family = "ST",
                        opt.method = "Nelder-Mead",
                        param.type = "pseudo-CP")
summary(depth.reg.skewt, "pseudo-CP")
plot(depth.reg.skewt, param.type = "pseudo-CP")

# also fit a skew-cauchy model
depth.reg.skewc <- selm(latent.depth.mean ~ 
                  maxcap.cons + maxcap.open +
                    econagg.dum +
                  fp.conc.index + num.mem + wartime + asymm + 
                    asymm.cap + non.maj.only + 
                    mean.threat + low.kap.sc + post45,
                data = atop.milsup,
                family = "SC",
                opt.method = "BFGS",
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
            "Economic Issue Linkage",
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
