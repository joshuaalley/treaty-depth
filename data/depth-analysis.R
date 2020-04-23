# Joshua Alley
# Texas A&M University 
# Analysis of the sources of alliance treaty depth


# focus on the posterior mean in this script: include uncertainty in later scripts


### Analysis focusing on alliances with military support


# Regression of depth on other alliance characteristics
depth.reg <- lm(latent.depth.mean ~ maxcap.democ +
                  fp.conc.index + num.mem + wartime + asymm + 
                  asymm.cap + non.maj.only +
                  mean.threat +
                  low.kap.sc + begyr,
                data = atop.milsup)
summary(depth.reg)
plot(density(depth.reg$residuals))


# Non-normal residuals imply robust regression may be worthwhile 
depth.reg.rob <- rlm(latent.depth.mean ~ maxcap.democ + 
                  fp.conc.index + num.mem + wartime + asymm + 
                    asymm.cap + non.maj.only + 
                    mean.threat + low.kap.sc + begyr,
                data = atop.milsup)
summary(depth.reg.rob)
plot(depth.reg.rob$residuals, depth.reg.rob$w)


# Logit with deep alliance dummy: depth above median value
depth.glm <- glm(deep.alliance ~ maxcap.democ + 
                       fp.conc.index + num.mem + wartime + asymm + 
                       asymm.cap + non.maj.only + 
                   mean.threat + low.kap.sc + begyr,
                     family = binomial(link = "probit"),
                     data = atop.milsup)
summary(depth.glm)



### Split analysis: before and after 1945
atop.milsup.pre45 <- filter(atop.milsup, begyr <= 1945)
atop.milsup.post45 <- filter(atop.milsup, begyr > 1945)


# look at key covariates
# Alliance democracy 
summary(atop.milsup.pre45$maxcap.democ)
summary(atop.milsup.post45$maxcap.democ)
# alliance size 
summary(atop.milsup.pre45$num.mem)
summary(atop.milsup.post45$num.mem)
# alliance size: use a bilateral dummy instead 
table(atop.milsup.pre45$bilat)
table(atop.milsup.post45$bilat)
# asymmetric capability
table(atop.milsup.pre45$asymm.cap)
table(atop.milsup.post45$asymm.cap)
# unconditional military support
table(atop.milsup.pre45$uncond.milsup)
table(atop.milsup.post45$uncond.milsup)
# depth
summary(atop.milsup.pre45$latent.depth.mean)
summary(atop.milsup.post45$latent.depth.mean)


# analysis before 1945 
# depth 
depth.reg.pre45 <- rlm(latent.depth.mean ~ maxcap.democ +
                       fp.conc.index + num.mem + wartime + asymm + 
                         asymm.cap + non.maj.only + mean.threat +
                       low.kap.sc,
                     data = atop.milsup.pre45)
summary(depth.reg.pre45)


# unconditional military support 
uncond.glm.pre45 <- glm(uncond.milsup ~ maxcap.democ + 
                    fp.conc.index + num.mem + wartime + asymm + 
                    asymm.cap + non.maj.only + mean.threat +
                    low.kap.sc,
                  family = binomial(link = "probit"),
                  data = atop.milsup.pre45)
summary(uncond.glm.pre45)


# analysis after 1945 
# depth 
depth.reg.post45 <- rlm(latent.depth.mean ~ maxcap.democ +  
                         fp.conc.index + num.mem + wartime + asymm + 
                         asymm.cap + non.maj.only + mean.threat +
                         low.kap.sc,
                       data = atop.milsup.post45)
summary(depth.reg.post45)


# unconditional military support 
# if compare non-maj only to (1) symmetric maj  and asymm
uncond.glm.post45 <- glm(uncond.milsup ~ maxcap.democ + econagg.dum +
                          fp.conc.index + num.mem + wartime + asymm + 
                           non.maj.only + mean.threat +
                          low.kap.sc,
                        family = binomial(link = "probit"),
                        data = atop.milsup.post45)
summary(uncond.glm.post45)


# analysis after 1919 a la Mattes 2012 
atop.milsup.post19 <- filter(atop.milsup, begyr >= 1919)
depth.reg.post19 <- rlm(latent.depth.mean ~ maxcap.democ +
                          fp.conc.index + num.mem + wartime + asymm + 
                          asymm.cap + non.maj.only + mean.threat +
                          low.kap.sc + begyr,
                        data = atop.milsup.post19)
summary(depth.reg.post19)



# Summarize results in a table for the appendix
stargazer(list(depth.reg, depth.reg.rob, depth.reg.post19, depth.glm),
          style = "all2",
          dep.var.labels=c("Latent Depth", "Deep Alliance Dummy"),
          covariate.labels=c(
            "Alliance Leader POLITY",
            "Foreign Policy Concessions", "Number of Members",
            "Wartime Alliance", "Asymmetric Obligations",
            "Asymmetric Capability", "Non-Major Only",
            "Average Threat",
            "Foreign Policy Disagreement", "Start Year"
          ),
          keep.stat = c("n"), ci=TRUE, 
          star.char = c("", "", ""),
          notes = "95\\% Confidence Intervals in Parentheses.", 
          notes.append = FALSE,
          label = c("tab:depth-alt-models")
)


