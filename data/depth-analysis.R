# Joshua Alley
# Texas A&M University 
# Analysis of the sources of alliance treaty depth


# focus on the posterior mean in this script: include uncertainty in later scripts

### Analysis focusing on alliances with military support


# Regression of depth on other alliance characteristics
depth.reg <- lm(latent.depth.mean ~ avg.democ + econagg.dum + uncond.milsup +
                  fp.conc.index + num.mem + wartime + asymm + asymm.cap +
                  low.kap.sc + begyr,
                data = atop.milsup)
summary(depth.reg)
plot(density(depth.reg$residuals))


# Add superpower membership to proxy for nuclear umbrella
depth.reg.super <- lm(latent.depth.mean ~ avg.democ + econagg.dum + uncond.milsup +
                  fp.conc.index + num.mem + wartime + asymm + asymm.cap +
                  low.kap.sc + begyr + us.mem + ussr.mem,
                data = atop.milsup)
summary(depth.reg.super)


# Non-normal residuals imply robust regression may be worthwhile 
depth.reg.rob <- rlm(latent.depth.mean ~ avg.democ + econagg.dum + uncond.milsup +
                  fp.conc.index + num.mem + wartime + asymm + asymm.cap +
                  low.kap.sc + begyr + us.mem + ussr.mem,
                data = atop.milsup)
summary(depth.reg.rob)
plot(depth.reg.rob$residuals, depth.reg.rob$w)


# Logit with deep alliance dummy: depth above median value
depth.glm <- glm(deep.alliance ~ avg.democ + econagg.dum + uncond.milsup +
                       fp.conc.index + num.mem + wartime + asymm + asymm.cap +
                       low.kap.sc + begyr + us.mem + ussr.mem,
                     family = binomial(link = "logit"),
                     data = atop.milsup)
summary(depth.glm)



# Split analysis: before and after 1945
atop.milsup.pre45 <- filter(atop.milsup, begyr <= 1945)
atop.milsup.post45 <- filter(atop.milsup, begyr > 1945)


# analysis before 1945 
depth.reg.pre45 <- rlm(latent.depth.mean ~ avg.democ + econagg.dum + uncond.milsup +
                       fp.conc.index + num.mem + wartime + asymm + asymm.cap +
                       low.kap.sc,
                     data = atop.milsup.pre45)
summary(depth.reg.pre45)

# analysis after 1945 
depth.reg.post45 <- rlm(latent.depth.mean ~ avg.democ + econagg.dum + uncond.milsup +
                         fp.conc.index + num.mem + wartime + asymm + asymm.cap +
                         low.kap.sc + us.mem + ussr.mem,
                       data = atop.milsup.post45)
summary(depth.reg.post45)
