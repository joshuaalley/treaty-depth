# Joshua Alley
# Texas A&M University 
# Analysis of the sources of alliance treaty depth


# focus on the posterior mean in this script: include uncertainty in later scripts


# data on non-major only treaties for viewing
atop.milsup.nonmaj <- filter(atop.milsup, non.maj.only == 1) %>%
                       select(atopid, latent.depth.mean, avg.democ, econagg.dum, uncond.milsup,
                                fp.conc.index, num.mem, wartime, asymm, 
                                asymm.cap, non.maj.only,
                                low.kap.sc, begyr, us.mem, ussr.mem)

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
                  fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only +
                  low.kap.sc + begyr + us.mem + ussr.mem,
                data = atop.milsup)
summary(depth.reg.super)


# Non-normal residuals imply robust regression may be worthwhile 
depth.reg.rob <- rlm(latent.depth.mean ~ avg.democ + econagg.dum + uncond.milsup +
                  fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only +
                  low.kap.sc + begyr + us.mem + ussr.mem,
                data = atop.milsup)
summary(depth.reg.rob)
plot(depth.reg.rob$residuals, depth.reg.rob$w)


# Logit with deep alliance dummy: depth above median value
depth.glm <- glm(deep.alliance ~ avg.democ + econagg.dum + uncond.milsup +
                       fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only +
                       low.kap.sc + begyr + us.mem + ussr.mem,
                     family = binomial(link = "probit"),
                     data = atop.milsup)
summary(depth.glm)



### Split analysis: before and after 1945
atop.milsup.pre45 <- filter(atop.milsup, begyr <= 1945)
atop.milsup.post45 <- filter(atop.milsup, begyr > 1945)


# look at key covariates
# only non-major powers 
table(atop.milsup.pre45$non.maj.only)
table(atop.milsup.post45$non.maj.only)
# Alliance democracy 
summary(atop.milsup.pre45$avg.democ)
summary(atop.milsup.post45$avg.democ)
# alliance size 
summary(atop.milsup.pre45$num.mem)
summary(atop.milsup.post45$num.mem)
# alliance size: use a bilateral dummy instead 
summary(atop.milsup.pre45$bilat)
summary(atop.milsup.post45$bilat)
# asymmetric capability
table(atop.milsup.pre45$asymm.cap)
table(atop.milsup.post45$asymm.cap)
# unconditional military support
table(atop.milsup.pre45$uncond.milsup)
table(atop.milsup.post45$uncond.milsup)
# depth
summary(atop.milsup.pre45$latent.depth.mean)
summary(atop.milsup.post45$latent.depth.mean)


# Split uncond milsup by non-major only
table(atop.milsup$non.maj.only, atop.milsup$uncond.milsup)
table(atop.milsup.pre45$non.maj.only, atop.milsup.pre45$uncond.milsup)
table(atop.milsup.post45$non.maj.only, atop.milsup.post45$uncond.milsup)


table(atop.milsup.pre45$non.maj.only, atop.milsup.pre45$asymm.cap)
table(atop.milsup.post45$non.maj.only, atop.milsup.post45$asymm.cap)

t.test(atop.milsup.pre45$latent.depth.mean ~ atop.milsup.pre45$uncond.milsup)
t.test(atop.milsup.post45$latent.depth.mean ~ atop.milsup.post45$uncond.milsup)

t.test(atop.milsup.pre45$latent.depth.mean ~ atop.milsup.pre45$non.maj.only)
t.test(atop.milsup.post45$latent.depth.mean ~ atop.milsup.post45$non.maj.only)


# analysis before 1945 
# depth 
depth.reg.pre45 <- rlm(latent.depth.mean ~ avg.democ + econagg.dum + uncond.milsup +
                       fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only +
                       low.kap.sc,
                     data = atop.milsup.pre45)
summary(depth.reg.pre45)


# unconditional military support 
uncond.glm.pre45 <- glm(uncond.milsup ~ avg.democ + econagg.dum +
                    fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only +
                    low.kap.sc,
                  family = binomial(link = "probit"),
                  data = atop.milsup.pre45)
summary(uncond.glm.pre45)


# analysis after 1945 
# depth 
depth.reg.post45 <- rlm(latent.depth.mean ~ avg.democ + econagg.dum + uncond.milsup +
                         fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only +
                         low.kap.sc + us.mem + ussr.mem,
                       data = atop.milsup.post45)
summary(depth.reg.post45)


# unconditional military support 
# if compare non-maj only to (1) symmetric maj  and asymm
uncond.glm.post45 <- glm(uncond.milsup ~ avg.democ + econagg.dum +
                          fp.conc.index + num.mem + wartime + asymm + non.maj.only +
                          low.kap.sc,
                        family = binomial(link = "probit"),
                        data = atop.milsup.post45)
summary(uncond.glm.post45)


# analysis after 1919 a la Mattes 2012 
atop.milsup.post19 <- filter(atop.milsup, begyr >= 1919)
depth.reg.post19 <- rlm(latent.depth.mean ~ avg.democ + econagg.dum + uncond.milsup +
                          fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only +
                          low.kap.sc + us.mem + ussr.mem,
                        data = atop.milsup.post45)
summary(depth.reg.post19)
