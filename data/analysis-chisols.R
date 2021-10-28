# Joshua Alley
# Analysis with CHISOLS Variables

### Chisols data
chisols <- read.csv("data/CHISOLSstyr4_0.csv")
glimpse(chisols)
chisols <- select(chisols,
                  ccode, year, leader, affiliation, 
                  solschange, solschdum, 
                  dem, mil, per, mon, sp,
                  olig, hybrid) %>% # change in last x years
           group_by(ccode) %>%
           mutate(
             dem.dum = ifelse(dem == "1", 1, 0),
             solsch5 = zoo::rollsumr(solschange, k = 5, fill = NA),
             dem5 = zoo::rollsumr(dem.dum, k = 5, fill = NA),
             solsch10 = zoo::rollsumr(solschange, k = 10, fill = NA),
             dem10 = zoo::rollsumr(dem.dum, k = 5, fill = NA)
           )
table(chisols$solschdum)
table(chisols$per)
table(chisols$dem)
# positive correlation between democracy and sols changes
table(chisols$dem, chisols$solsch5)
table(chisols$dem, chisols$solsch10)
cor.test(chisols$dem5, chisols$solsch5)
cor.test(chisols$dem10, chisols$solsch10)


# add to alliance data 
chisols.data <- left_join(chisols, atop.cow.year)  %>%
  group_by(atopid) %>%
  filter(atopid > 0) %>% # remove no alliance obs
  group_by(atopid, year) %>% # group and summarize
  mutate(
    most.cap = ifelse(cinc == max(cinc, na.rm = TRUE), 1, 0),
    maxcap.solsch5 = ifelse(most.cap == 1, solsch5, 0),
    maxcap.solsch10 = ifelse(most.cap == 1, solsch10, 0),
    maxcap.ccode = ifelse(most.cap == 1, ccode, 0)
  ) %>% 
  summarize(
    # changes in leadership sols 
    maxcap.solsch5 = max(maxcap.solsch5, na.rm = TRUE),
    maxcap.solsch10 = max(maxcap.solsch10, na.rm = TRUE),
    maxcap.ccode = max(maxcap.ccode, na.rm = TRUE),
    # keep groups
    .groups = "keep"
  ) %>%
  select(atopid, maxcap.solsch5, maxcap.solsch10,
         maxcap.ccode, year) %>%
  left_join(key.data) %>% # defensive and offensive alliances
  mutate( # create logs
    log.solsch10 = log(maxcap.solsch10 + 1),
    log.solsch5 = log(maxcap.solsch5 + 1)
  ) %>% 
  filter(year == begyr) 


# check output
# shows more turnover in SOLS in electoral democracies
table(chisols.data$maxcap.solsch5)
table(chisols.data$maxcap.liedh, chisols.data$maxcap.solsch5)
table(chisols.data$maxcap.solsch10)
table(chisols.data$maxcap.liedh, chisols.data$maxcap.solsch10)
t.test(maxcap.solsch10 ~ maxcap.liedh, data = chisols.data) # outlier-driven, 


# plot depth against sols
ggplot(chisols.data, aes(x = maxcap.solsch5, y = latent.depth.mean,
                         color = factor(maxcap.liedh))) + 
                 geom_jitter(alpha = .75)
ggplot(chisols.data, aes(x = maxcap.solsch10, y = latent.depth.mean,
                         color = factor(maxcap.liedh))) + 
                  geom_jitter(alpha = .75)


# model it: replace lied with sols
# regress depth on sols change and constraints
# sols change as post-treatment variable for LIED
depth.reg.ch5 <- lm(latent.depth.mean ~
                       maxcap.solsch5 + maxcap.cons + 
                       econagg.dum + uncond.milsup +
                       fp.conc.index + num.mem + wartime + asymm + 
                       asymm.cap + non.maj.only +
                       mean.threat +
                       low.kap.sc + post45,
                     data = chisols.data)
summary(depth.reg.ch5)

# robust regression
depth.rlm.ch5 <- rlm(latent.depth.mean ~
                        maxcap.solsch5 + maxcap.cons + 
                        econagg.dum + uncond.milsup +
                        fp.conc.index + num.mem + wartime + asymm + 
                        asymm.cap + non.maj.only +
                        mean.threat +
                        low.kap.sc + post45,
                      data = chisols.data)
summary(depth.rlm.ch5)

# beta regression 
depth.breg.ch5 <- betareg(latent.depth.mean.rs ~ 
                             maxcap.solsch5 + maxcap.cons +
                             econagg.dum + uncond.milsup +
                             fp.conc.index + num.mem + wartime + asymm + 
                             asymm.cap + non.maj.only +
                             mean.threat +
                             low.kap.sc + post45,
                           data = chisols.data)
summary(depth.breg.ch5)



# sols changes in previous 10 years 
# Regression of depth on leadership change and constraints
depth.reg.ch10 <- lm(latent.depth.mean ~
                  maxcap.solsch10 + maxcap.cons + 
                  econagg.dum + uncond.milsup +
                  fp.conc.index + num.mem + wartime + asymm + 
                  asymm.cap + non.maj.only +
                  mean.threat +
                  low.kap.sc + post45,
                data = chisols.data)
summary(depth.reg.ch10)

# robust regression
depth.rlm.ch10 <- rlm(latent.depth.mean ~
                   maxcap.solsch10  + maxcap.cons + 
                   econagg.dum + uncond.milsup +
                   fp.conc.index + num.mem + wartime + asymm + 
                   asymm.cap + non.maj.only +
                   mean.threat +
                   low.kap.sc + post45,
                 data = chisols.data)
summary(depth.rlm.ch10)

# beta regression 
depth.breg.ch10 <- betareg(latent.depth.mean.rs ~ maxcap.solsch10 + maxcap.cons +
                            econagg.dum + uncond.milsup +
                            fp.conc.index + num.mem + wartime + asymm + 
                            asymm.cap + non.maj.only +
                            mean.threat +
                            low.kap.sc + post45,
                          data = chisols.data)
summary(depth.breg.ch10)


# beta regression with logged IV
depth.breg.lch10 <- betareg(latent.depth.mean.rs ~ log.solsch10 + maxcap.cons +
                             econagg.dum + uncond.milsup +
                             fp.conc.index + num.mem + wartime + asymm + 
                             asymm.cap + non.maj.only +
                             mean.threat +
                             low.kap.sc + post45,
                           data = chisols.data)
summary(depth.breg.lch10)
# beta regression with logged IV: five years
depth.breg.lch5 <- betareg(latent.depth.mean.rs ~ log.solsch5 + maxcap.cons +
                              econagg.dum + uncond.milsup +
                              fp.conc.index + num.mem + wartime + asymm + 
                              asymm.cap + non.maj.only +
                              mean.threat +
                              low.kap.sc + post45,
                            data = chisols.data)
summary(depth.breg.lch5)



# plot results 
plot(ggeffect(depth.breg.ch5, terms = c("maxcap.solsch5"),
              interval = "confidence"))
plot(ggeffect(depth.breg.ch10, terms = c("maxcap.solsch10"),
                          interval = "confidence"))

# plot coefficient estimates
modelplot(list(depth.breg.ch5, depth.breg.lch5,
               depth.breg.ch10, depth.breg.lch10),
          coef_map = c("maxcap.solsch5" = "SOLS Changes in 5 Years",
                       "log.solsch5" = "Log SOLS Changes in 5 Years",
                       "maxcap.solsch10" = "SOLS Changes in 10 Years",
                       "log.solsch10" = "Log SOLS Changes in 10 Years")) +
          scale_color_grey() +
          ggtitle("Previous Changes in Ruling Coalition and\n Alliance Treaty Depth")
ggsave("figures/ch-plot.png", height = 4, width = 6)



# leadership change and uncond support: no stat sig, small magnitude
uncond.glm.ch10 <- glm(uncond.milsup ~ 
                    maxcap.solsch10 + maxcap.cons + 
                    econagg.dum + 
                    fp.conc.index + num.mem + wartime + asymm +
                    asymm.cap + non.maj.only + mean.threat + 
                    low.kap.sc + post45,
                  family = binomial(link = "probit"),
                  data = chisols.data)
summary(uncond.glm.ch10)

uncond.glm.ch <- glm(uncond.milsup ~ 
                       maxcap.solsch5 + maxcap.cons + 
                       econagg.dum + 
                       fp.conc.index + num.mem + wartime + asymm +
                       asymm.cap + non.maj.only + mean.threat + 
                       low.kap.sc + post45,
                     family = binomial(link = "probit"),
                     data = chisols.data)
summary(uncond.glm.ch10)
