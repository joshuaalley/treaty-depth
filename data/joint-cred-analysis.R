# Joshua Alley
# Analysis of multiple sources of alliance credibility

# data is loaded by alliance-measures.R and
# depth-analysis.R 

# glm model of unconditional military support
# full democ
uncond.glm.dem <- glm(uncond.milsup ~ 
                    maxcap.democ + econagg.dum + 
                    fp.conc.index + num.mem + wartime + asymm +
                    asymm.cap + non.maj.only + mean.threat + 
                    low.kap.sc + post45,
                  family = binomial(link = "probit"),
                  data = key.data)
summary(uncond.glm.dem)

# glm model of unconditional military support
uncond.glm <- glm(uncond.milsup ~ 
                       maxcap.cons + maxcap.lied + 
                       econagg.dum + 
                       fp.conc.index + num.mem + wartime + asymm +
                       asymm.cap + non.maj.only + mean.threat + 
                       low.kap.sc + post45,
                     family = binomial(link = "probit"),
                     data = key.data)
summary(uncond.glm)


# glm model of economic issue linkages
linkage.glm <- glm(econagg.dum ~ 
                     maxcap.cons + maxcap.lied +
                    fp.conc.index + num.mem + wartime + asymm +  
                     asymm.cap + non.maj.only + mean.threat +
                    low.kap.sc + post45,
                  family = binomial(link = "probit"),
                  data = key.data)
summary(linkage.glm)


### Joint analysis of unconditional military support and depth
# use GJRM, as it allows for correlated errors

### Fit a model with democratic instituions of most capable member

uncond.formula <- uncond.milsup ~ maxcap.cons + maxcap.lied + 
  econagg.dum + 
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only + 
  s(mean.threat) + low.kap.sc + s(begyr)

depth.formula <- latent.depth.mean.rs ~ maxcap.cons + maxcap.lied + 
  econagg.dum +
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only + 
  s(mean.threat) + low.kap.sc + s(begyr)

# model the dependence between the error terms as a function of start year
theta.formula <- ~ s(begyr) + maxcap.cons + maxcap.lied + num.mem
eq.sigma <- ~ 1



copulas <- c("N", "C0", "C90", "C180", "C270", "J0", "J90", "J180", "J270",
             "G0", "G90", "G180", "G270", "F", "AMH", "FGM", "T", "PL", "HO")

# Create a list of models
gjrm.models <- vector(mode = "list", length = length(copulas))

# FISK (log-logistic), inverse gaussian, Dagum and beta distributions are best
# in terms of residual fit
# Beta has the lowest AIC.
for(i in 1:length(copulas)){
  gjrm.models[[i]]  <- gjrm(list(uncond.formula, depth.formula,
                                 eq.sigma, theta.formula), 
                            data = key.data,
                            margins = c("probit", "BE"),
                            Model = "B",
                            BivD = copulas[i]
  )
}
aic.gjrm <- lapply(gjrm.models, AIC)
aic.gjrm

# NB for interpretation: smoothed terms
copulas[17] 
joint.gjrm <- gjrm(list(uncond.formula, depth.formula,
                        eq.sigma, theta.formula), 
                   data = key.data,
                   margins = c("probit", "BE"),
                   Model = "B",
                   BivD = "T"
)
conv.check(joint.gjrm)

AIC(joint.gjrm)
summary(joint.gjrm)
post.check(joint.gjrm)

# Plot smooths
plot(joint.gjrm, eq = 1, seWithMean = TRUE,
     shade = TRUE, pages = 1) # smoothed terms 
plot(joint.gjrm, eq = 2, seWithMean = TRUE,
     shade = TRUE, pages = 1) # smoothed terms 
plot(joint.gjrm, eq = 4, seWithMean = TRUE,
     shade = TRUE, pages = 1) # smoothed terms 



### Lexical Index GJRM results 
summary.depth.gjrm <- summary(joint.gjrm)

# tabulate the results 
summary.depth.gjrm[["tableP1"]] # uncond milsup
summary.depth.gjrm[["tableP2"]] # depth 

# Combine all the results in a single table. 
# Start with unconditional table
uncond.tab <- as.data.frame(rbind(summary.depth.gjrm[["tableP1"]][, 1:2],
                                  summary.depth.gjrm[["tableNP1"]][, c(1, 3)]
))
uncond.tab$variable <- c("(Intercept)",  
                         "Executive Constraints", 
                         "Lexical Index of Democracy",
                         "Economic Issue Linkage", 
                         "FP Concessions", "Number of Members", 
                         "Wartime Alliances", "Asymmetric Obligations",
                         "Asymmetric Capability", "Non-Major Only", "FP Disagreement",
                         "s(Mean Threat)", "s(Start Year)")


# table for treaty depth
depth.tab <- as.data.frame(rbind(summary.depth.gjrm[["tableP2"]][, 1:2],
                                 summary.depth.gjrm[["tableNP2"]][, c(1, 3)]
))
depth.tab$variable <- c("(Intercept)",
                        "Executive Constraints",
                        "Lexical Index of Democracy",
                        "Economic Issue Linkage",
                        "FP Concessions", "Number of Members", 
                        "Wartime Alliances", "Asymmetric Obligations",
                        "Asymmetric Capability", "Non-Major Only", "FP Disagreement",
                        "s(Mean Threat)", "s(Start Year)")

joint.tab <- full_join(uncond.tab, depth.tab, by = "variable")
joint.tab <- as.data.frame(joint.tab[, c(3, 1, 2, 4, 5)])


# Tabulate all the equations together
# create a header
head.xtab <- list()
head.xtab$pos <- list(-1)
head.xtab$command <- paste0(paste0('& \\multicolumn{2}{c}{Uncond. Mil. Support} & \\multicolumn{2}{c}{Latent Depth}',
                                   collapse=''), '\\\\')



print(
  xtable(joint.tab, 
         caption = c("Results from a joint generalized regression model of treaty depth and unconditional military support in 
         offensive and defensive alliances from 1816 to 2007. 
                     All smoothed terms report the effective degrees of freedom and the chi-squared term. 
                     The unconditional military support model is a binomial GLM with a probit link function. 
                     The treaty depth model is a beta regression. 
                     I model the error correlation between the two processes with a T copula."),
         label = c("tab:gjrm-res"), auto = TRUE),
  add.to.row = head.xtab, 
  include.rownames = FALSE)




### Model process with aggregate indicator

# Use a beta regression with rescaled depth 
beta.reg.depth.agg <- betareg(latent.depth.mean.rs ~ 
                            maxcap.democ + econagg.dum +
                            fp.conc.index + num.mem + wartime + asymm + 
                            asymm.cap + non.maj.only + 
                            mean.threat + low.kap.sc + post45, data = key.data)
summary(beta.reg.depth.agg)



# joint model
uncond.formula.agg <- uncond.milsup ~ maxcap.democ +
  econagg.dum + 
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only + 
  s(mean.threat) + low.kap.sc + s(begyr)

depth.formula.agg <- latent.depth.mean.rs ~ maxcap.democ + 
  econagg.dum +
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only + 
  s(mean.threat) + low.kap.sc + s(begyr)

theta.formula.agg <- ~ s(begyr) + maxcap.democ + num.mem

# Create a list of models
gjrm.models.agg <- vector(mode = "list", length = length(copulas))

# FISK (log-logistic), inverse gaussian, Dagum and beta distributions are best
# in terms of residual fit
# Beta has the lowest AIC.
for(i in 1:length(copulas)){
  gjrm.models.agg[[i]]  <- gjrm(list(uncond.formula.agg, depth.formula.agg,
                                     eq.sigma, theta.formula.agg), 
                                 data = key.data,
                                 margins = c("probit", "BE"),
                                 Model = "B",
                                 BivD = copulas[i]
  )
}
aic.gjrm.agg <- lapply(gjrm.models.agg, AIC)
aic.gjrm.agg

# NB for interpretation: smoothed terms
copulas[17] 
joint.gjrm.agg <- gjrm.models.agg[[17]] 
conv.check(joint.gjrm.agg)
AIC(joint.gjrm.agg)
summary(joint.gjrm.agg)


# Plot smooths
plot(joint.gjrm.agg, eq = 1, seWithMean = TRUE,
     shade = TRUE, pages = 1) # smoothed terms 
plot(joint.gjrm.agg, eq = 2, seWithMean = TRUE,
     shade = TRUE, pages = 1) # smoothed terms 
plot(joint.gjrm.agg, eq = 4, seWithMean = TRUE,
     shade = TRUE, pages = 1) # smoothed terms 





### Trivariate model with issue linkages
depth.formula.tri <- deep.alliance ~ 
  maxcap.lied + maxcap.cons +
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only +
  mean.threat + low.kap.sc + post45

uncond.formula.tri <- uncond.milsup ~ 
  maxcap.lied + maxcap.cons +
  fp.conc.index + num.mem + wartime + asymm + asymm.cap + non.maj.only +
  mean.threat + low.kap.sc + post45

linkage.formula <- econagg.dum ~
  maxcap.lied + maxcap.cons +
  fp.conc.index + num.mem + wartime + asymm +  asymm.cap + non.maj.only +
  mean.threat + low.kap.sc + post45

joint.gjrm.tri <- gjrm(list(uncond.formula.tri, depth.formula.tri, 
                                     linkage.formula), data = key.data,
                                margins = c("probit", "probit", "probit"),
                                Model = "T", 
                                BivD = "N")

# No difference in AIC or convergence across copulas
conv.check(joint.gjrm.tri)
AIC(joint.gjrm.tri)
summary(joint.gjrm.tri)



# switch off gjrm plots 
dev.off()

