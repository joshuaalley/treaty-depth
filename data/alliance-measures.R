# Joshua Alley
# Summarizing and measuring Key alliance characteristics


# Load ATOP v4 alliance-level data 
atop <- read.csv("data/atop-alliance-level.csv")
atop.mem.full <- read.csv("data/atop-member-level.csv")


# identify non-aggression only pacts
# Also, recode arms requirements and military aid variables from ATOP into dummy 
# variables that capture conditions where increases in arms spending are likely
atop <- mutate(atop, nonagg.only = ifelse((nonagg == 1 & offense != 1
                                           & defense != 1 & 
                                             consul != 1 & neutral != 1), 1 , 0),
               armred.rc = ifelse(armred == 2, 1, 0),
               milaid.rc = ifelse(milaid >= 2, 1, 0)
)


# Conditional alliance commtiments
table(atop$conditio) # condititional
table(atop$offcon)
table(atop$offcon, atop$offense)
table(atop$defcon)
table(atop$defcon, atop$defense)


# overlap between offensive and defensive
table(atop$offense, atop$defense)
table(atop$offcon, atop$defcon)


# Generate counts of conditions attached to each type of commitment
# Use ATOP member-level data because conditions vary across members
atop.mem.full <- atop.mem.full %>%
    mutate(
      off.cond.count = offcoadv + offcoloc + offcocon + offconum + offcodem,
      def.cond.count = defcoadv + defcoloc + defcocon + defconum + defcodem + defconpr,
      loc.cond = ifelse(offcoloc == 1 | defcoloc == 1, 1, 0),
      adv.cond = ifelse(offcoadv == 1 | defcoadv == 1, 1, 0),
      con.cond = ifelse(offcocon == 1 | defcocon == 1, 1, 0),
      num.cond = ifelse(offconum == 1 | defconum == 1, 1, 0),
      dem.cond = ifelse(offcodem == 1 | defcodem == 1, 1, 0),
      milsup.cond = loc.cond + adv.cond + con.cond + num.cond + dem.cond + defconpr,
      mil.support = ifelse(offense == 1 | defense == 1, 1, 0)
    )

ggplot(filter(atop.mem.full, offense == 1), aes(x = off.cond.count)) + geom_bar() # offensive conditions
ggplot(filter(atop.mem.full, defense == 1), aes(x = def.cond.count)) + geom_bar() # defensive conditions

ggplot(filter(atop.mem.full, mil.support == 1), aes(x = milsup.cond)) + geom_bar() # total conditions on military support
ggplot(atop.mem.full, aes(x = total.cond)) + geom_bar() # total conditions on any kind of support

# Precise numbers of offense and defense pacts with number of conditions
table(atop.mem.full$def.cond.count, atop.mem.full$defense)
table(atop.mem.full$off.cond.count, atop.mem.full$offense)

# summarize conditional variables
atop.cond.count <- atop.mem.full %>% 
                    group_by(atopid) %>%
                     filter(mil.support == 1) %>%
                      summarize(
                        conditions = min(milsup.cond, na.rm = TRUE)
                      ) %>%
                      mutate(
                        uncond.oblig = 4 - conditions
                      )
glimpse(atop.cond.count)

# Generate an unconditional variable for defense and offense
# Generate other dummy indicators to produce a cumulative indicator of alliance scope
atop <- atop %>%
  mutate(
    uncond = ifelse(conditio == 0, 1, 0), # unconditional dummy
    uncond.def = ifelse(defcon == 0 & defense == 1, 1, 0), # unconditional defense dummy
    uncond.off = ifelse(offcon == 0 & offense == 1, 1, 0), # unconditional offense dummy 
    agpro.mult = ifelse(agprois == 5, 1, 0), # promise for additional agreements on multiple issues
    io.form = ifelse(organ1 > 0, 1, 0), # promise to form  an IO
    ecaid.dum = ifelse(ecaid > 0, 1, 0), # dummy indicator of economic aid
    
    uncond.milsup = ifelse(conditio == 0 & (offense == 1 | defense == 1), 1, 0), # unconditional military support
    
    milaid.dum = ifelse(milaid > 0, 1, 0), # dummy indicator of military aid
    base.dum = ifelse(base > 0, 1, 0),
    milcon.dum = ifelse(milcon > 0, 1, 0),
    subord.dum = ifelse(subord > 0, 1, 0),
    
    trade.dum = ifelse(ecaid == 3, 1, 0), # dummy indicator of economic aid
    compag.econ = ifelse(compag == 2, 1, 0),
    compag.mil = ifelse(compag == 1, 1, 0),
    agpro.econ = ifelse(agprois == 2, 1, 0),
    organ1.dum = ifelse(organ1 > 0, 1, 0),
    
    notaiden.dum = ifelse(notaiden > 0, 1, 0),
    interv.dum = ifelse(interv > 0, 1, 0)
 ) %>% 
  rowwise() %>%
  mutate(scope.index = sum(uncond.milsup, milaid.dum, ecaid.dum, io.form, agpro.mult, na.rm = TRUE),
         milcor.index = sum(milcon.dum, milaid.dum, base.dum, intcom, organ1.dum, compag.mil, na.rm = TRUE),
         econagg.index = sum(trade.dum, nomicoop, compag.econ, agpro.econ, na.rm = TRUE),
         econagg.dum = ifelse(econagg.index > 0, 1, 0),
         fp.conc.index = sum(noothall, notaiden.dum, thirdcom, divgains, interv.dum, na.rm = TRUE))



# Check variables: 
table(atop$uncond.def, atop$defcon)
table(atop$uncond.off, atop$offcon)
table(atop$offcon, atop$uncond.milsup)
table(atop$defcon, atop$uncond.milsup)
table(atop$uncond, atop$uncond.def) # 123/413 unconditional commitments are defense pacts
table(atop$uncond, atop$uncond.off) # 13/413 unconditional commitments are offense pacts
table(atop$uncond.milsup)
table(atop$scope.index, atop$uncond.milsup)

# Check indices
table(atop$milcor.index)
table(atop$econagg.index)
table(atop$fp.conc.index)
table(atop$milcor.index, atop$econagg.index)
table(atop$fp.conc.index, atop$milcor.index)
table(atop$fp.conc.index, atop$econagg.index)


# aggregate index of scope
table(atop$scope.index)
ggplot(atop, aes(x = scope.index)) + geom_bar()


# Create variables for US and USSR membershipin Cold War treaties
ussr.mem <- apply(atop[, 72:130], 1, function(x) ifelse(x == 365, 1, 0))
ussr.mem <- t(ussr.mem)
atop$ussr.mem <- rowSums(ussr.mem, na.rm = TRUE)

# US
us.mem <- apply(atop[, 72:130], 1, function(x) ifelse(x == 2, 1, 0))
us.mem <- t(us.mem)
atop$us.mem <- rowSums(us.mem, na.rm = TRUE)

# Remove the US and Russian membership matrices from the environment
rm(ussr.mem)
rm(us.mem)

# limit US and USSR to Cold War Treaties
atop$us.mem[atop$begyr < 1945] <- 0
atop$ussr.mem[atop$begyr < 1945 | atop$begyr > 1991] <- 0


# create a superpower membership dummy
atop$super.mem <- ifelse(atop$us.mem == 1 | atop$ussr.mem == 1, 1, 0)



# count number of major powers in an alliance- need to capture symmetric major power treaties
atop.mem.list <- atop %>%
  select(c(begyr, (72:130)))
atop.mem.list$mp.count <- rowSums(
                (atop.mem.list == 2 & atop.mem.list$begyr > 1898) | # US 
                (atop.mem.list == 255 & atop.mem.list$begyr <= 1918)  | # Germany: 1816 to 1918
                (atop.mem.list == 255 & atop.mem.list$begyr >= 1925 & atop.mem.list$begyr <= 1945) | # Germany 1925 to 1945
                (atop.mem.list == 255 & atop.mem.list$begyr >= 1991) | # Germany 1991 to present  
                (atop.mem.list == 200) | # UK
                (atop.mem.list == 365 & atop.mem.list$begyr <= 1917) | # Russia: 1816 to 1917
                (atop.mem.list == 365 & atop.mem.list$begyr >= 1922) | # Russia: 1922 to present
                (atop.mem.list == 220 & atop.mem.list$begyr <= 1940) | # France 1816 to 1940
                (atop.mem.list == 220 & atop.mem.list$begyr >= 1945) | # France 1945 to present 
                (atop.mem.list == 710 & atop.mem.list$begyr >= 1950) | # China
                (atop.mem.list == 300 & atop.mem.list$begyr <= 1918) | # Austria-Hungary
                (atop.mem.list == 325 & atop.mem.list$begyr >= 1860 & atop.mem.list$begyr <= 1943) | # Italy
                (atop.mem.list == 740 & atop.mem.list$begyr <= 1945 & atop.mem.list$begyr >= 1895) | # Japan 1895 to 1940
                (atop.mem.list == 740 & atop.mem.list$begyr >= 1991), 
                na.rm = TRUE
                )

# assign count to ATOP data
atop$mp.count <- atop.mem.list$mp.count
rm(atop.mem.list)
             


# count number of members: non-missing membership variables
atop$num.mem <-  apply(atop[, 72:130], 1, function(x) sum(!is.na(x)))



# Generate a measure of asymmetric capability
atop <- atop %>%
  mutate(
    non.maj.only = 
      ifelse(
        mp.count == 0 & super.mem == 0, 1, 0
      ),
    asymm.cap = 
      ifelse(
        super.mem == 1 | # super powers always generate asymm cap (no alliance only with each other)
          (mp.count > 0 & (mp.count < num.mem)), # major powers with non-major allies
        1, 0
      )
  )


# Generate a measure of FP similarity in initial year of alliance 
all.fp.sim <- read.csv("data/all-fp-sim.csv")
all.fp.sim <- all.fp.sim[complete.cases(all.fp.sim), ] # missing data on all measures of FP similarity 

# Get first observation for each ATOPID
all.fp.sim <- all.fp.sim %>%
  group_by(atopid) %>%
  mutate(
    yr1 = min(year)
  ) 

all.fpsim.first <- filter(all.fp.sim, year == yr1) %>% 
  group_by() %>% 
  select(-c(year, yr1))  


# Add measures of FP similiarity in first year observed
atop <- left_join(atop, all.fpsim.first)



### Generate a latent measure of depth for alliances with military support

# Split military support data
atop.milsup <- filter(atop, offense == 1 | defense == 1) 

# Turn dummy indicators into factors in a separate dataset
atop.depth <- select(atop.milsup,
                     intcom, compag.mil,  
                     milaid, milcon, base, 
                     organ1, subord.dum, contrib) 
atop.depth <- as.data.frame(atop.depth)
for(i in 1:ncol(atop.depth)){
  atop.depth[, i] <- as.ordered(atop.depth[, i])
}

# Use Murray BFA approach
latent.depth <- bfa_copula(~ intcom + compag.mil +
                             milaid + milcon + base + 
                             organ1 + subord.dum + contrib, 
                           data = atop.depth, num.factor = 1,
                           restrict = list(c("intcom", 1, ">0")),
                          factor.scales = FALSE,
                          keep.scores = TRUE, loading.prior = "normal", 
                          px = TRUE, imh.iter = 1000, imh.burn = 1000,
                          nburn = 20000, nsim = 30000, thin = 30, 
                          print.status = 2000)

# Little bit of diagnosis
# plot(get_coda(latent.depth))

# Diagnosis of convergence with coda
lcap.sam <- get_coda(latent.depth, scores = TRUE)
effectiveSize(lcap.sam)
diag.geweke  <- geweke.diag(lcap.sam)

# Plot to see if Geweke Z-scores appear to be from Normal(0, 1) distribution
par(mfrow=c(1, 1))
plot(density(diag.geweke$z))
lines(density(rnorm(10000, 0, 1)))


# Express correlations: check single latent dimension
cor.ld1 <- cor_samp(latent.depth)
dimnames(cor.ld1) <- list(colnames(atop.depth),
                          colnames(atop.depth),
                          paste0("sample", seq(1:1000))
)
ld1.samp <- apply(
  apply(cor.ld1, 3L, c),
  1, 
  mean)
ld1.vars <- expand.grid(dimnames(cor.ld1)[1:2])
cor.ld1.mat <- data.frame(ld1.vars, ld1.samp) %>%
  pivot_wider(id_cols = c(Var1),
              names_from = Var2,
              values_from = ld1.samp)
cor.ld1.mat # print correlation 
# remove var1 for decomposition
cor.ld1.mat <- cor.ld1.mat %>%
  select(-Var1)



ev <- eigen(cor.ld1.mat) # get eigenvalues
ap <- nFactors::parallel(subject=nrow(atop.depth),
                         var=ncol(atop.depth),
                         rep=100,cent=.05)
nS <- nFactors::nScree(x=ev$values, aparallel=ap$eigen$qevpea)
nFactors::plotnScree(nS)


# plot density of factors
# Create a dataset of factors
latent.factors <- cbind.data.frame(c("Integrated Command", "Companion Mil. Agreement", 
                                     "Military Aid", "Policy Coordination", "Bases",
                                     "Formal IO", "Subordination", "Specific Contrib"),
                                   latent.depth[["post.loadings.mean"]],
                                   sqrt(latent.depth[["post.loadings.var"]])
)
colnames(latent.factors) <- c("var", "mean", "sd")

# plot factor loadings 
latent.factors <- arrange(latent.factors, desc(mean)) 
latent.factors$var<- reorder(latent.factors$var, latent.factors$mean)

loadings.plot <- ggplot(latent.factors, aes(x = mean, y = var)) + 
                   geom_point(size = 2) +
                   geom_errorbarh(aes(xmin = mean - 2*sd, 
                     xmax = mean + 2*sd),
                     height = .2, size = 1) +
                   geom_vline(xintercept = 0) +
                   labs(x = "Factor Loading", y = "Variable") +
                   ggtitle("Factor Loadings in Latent Variable Model") +
                   theme_bw()
loadings.plot 

# get posterior scores of latent factor: mean and variance
atop.milsup$latent.depth.mean <- as.numeric(t(latent.depth$post.scores.mean))
atop.milsup$latent.depth.var <- as.numeric(t(latent.depth$post.scores.var))
atop.milsup$latent.depth.sd <- sqrt(atop.milsup$latent.depth.var)


# Summarize latent depth: treaties with military support only
# weakest is 1870 France-UK offense/neutrality pact- meant to ensure Belgian neutrality
# median is ATOP 1180- UK, Fr and Austria during Crimean war

# 289 treaties with some promise of military support 
nrow(atop.milsup)

# histogram
ls.hist <- ggplot(atop.milsup, aes(x = latent.depth.mean)) + geom_histogram() +
  theme_classic() + labs(x = "Mean Latent Depth", y = "Treaties")
ls.hist


# depth against year of formation for treaties with military support
# Add error bars to plot
ls.styear <- ggplot(atop.milsup, aes(x = begyr, y = latent.depth.mean)) +
  geom_errorbar(aes(ymin = latent.depth.mean - latent.depth.sd, 
                    ymax = latent.depth.mean + latent.depth.sd,
                    width=.01), position = position_dodge(0.1)) +
  geom_point(position = position_dodge(0.1)) +
  labs(x = "Start Year", y = "Latent Depth of Treaty") +
  ggtitle("Latent Depth Measure by Start Year of Treaty") +
  theme_classic()
ls.styear
# Combine loadings and styear plot
grid.arrange(loadings.plot, ls.styear, ncol = 1)
depth.sum <- arrangeGrob(loadings.plot, ls.styear, ncol = 1)
ggsave("figures/loadings-measure.png",
       depth.sum,
       height = 6, width = 8)


# 171 alliances with depth above -.6 
sum(atop.milsup$latent.depth.mean > -.6)
summary(atop.milsup$latent.depth.mean)

# highlight NATO
atop.milsup %>% 
  mutate(NATO = ifelse(atopid == 3180, T, F)) %>% 
  ggplot(aes(x = begyr, y = latent.depth.mean, color = NATO)) +
  geom_point() +
  scale_color_manual(values = c('#595959', 'red'))



# Look at correlation between FP similarity and depth
cor.test(atop.milsup$mean.kap.sc, atop.milsup$latent.depth.mean)
cor.test(atop.milsup$low.kap.sc, atop.milsup$latent.depth.mean)

ggplot(atop.milsup, aes(x = mean.kap.sc, y = latent.depth.mean)) + 
  geom_point() + theme_classic()

# t-test w/ FP disagreement: higher FP agreement w/ economic agreements 
t.test(atop.milsup$mean.kap.sc ~ atop.milsup$econagg.dum)
t.test(atop.milsup$low.kap.sc ~ atop.milsup$econagg.dum)


# Plot alliance depth against size
cor.test(atop.milsup$num.mem, atop.milsup$latent.depth.mean)
ggplot(atop.milsup, aes(x = num.mem, y = latent.depth.mean)) +
  geom_point()


# Add democracy data
# load data: summary of key vars in start year
alliance.democ <- read.csv("data/alliance-democ.csv")


# combine alliance democ and ATOP milsup data 
atop.milsup <- left_join(atop.milsup, alliance.democ)
# also add count of conditions and unconditional obligations
atop.milsup <- left_join(atop.milsup, atop.cond.count)

# Create a deep alliance dummy
atop.milsup$deep.alliance <- ifelse(atop.milsup$latent.depth.mean > median(atop.milsup$latent.depth.mean),
                                    1, 0)

# Create a conditional military support dummy
atop.milsup$cond.milsup <- ifelse(atop.milsup$uncond.milsup == 0, 1, 0)
table(atop.milsup$cond.milsup, atop.milsup$uncond.milsup)


# Quick tabulation of Leeds and Anac Military institutionalization measure
table(atop.milsup$milinst)


# add a variable splitting the democracy variable at 0
summary(atop.milsup$avg.democ)
atop.milsup$democ.pos <- ifelse(atop.milsup$avg.democ > 1, 1, 0)
table(atop.milsup$democ.pos)


# Address max democ -Inf issue
# comes from all NA
atop.milsup$maxcap.open[atop.milsup$maxcap.open == -Inf] <- NA

atop.milsup$maxcap.comp.lied[atop.milsup$maxcap.comp.lied == -Inf] <- NA
atop.milsup$maxcap.open.lied[atop.milsup$maxcap.open.lied == -Inf] <- NA
atop.milsup$maxcap.lied[atop.milsup$maxcap.lied == -Inf] <- NA

atop.milsup$maxcap.cont.std[atop.milsup$maxcap.cont.std == -Inf] <- NA

atop.milsup$maxcap.poly[atop.milsup$maxcap.poly == -Inf] <- NA

# Clean up the maximum democracy by capability
select(atop.milsup, atopid, maxcap.democ.max, maxcap.democ.min) %>% 
  filter(maxcap.democ.max == maxcap.democ.min)
atop.milsup$maxcap.democ.max[atop.milsup$maxcap.democ.max < 0] <- 0 # Maximum below 0
atop.milsup$maxcap.democ.min[atop.milsup$maxcap.democ.min > 0] <- 0 # minimum above 0
select(atop.milsup, atopid, maxcap.democ.max, maxcap.democ.min) %>% 
  filter(maxcap.democ.max == maxcap.democ.min)


# create measure with polity score of most capable 
# Add zero in one column to max or min in the other 
atop.milsup$maxcap.democ <- atop.milsup$maxcap.democ.min + atop.milsup$maxcap.democ.max

summary(atop.milsup$maxcap.democ)
ggplot(atop.milsup, aes(x = maxcap.democ)) + geom_histogram()


summary(atop.milsup$maxcap.rec)
atop.milsup$maxcap.rec[atop.milsup$maxcap.rec == -Inf] <- 0
summary(atop.milsup$maxcap.comp)
atop.milsup$maxcap.comp[atop.milsup$maxcap.comp == -Inf] <- 0
summary(atop.milsup$maxcap.comp)
atop.milsup$maxcap.cons[atop.milsup$maxcap.cons == -Inf] <- 0


# Create a post-45 dummy
atop.milsup$post45 <- ifelse(atop.milsup$begyr > 1945, 1, 0)


# tabulate raw data
t.test(atop.milsup$latent.depth.mean ~ atop.milsup$maxcap.liedh)
cor.test(atop.milsup$latent.depth.mean, atop.milsup$maxcap.lied)
# boxplot depth by electoral comp 
depth.boxplot <- filter(atop.milsup, !is.na(maxcap.liedh)) %>%
                 ggplot(aes(x = factor(maxcap.liedh), y = (latent.depth.mean))) +
                  geom_boxplot() +
                  geom_jitter() +
                  labs(x = "Full Electoral Democracy", y = "Mean Latent Treaty Depth")
depth.boxplot
# plot against start year
depth.time <- filter(atop.milsup, !is.na(maxcap.liedh)) %>%
                ggplot(aes(x = begyr, y = latent.depth.mean, 
                        shape = factor(maxcap.liedh))) +
                  geom_point(alpha = .75, size = 2) +
                  scale_shape_manual(values = c(16, 17),
                     labels = c("Present", "Absent")) +
                  labs(x = "Alliance Start Year", y = "Mean Latent Treaty Depth",
                  shape = "Full Electoral \n Democracy")
depth.time

# combine plots
grid.arrange(depth.boxplot, depth.time,
             nrow = 2)
raw.data <- arrangeGrob(depth.boxplot, depth.time,
                        nrow = 2)
ggsave("figures/raw-data.png", raw.data, height = 6, width = 8)

# two IVS
table(atop.milsup$maxcap.cons, atop.milsup$maxcap.lied)
table(atop.milsup$maxcap.cons, atop.milsup$maxcap.liedh)
# France and post-Soviet Russia are the two alliance leaders
# that have full elections w/o constraints





### compare model fit with one and two factors
latent.depth2 <- bfa_copula(~ intcom + compag.mil +
                             milaid + milcon + base +
                             organ1 + subord.dum + contrib,
                           data = atop.depth, num.factor = 2,
                           restrict = list(c("intcom", 1, ">0"),
                                           c("milaid", 2, ">0")),
                           factor.scales = FALSE,
                           keep.scores = TRUE, loading.prior = "normal",
                           px = TRUE, imh.iter = 1000, imh.burn = 1000,
                           nburn = 20000, nsim = 30000, thin = 30, print.status = 2000)
# plot(get_coda(latent.depth2))


# Diagnosis of convergence with coda
lcap.sam <- get_coda(latent.depth2, scores = TRUE)
effectiveSize(lcap.sam)
diag.geweke  <- geweke.diag(lcap.sam)

# Plot to see if Geweke Z-scores appear to be from Normal(0, 1) distribution
par(mfrow=c(1, 1))
plot(density(diag.geweke$z))
lines(density(rnorm(10000, 0, 1)))
# decidedly not- some fat tails. 



# quick summary of duration
atop.milsup$duration <- ifelse(atop.milsup$endyr != 0,
                               atop.milsup$endyr - atop.milsup$begyr,
                               2016 - atop.milsup$begyr)

ggplot(atop.milsup, aes(x = avg.democ, y = duration,
                        color = latent.depth.mean,
                        shape = factor(post45))) +
  scale_colour_gradient2() +
   geom_hline(yintercept = 10) +
   geom_jitter(size = 2)

ggplot(atop.milsup, aes(x = duration,
                        fill = factor(post45))) +
  facet_wrap(~ factor(maxcap.democ)) +
  geom_vline(xintercept = 10) +
  geom_histogram()


ggplot(filter(atop.milsup, maxcap.democ == 10),
       aes(x = duration, 
           fill = factor(post45))) +
  geom_vline(xintercept = 10) +
  geom_histogram()
