# Joshua Alley
# Texas A&M University
# Create a sample of non-allied groups of states 

# need same size as observed data: capture that information here
num.mem.obs <- cbind.data.frame(table(atop.milsup$num.mem))
colnames(num.mem.obs) <- c("num.mem", "num.all")
num.mem.obs <- as.data.frame(apply(num.mem.obs, 2, as.numeric))
glimpse(num.mem.obs)

# Load data on states 
state.vars <- read.csv("data/state-vars.csv")


# Use observed data to draw samples from state data
# group by year
state.vars <- state.vars %>% 
            group_by(year) %>%
             select(stateabb, ccode, year,
                    polity, polity2, gdp,
                    majpower, super.power, cinc,
                    disputes, ln.milex,
                    atwar, rival.milex, lsthreat) %>%
           left_join(select(data_polity,
                            ccode, year, exec.cons,
                            open.pol, lexical_index_original, 
                            v2x_polyarchy))


# sequence will be something like this
# Sample by year for all sizes
# cut out duplicates and ensure enough of each for final step
# Randomly sample remaining, roughly weighted by size and year 


# create a list of dataframes by alliance size
alliance.data <- vector(mode = "list", length = nrow(num.mem.obs))

# Sample for most common alliance sizes by year
for(i in 1:nrow(num.mem.obs)){
  alliance.data[[i]] <- as.data.frame(
                        sample_n(state.vars, 
                         size = num.mem.obs$num.mem[i],
                         replace = TRUE) # filter to avoid multiple membership later
  )
}

# do this again, but get larger alliances from after 1950
# need enough states to avoid filling with repeat members
for(i in 13:19){
  alliance.data[[i]] <- as.data.frame(
    filter(state.vars, year >= 1949) %>%
    sample_n(size = num.mem.obs$num.mem[i],
             replace = FALSE)
  )
}

# clean up the alliance data with lapply. 
alliance.data <- lapply(alliance.data,
                        function(x){
                          x %>%
                            group_by(year) %>%
                            mutate(
                              most.cap = ifelse(cinc == max(cinc, na.rm = TRUE), 1, 0),
                              maxcap.democ = ifelse(most.cap == 1, polity2, 0),
                              maxcap.lied = ifelse(most.cap == 1, lexical_index_original, 0),
                              maxcap.cons = ifelse(most.cap == 1 & exec.cons == 1, 1, 0)
                            ) %>%
                            summarize(
                              size = n(),
                              begyr = max(year, na.rm = TRUE),
                              avg.democ = mean(polity2, na.rm = TRUE),
                              mean.threat = mean(lsthreat, na.rm = TRUE),
                              total.cap = sum(cinc, na.rm = TRUE),
                              mp.count = sum(majpower, na.rm = TRUE),
                              wartime = max(atwar, na.rm = TRUE),
                              ccode.unique = length(unique(ccode)),
                              maxcap.lied = max(maxcap.lied, na.rm = TRUE),
                              maxcap.cons = max(maxcap.cons, na.rm = TRUE),
                              maxcap.democ.min = min(maxcap.democ, na.rm = TRUE),
                              maxcap.democ.max = max(maxcap.democ, na.rm = TRUE)
                            )   %>%
                           mutate(
                             asymm.cap = ifelse(mp.count > 0 & mp.count < size, 1, 0),
                              non.maj.only = ifelse(mp.count == 0, 1, 0)
                            )
                        }
                    )
alliance.data.unlist <- bind_rows(alliance.data) %>%
                         filter(ccode.unique == size) # exclude duplicate states from sampling w/ replacement

alliance.data.unlist$sample <- group_indices(alliance.data.unlist, year, size)

# Add more bilateral alliances
set.seed(14)
bilat.all <- as.data.frame(
  sample_n(state.vars, 
           size = 10,
           replace = FALSE))
bilat.all$sample <- rep(1:5, each = 2)
set.seed(12)

# summarize data
bilat.all <- bilat.all %>%
  group_by(year, sample) %>%
  mutate(
    most.cap = ifelse(cinc == max(cinc, na.rm = TRUE), 1, 0),
    maxcap.democ = ifelse(most.cap == 1, polity2, 0),
    maxcap.lied = ifelse(most.cap == 1, lexical_index_original, 0),
    maxcap.cons = ifelse(most.cap == 1 & exec.cons == 1, 1, 0)
  ) %>%
  summarize(
    size = n(),
    begyr = max(year, na.rm = TRUE),
    avg.democ = mean(polity2, na.rm = TRUE),
    mean.threat = mean(lsthreat, na.rm = TRUE),
    total.cap = sum(cinc, na.rm = TRUE),
    mp.count = sum(majpower, na.rm = TRUE),
    wartime = max(atwar, na.rm = TRUE),
    ccode.unique = length(unique(ccode)),
    maxcap.lied = max(maxcap.lied, na.rm = TRUE),
    maxcap.cons = max(maxcap.cons, na.rm = TRUE),
    maxcap.democ.min = min(maxcap.democ, na.rm = TRUE),
    maxcap.democ.max = max(maxcap.democ, na.rm = TRUE)
  )   %>%
  mutate(
    asymm.cap = ifelse(mp.count > 0 & mp.count < size, 1, 0),
    non.maj.only = ifelse(mp.count == 0, 1, 0)
  )

# full sampled data: combine extra bilateral alliances
samp.data <- bind_rows(alliance.data.unlist, bilat.all) %>% 
                distinct() # drop duplicate observations
table(samp.data$size)
table(atop.milsup$num.mem)

# Stratified sampling by alliance size

# create list and fill with samples
samp.noall <- vector(mode = "list", length = nrow(num.mem.obs))
for(i in 1:nrow(num.mem.obs)){
samp.noall[[i]] <- samp.data %>% 
  filter(size == num.mem.obs$num.mem[i]) %>%
  sample_n(size = 5*num.mem.obs$num.all[i])
}
samp.noall <- bind_rows(samp.noall)
table(samp.noall$size)


## Old measure
# Clean up the maximum democracy by capability in non-allied groups
# select(samp.noall, maxcap.democ.max, maxcap.democ.min) %>% 
#   filter(maxcap.democ.max == maxcap.democ.min)
# samp.noall$maxcap.democ.max[samp.noall$maxcap.democ.max < 0] <- 0 # Maximum below 0
# samp.noall$maxcap.democ.min[samp.noall$maxcap.democ.min > 0] <- 0 # minimum above 0
# select(samp.noall, maxcap.democ.max, maxcap.democ.min) %>% 
#   filter(maxcap.democ.max == maxcap.democ.min)
# 
# # create measure with polity score of most capable 
# # Add zero in one column to max or min in the other 
# samp.noall$maxcap.democ <- samp.noall$maxcap.democ.min + samp.noall$maxcap.democ.max
# summary(samp.noall$maxcap.democ)

# look at key IVs
summary(samp.noall$maxcap.lied)
table(samp.noall$maxcap.cons)

# combine sampled data with observed alliances
alldata.comb <- atop.milsup %>%
                select(atopid, begyr, maxcap.lied, maxcap.cons, avg.democ, 
                       mean.threat, asymm.cap, non.maj.only, asymm, wartime,
                       latent.depth.mean, uncond.milsup, num.mem,
                       econagg.dum, fp.conc.index, low.kap.sc, uncond.oblig
                       ) %>%
                rename(
                   size = num.mem
                ) %>%
                bind_rows(samp.noall)


# Rescale depth for beta dist: shift location
alldata.comb$latent.depth.mean.rs <- (alldata.comb$latent.depth.mean + 1)

# Add 1 to uncond.oblig variable: captures base str from military support relative to no alliance
# shifts distribution for a poisson hurdle
alldata.comb$oblig.str <- alldata.comb$uncond.oblig + 1
alldata.comb$oblig.str[is.na(alldata.comb$atopid)] <- 0
table(alldata.comb$oblig.str)

# Check the data
table(alldata.comb$asymm.cap)
table(alldata.comb$non.maj.only)
summary(alldata.comb$avg.democ)
summary(alldata.comb$mean.threat)
table(alldata.comb$uncond.milsup)
summary(alldata.comb$latent.depth.mean.rs)
table(alldata.comb$size)

# Mark out variables from alliance-level data
# Create a hurdle version of latent depth 
alldata.comb$latent.depth.hurdle <- alldata.comb$latent.depth.mean.rs
ggplot(alldata.comb, aes(x = latent.depth.hurdle)) + geom_histogram()
alldata.comb$latent.depth.hurdle[is.na(alldata.comb$atopid)] <- 0
summary(alldata.comb$latent.depth.hurdle)
ggplot(alldata.comb, aes(x = latent.depth.hurdle)) + geom_histogram()

# mark no alliance ATOPID as zero
alldata.comb$atopid[is.na(alldata.comb$atopid)] <- 0



# Fit a hurdle model with brms
# varying intercepts by year 
hurdle.depth <- brm(
  formula = bf(
    latent.depth.hurdle ~ 1 + maxcap.lied + maxcap.cons + size + mean.threat +
      non.maj.only + asymm.cap + 
      wartime + (1 | begyr), 
    hu ~ 1 + avg.democ + mean.threat + asymm.cap + 
      wartime + (1 | begyr)
  ),
  family = hurdle_gamma,
  prior = c(
    set_prior("normal(0, 5)", class = "b"),
    set_prior("normal(0, 5)", class = "b", dpar = "hu"),
    set_prior("normal(0, 1)", class = "sd")
  ),
  data = alldata.comb,
  control = list(adapt_delta = .9)
)

pp_check(hurdle.depth)
summary(hurdle.depth)

# plot conditional effects: 90% intervals
plot(conditional_effects(hurdle.depth, probs = c(0.05, 0.95)),
     ask = FALSE,
     rug = TRUE)
# Pull democracy plot
ce.democ <- conditional_effects(hurdle.depth, probs = c(0.05, 0.95),
                    effects = "maxcap.lied")
plot(ce.democ,
    plot = FALSE)[[1]] +
  scale_color_grey() +
  scale_fill_grey() +
  labs(x = "Electoral Competition in Alliance Leader",
   y = "Latent Depth (shifted)")


### Attempt a joint model with military support and depth

# start with a poisson hurdle of obligation str
hurdle.cond <- brm(
  formula = bf(
    oblig.str ~ 1 + maxcap.lied + maxcap.cons + size + mean.threat +
      non.maj.only + asymm.cap +
      wartime + (1 | begyr), 
    hu ~ 1 + avg.democ + mean.threat + asymm.cap + 
      wartime + (1 | begyr)
  ),
  family = hurdle_poisson,
  prior = c(
    set_prior("normal(0, 5)", class = "b"),
    set_prior("normal(0, 5)", class = "b", dpar = "hu"),
    set_prior("normal(0, 1)", class = "sd")
  ),
  data = alldata.comb,
  control = list(adapt_delta = .9)
)

summary(hurdle.cond)

# Plot results 
plot(conditional_effects(hurdle.cond, probs = c(0.05, 0.95)),
     ask = FALSE,
     rug = TRUE)


# set up formulas for the joint hurdle
# better luck without setting the priors
# depth 
depth.formula.hurdle <- brmsformula(
    latent.depth.hurdle ~ 1 + maxcap.lied + maxcap.cons + size + mean.threat +
      non.maj.only + asymm.cap +
      wartime + (1 | p | begyr), 
    hu ~ 1 + avg.democ + mean.threat + asymm.cap + 
      wartime 
  ) + hurdle_gamma(link = "log", link_shape = "log", link_hu = "logit")
depth.priors.hurdle <- c(set_prior("normal(0, 5)", class = "b", resp = "latentdepthhurdle"),
                         set_prior("normal(0, 5)", class = "b", dpar = "hu", resp = "latentdepthhurdle"),
                        set_prior("cauchy(0, 1)", class = "sd", resp = "latentdepthhurdle")
                           )


# conditional str
cond.formula.hurdle <- brmsformula(
    oblig.str ~ 1 + maxcap.lied + maxcap.cons + size + mean.threat +
      non.maj.only + asymm.cap +  
      wartime + (1 | p | begyr), 
    hu ~ 1 + avg.democ + mean.threat + asymm.cap + 
      wartime 
     ) + hurdle_poisson(link = "log")
cond.priors.hurdle <- c(set_prior("normal(0, 5)", class = "b", resp = "obligstr"),
                        set_prior("normal(0, 5)", class = "b", dpar = "hu", resp = "obligstr"),                         
                        set_prior("cauchy(0, 1)", class = "sd", resp = "obligstr")
          )


# Run the joint model 
priors.hurdle <- depth.priors.hurdle + cond.priors.hurdle
system.time(
joint.hurdle <- brm(depth.formula.hurdle + cond.formula.hurdle +
                      set_rescor(FALSE), 
                    data = alldata.comb,
                    prior = priors.hurdle,
                    control = list(adapt_delta = .9),
                    iter = 2000, warmup = 1000,
                    chains = 4, cores = 4)
)
summary(joint.hurdle)


# Pull and plot joint results
ce.democ.joint <- conditional_effects(joint.hurdle, probs = c(0.025, 0.975),
                                effects = "maxcap.lied")
ce.plot.joint <- plot(ce.democ.joint,
                ask = FALSE,
                line_args = (aes(color = "black"))
                )

# Obligations
oblig.joint.plot <- ce.plot.joint$obligstr.obligstr_maxcap.lied +
  scale_color_grey() +
  scale_fill_grey() +
  theme_bw() +
  labs(x = "Electoral Democracy of Alliance Leader",
       y = "Strength of Military Support Conditions")
oblig.joint.plot

# latent depth 
depth.joint.plot <- ce.plot.joint$latentdepthhurdle.latentdepthhurdle_maxcap.lied +
                      scale_color_grey() +
                      scale_fill_grey() +
                      theme_bw() + 
                      labs(x = "Electoral Democracy of Alliance Leader",
                       y = "Latent Depth (shifted)")
depth.joint.plot

# combine plots and export
grid.arrange(oblig.joint.plot, depth.joint.plot,
             ncol = 2)
results.joint.hurdle <- arrangeGrob(oblig.joint.plot, depth.joint.plot,
                            ncol = 2)
ggsave("appendix/results-joint-hurdle.png", results.joint.hurdle,
       height = 6, width = 8) #save file
