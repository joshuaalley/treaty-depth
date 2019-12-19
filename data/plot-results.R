# Joshua Alley
# Texas A&M University
# Plot results for mediation analyses of latent depth



# tabulate the results
# latent depth mean
med.res.mean <- mediation(brm.multivar, treatment = "non.maj.only", prob = .9)
xtable(med.res.mean) # will probably plot instead
med.mean.plot  <- as_tibble(med.res.mean) %>% filter(effect == "direct" | effect == "indirect" | effect == "total") %>%
  ggplot(aes(y = effect, x = value)) +
  geom_vline(xintercept = 0) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = hdi.low, xmax = hdi.high,
                     height = .1), size = 1) + 
  labs(x = "Estimated Effect", y = "Effect Type") +
  ggtitle("Mean Latent Depth") +
  theme_bw()
med.mean.plot


# Deep alliance dummy 
med.res.dum <- mediation(brm.multivar.dum, treatment = "non.maj.only", prob = .9)
xtable(med.res.dum) # will probably plot instead
med.dum.plot  <- as_tibble(med.res.dum) %>% filter(effect == "direct" | effect == "indirect" | effect == "total") %>%
  ggplot(aes(y = effect, x = value)) +
  geom_vline(xintercept = 0) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = hdi.low, xmax = hdi.high,
                     height = .1), size = 1) + 
  labs(x = "Estimated Effect", y = "Effect Type") +
  ggtitle("Deep Alliance Dummy") +
  theme_bw()
med.dum.plot

# plot results
# Deep alliance dummy 
med.res.unc <- mediation(brm.multivar.unc, treatment = "non.maj.only", prob = .9)
xtable(med.res.unc) # will probably plot instead
med.unc.plot  <- as_tibble(med.res.unc) %>% filter(effect == "direct" | effect == "indirect" | effect == "total") %>%
  ggplot(aes(y = effect, x = value)) +
  geom_vline(xintercept = 0) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = hdi.low, xmax = hdi.high,
                     height = .1), size = 1) + 
  labs(x = "Estimated Effect", y = "Effect Type") +
  ggtitle("Uncertainty in Treaty Depth") +
  theme_bw()
med.unc.plot

# combine 
multiplot.ggplot(med.mean.plot, med.dum.plot, med.unc.plot)
