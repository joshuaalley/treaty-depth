# Joshua Alley
# Texas A&M University 
# Exploratory plots 


# Run some correlations, t-tests and scatterplots 

# correlation between avg democracy and depth
cor.test(atop.milsup$avg.democ, atop.milsup$latent.depth.mean)

ggplot(atop.milsup, aes(x = avg.democ, y = latent.depth.mean)) + 
  geom_point() + geom_smooth(method = "lm") +
  theme_classic()

# Look at correlation between FP similarity and depth
cor.test(atop.milsup$mean.kap.sc, atop.milsup$latent.depth.mean)
cor.test(atop.milsup$low.kap.sc, atop.milsup$latent.depth.mean)

ggplot(atop.milsup, aes(x = low.kap.sc, y = latent.depth.mean)) + 
  geom_point() + theme_classic()


# Plot alliance depth against size
cor.test(atop.milsup$num.mem, atop.milsup$latent.depth.mean)
ggplot(atop.milsup, aes(x = num.mem, y = latent.depth.mean)) +
  geom_point()

# Asymmetric capability
# Plot alliance depth against size
t.test( atop.milsup$latent.depth.mean ~ atop.milsup$asymm.cap)
ggplot(atop.milsup, aes(x = as.factor(asymm.cap), y = latent.depth.mean)) +
  geom_violin() +
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.5)


# Uncoditional military support
t.test(atop.milsup$latent.depth.mean ~ atop.milsup$uncond.milsup)

# Focus on avg democracy 
ggplot(atop.milsup, aes(x = as.factor(uncond.milsup), y = latent.depth.mean)) +
  geom_violin() +  # add violin
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.5,  # somewhat trasparent
             aes(size = avg.democ)) + theme_classic()

# Combine unconditional, size and depth 
ggplot(atop.milsup, aes(x = as.factor(uncond.milsup), y = latent.depth.mean)) +
  geom_violin() +  # add violin
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.7,  # somewhat trasparent
             aes(size = num.mem, color = avg.democ)) +
  scale_colour_viridis_c(option = "plasma") + # change color scale
  theme_classic()

# Switch x-axis variables
ggplot(atop.milsup, aes(x = avg.democ, y = latent.depth.mean)) +
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.7,  # somewhat trasparent
             aes(size = num.mem, color = as.factor(uncond.milsup))) +
  scale_colour_viridis_d(option = "plasma") + # change color scale
  theme_classic()



# Look at trends over time:
# Plot alliance depth against size
cor.test(atop.milsup$latent.depth.mean, atop.milsup$begyr)
ggplot(atop.milsup, aes(x = begyr, y = latent.depth.mean)) +
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.5)


# Check the use of uncoditional military support
ggplot(atop.milsup, aes(x = begyr, y = latent.depth.mean)) +
  geom_point(size = 3,
             position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.7,  # somewhat trasparent
             aes(color = as.factor(uncond.milsup))) +
  scale_colour_viridis_d(option = "plasma") + # change color scale
  theme_classic()

# Check alliance democracy at time of formation 
ggplot(atop.milsup, aes(x = begyr, y = latent.depth.mean)) +
  geom_point(size = 3,
             position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.7,  # somewhat trasparent
             aes(color = avg.democ)) +
  scale_colour_viridis_c(option = "plasma") + # change color scale
  theme_classic()


# Start year, democracy and unconditional military support
ggplot(atop.milsup, aes(x = begyr, y = latent.depth.mean)) +
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.7,  # somewhat trasparent
             aes(color = avg.democ, size = as.factor(uncond.milsup))) +
  scale_colour_viridis_c(option = "plasma") + # change color scale
  theme_classic()

# series of pairwise correlations and t-tests
t.test(atop.milsup$avg.democ ~ atop.milsup$uncond.milsup) # uncond milsup alliances are less democratic
t.test(atop.milsup$begyr ~ atop.milsup$uncond.milsup) # uncond milsup is more common in modern era
t.test(atop.milsup$num.mem ~ atop.milsup$uncond.milsup) # less clear difference in conditionality by size
cor.test(atop.milsup$avg.democ, atop.milsup$begyr) # democracy and start year are positively correlated
cor.test(atop.milsup$avg.democ, atop.milsup$num.mem) # democracy and alliance size are positively correlated 


# Start year, avg democ and unconditional military support in bilateral treaties
atop.milsup %>% filter(bilat == 1) %>% 
ggplot(aes(x = begyr, y = latent.depth.mean)) +
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.7,  # somewhat trasparent
             aes(color = avg.democ, size = as.factor(uncond.milsup))) +
  scale_colour_viridis_c(option = "plasma") + # change color scale
  theme_classic()


# Start year, avg democ and unconditional military support in multilateral treaties
atop.milsup %>% filter(bilat == 0) %>% 
  ggplot(aes(x = begyr, y = latent.depth.mean)) +
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.7,  # somewhat trasparent
             aes(color = avg.democ, size = as.factor(uncond.milsup))) +
  scale_colour_viridis_c(option = "plasma") + # change color scale
  theme_classic()


# Asymmetric capability
ggplot(atop.milsup, aes(x = begyr, y = latent.depth.mean)) +
  geom_point(size = 3,
             position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.7,  # somewhat trasparent
             aes(color = as.factor(asymm.cap))) +
  scale_colour_viridis_d(option = "plasma") + # change color scale
  theme_classic()
t.test(atop.milsup$latent.depth.mean ~ atop.milsup$asymm.cap)

# Asymmetric obligations
ggplot(atop.milsup, aes(x = begyr, y = latent.depth.mean)) +
  geom_point(size = 3,
             position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.7,  # somewhat trasparent
             aes(color = as.factor(asymm))) +
  scale_colour_viridis_d(option = "plasma") + # change color scale
  theme_classic()
t.test(atop.milsup$latent.depth.mean ~ atop.milsup$asymm)


# Only non-major powers
ggplot(atop.milsup, aes(x = begyr, y = latent.depth.mean)) +
  geom_point(size = 3,
             position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.7,  # somewhat trasparent
             aes(color = as.factor(non.maj.only))) +
  scale_colour_viridis_d(option = "plasma") + # change color scale
  theme_classic()
