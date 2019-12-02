# Joshua Alley
# Texas A&M University 
# Analysis of the sources of alliance treaty depth


# focus on the posterior mean in this script: include uncertainty in later scripts

# Load packages
library(MASS)
library(tidyverse)
library(coefplot)


# load data 
atop <- read.csv("data/atop-additions.csv")
atop.milsup <- read.csv("data/atop-milsup.csv")
alliance.democ <- read.csv("data/alliance-democ.csv")
depth.post <- read.csv("data/depth-post.csv")


# combine alliance democ and ATOP milsup data 
atop.milsup <- left_join(atop.milsup, alliance.democ)

# Create a deep alliance dummy
atop.milsup$deep.alliance <- ifelse(atop.milsup$latent.depth.mean > median(atop.milsup$latent.depth.mean),
                                    1, 0)

### Analysis focusing on alliances with military support

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


# Asymmetric capability
ggplot(atop.milsup, aes(x = begyr, y = latent.depth.mean)) +
  geom_point(size = 3,
             position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.7,  # somewhat trasparent
             aes(color = as.factor(asymm.cap))) +
  scale_colour_viridis_d(option = "plasma") + # change color scale
  theme_classic()




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
