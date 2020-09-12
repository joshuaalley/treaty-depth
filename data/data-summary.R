# Joshua Alley & Matthew Fuhrmann
# Exploratory plots 



# descriptive statistics

ggplot(atop.milsup, aes(maxcap.open)) + geom_bar()
cor.test(atop.milsup$latent.depth.mean, atop.milsup$maxcap.lied)


# plot depth over time 
depth.scatter <- ggplot(atop.milsup, aes(x = begyr, y = latent.depth.mean)) +
  geom_point() +
  labs(x = "Alliance Start Year", y = "Latent Measure of Alliance Depth") +
  ggtitle("Treaty Depth and Alliance Start Year") +
  theme_bw()
depth.scatter


# Run some correlations, t-tests and scatterplots 

# Uncoditional military support
table(atop.milsup$uncond.milsup)
table(atop.milsup$maxcap.rec, atop.milsup$uncond.milsup)

# correlation between democracy and depth
t.test(atop.milsup$latent.depth.mean ~ atop.milsup$maxcap.rec, na.rm = TRUE)

ggplot(atop.milsup, aes(x = maxcap.democ, y = latent.depth.mean)) + 
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


# correlation between different democracy pieces
table(atop.milsup$maxcap.cons, atop.milsup$maxcap.lied)
t.test(maxcap.lied ~ maxcap.cons, data = atop.milsup)


# Focus on avg democracy 
ggplot(atop.milsup, aes(x = as.factor(uncond.milsup), y = latent.depth.mean)) +
  geom_violin() +  # add violin
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.5,  # somewhat trasparent
             aes(size = maxcap.democ)) + theme_classic()

# Combine unconditional, size and depth 
ggplot(atop.milsup, aes(x = as.factor(uncond.milsup), y = latent.depth.mean)) +
  geom_violin() +  # add violin
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.7,  # somewhat trasparent
             size = 2,
             aes(color = maxcap.democ)) +
  scale_colour_viridis_c(option = "plasma") + # change color scale
  theme_classic()

# Switch x-axis variables
ggplot(atop.milsup, aes(x = maxcap.democ, y = latent.depth.mean)) +
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
             aes(color = maxcap.democ)) +
  scale_colour_viridis_c(option = "plasma") + # change color scale
  theme_classic()


# Start year, democracy and unconditional military support
ggplot(atop.milsup, aes(x = begyr, y = latent.depth.mean)) +
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.7,  # somewhat trasparent
             aes(color = maxcap.democ, size = as.factor(uncond.milsup))) +
  scale_colour_viridis_c(option = "plasma") + # change color scale
  theme_classic()

# series of pairwise correlations and t-tests
t.test(atop.milsup$maxcap.democ ~ atop.milsup$uncond.milsup) # uncond milsup alliances are less democratic
t.test(atop.milsup$begyr ~ atop.milsup$uncond.milsup) # uncond milsup is more common in modern era
t.test(atop.milsup$num.mem ~ atop.milsup$uncond.milsup) # less clear difference in conditionality by size
cor.test(atop.milsup$maxcap.democ, atop.milsup$begyr) # democracy and start year are positively correlated
cor.test(atop.milsup$maxcap.democ, atop.milsup$num.mem) # democracy and alliance size are positively correlated 


# Start year, democracy and alliance size
ggplot(atop.milsup, aes(x = begyr, y = latent.depth.mean)) +
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.7,  # somewhat trasparent
             aes(color = maxcap.democ, size = num.mem)) +
  scale_colour_viridis_c(option = "plasma") + # change color scale
  theme_classic()

# Start year, maxcap democ and unconditional military support in bilateral treaties
atop.milsup %>% filter(bilat == 1) %>% 
ggplot(aes(x = begyr, y = latent.depth.mean)) +
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.7,  # somewhat trasparent
             aes(size = maxcap.democ, color = as.factor(uncond.milsup))) +
  theme_classic()


# Start year, maxcap democ and unconditional military support in multilateral treaties
atop.milsup %>% filter(bilat == 0) %>% 
  ggplot(aes(x = begyr, y = latent.depth.mean)) +
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.7,  # somewhat trasparent
             aes(color = maxcap.democ, size = as.factor(uncond.milsup))) +
  scale_colour_viridis_c(option = "plasma") + # change color scale
  theme_classic()



# Plot depth against weighted avg democracy, shape by uncond milsup
ggplot(atop.milsup, aes(x = maxcap.democ, y = latent.depth.mean)) +
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.7,  # somewhat trasparent,
             size = 2.5,
             aes(shape = as.factor(uncond.milsup))) +
  scale_shape_manual(values = c(16, 17),
                      labels = c("Conditional", "Unconditional")) +
  geom_smooth(method = "lm") +
  labs(x = "Democracy of Most Capable Member", y = "Latent Treaty Depth",
       shape = "Conditionality") +
  theme_bw()



# split by uncond milsup
ggplot(atop.milsup, aes(x = as.factor(uncond.milsup), y = latent.depth.mean)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.7,  # somewhat trasparent,
             size = 2.5,
             aes(color = maxcap.democ))  +
  scale_colour_viridis_c(option = "plasma") + # change color scale
  labs(x = "Unconditional", y = "Latent Treaty Depth",
       shape = "Conditionality") +
  theme_bw()


ggplot(atop.milsup, aes(x = maxcap.democ)) +
  geom_histogram() +
  facet_wrap(vars(uncond.milsup, deep.alliance)) +
  labs(y = "Count", x = "Average Democracy") +
  theme_bw()

ggplot(atop.milsup, aes(x = dem.prop)) +
  geom_histogram() +
  facet_wrap(vars(uncond.milsup, deep.alliance)) +
  labs(y = "Count", x = "Proportion of Democracies") +
  theme_bw()



### summarize democracy by depth and uncond milsup 
atop.democ.group <- atop.milsup %>%
                    group_by(deep.alliance, uncond.milsup) %>%
                     summarize(
                       avg.prop.open = mean(prop.open, na.rm = TRUE),
                       avg.open = mean(maxcap.lied, na.rm = TRUE)
                     )


# plot summary democracy scores
ggplot(data = atop.democ.group, 
                        aes(y = as.factor(deep.alliance), 
                            x = as.factor(uncond.milsup), 
                            fill = avg.open)) + 
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#FFFFFF", mid = "#999999", high = "#333333", 
                       space = "Lab", 
                       name = "Average \n Alliance Leader \n Electoral Competition"
                    ) +
  labs(y = "Treaty Depth", x = "Military Support") +
  scale_y_discrete(labels = c("Shallow Alliance", "Deep Alliance")) + 
  scale_x_discrete(labels = c("Conditional", "Unconditional")) +
  theme_bw() + coord_fixed() +
  geom_text(aes(y = as.factor(deep.alliance), x = as.factor(uncond.milsup), 
                label = round(avg.open, digits = 2)),
            color = "black", size = 6) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))
ggsave("figures/democ-combo.png", height = 6, width = 8)


# Same plot with average democratic proportion
ggplot(data = atop.democ.group, 
       aes(y = as.factor(deep.alliance), 
           x = as.factor(uncond.milsup), 
           fill = avg.prop.open)) + 
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#FFFFFF", mid = "#999999", high = "#333333", 
                       space = "Lab", 
                       name = "Avg. Share of Members \n with Open Electoral\n Competition") +
  labs(y = "Treaty Depth", x = "Military Support") +
  scale_y_discrete(labels = c("Shallow Alliance", "Deep Alliance")) + 
  scale_x_discrete(labels = c("Conditional", "Unconditional")) +
  theme_bw() + coord_fixed() +
  geom_text(aes(y = as.factor(deep.alliance), x = as.factor(uncond.milsup), 
                label = round(avg.prop.open, digits = 2)),
            color = "black", size = 6) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))
ggsave("appendix/democ-prop-combo.png", height = 6, width = 8)





### Break out a quick look at GWF data 
alliance.gwf.count <- filter(alliance.year, year >= 1946) %>% 
  select(c(atopid,  
           maxcap.party, maxcap.personal,
           maxcap.military, maxcap.monarchy)) %>% 
  left_join(atop.milsup) %>%
  filter(offense == 1 | defense == 1) %>% 
  select(atopid, deep.alliance, uncond.milsup, maxcap.democ, 
           maxcap.party, maxcap.personal,
           maxcap.military, maxcap.monarchy) %>%
  mutate(maxcap.democ = ifelse(maxcap.democ >= 6, 1, 0)) 
glimpse(alliance.gwf.count)

# reshape long to get counts of most capable members 
alliance.gwf.count <- alliance.gwf.count %>%
                      group_by(deep.alliance, uncond.milsup) %>%
                      summarize(
                        democ.count = sum(maxcap.democ, na.rm = TRUE), 
                        gwf.party.count = sum(maxcap.party, na.rm = TRUE), 
                        gwf.military.count = sum(maxcap.military, na.rm = TRUE),
                        gwf.monarchy.count = sum(maxcap.monarchy, na.rm = TRUE), 
                        gwf.personal.count = sum(maxcap.personal, na.rm = TRUE)
                      ) %>%
                 group_by() %>%
                 pivot_longer(-c(deep.alliance, uncond.milsup), 
                               names_to = "regime.type",
                               values_to = "count")
glimpse(alliance.gwf.count)

# Bar plot of different regimes by alliance quadrant
ggplot(alliance.gwf.count, aes(y = count, x = regime.type,
                               fill = regime.type)) +
  facet_wrap(~uncond.milsup + deep.alliance,
     labeller = labeller(uncond.milsup = c("1" = "Unconditional", 
                                           "0" = "Conditional"),
                    deep.alliance = c("1" = "Deep", "0" = "Shallow"))
     ) +
  geom_bar(stat = "identity") +
  ggtitle("Count of Regime Type Members by Alliance Treaty Design")



# Break out a quick look at GWF data w/ proportions
alliance.gwf.prop <- filter(alliance.year, year >= 1946 & begyr == year) %>% 
  select(atopid, dem.prop, party.prop,        
           military.prop, monarchy.prop,     
           personal.prop) %>% 
  left_join(atop.milsup) %>%
  filter(offense == 1 | defense == 1) %>%
  select(atopid, dem.prop, uncond.milsup, deep.alliance,
         party.prop, military.prop, monarchy.prop,     
         personal.prop)
glimpse(alliance.gwf.prop)

# reshape long to get proportions by type
# reshape long to get coutns 
alliance.gwf.prop <- alliance.gwf.prop %>%
  group_by(deep.alliance, uncond.milsup) %>%
  summarize(
    democ.prop = mean(dem.prop, na.rm = TRUE), 
    party.prop = mean(party.prop, na.rm = TRUE), 
    military.prop = mean(military.prop, na.rm = TRUE),
    monarchy.prop = mean(monarchy.prop, na.rm = TRUE), 
    personal.prop = mean(personal.prop, na.rm = TRUE)
  ) %>%
  group_by() %>%
  pivot_longer(-c(deep.alliance, uncond.milsup), 
               names_to = "regime.type",
               values_to = "prop")
glimpse(alliance.gwf.prop)

# Bar plot of different regimes by alliance quadrant
ggplot(alliance.gwf.prop, aes(y = prop, x = regime.type,
                               fill = regime.type)) +
  facet_wrap(~uncond.milsup + deep.alliance,
             labeller = labeller(uncond.milsup = c("1" = "Unconditional", 
                                                   "0" = "Conditional"),
                                 deep.alliance = c("1" = "Deep", "0" = "Shallow"))
  ) +
  geom_bar(stat = "identity") +
  ggtitle("Average Proportion of Regime Types by Alliance Treaty Design")
