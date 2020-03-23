# Joshua Alley
# Texas A&M University 
# Exploratory plots 



# descriptive statistics

ggplot(atop.milsup, aes(dem.prop)) + geom_histogram()

# average democracy: weighted or not
summary(atop.milsup$avg.democ)
ggplot(atop.milsup, aes(x = avg.democ)) + geom_histogram()
summary(atop.milsup$avg.democ.weight)
ggplot(atop.milsup, aes(x = avg.democ.weight)) + geom_histogram()
ggplot(atop.milsup, aes(x = asinh(avg.democ.weight))) + geom_histogram()


# max democracy: weighted or not
summary(atop.milsup$max.democ)
ggplot(atop.milsup, aes(x = max.democ)) + geom_histogram()
summary(atop.milsup$max.democ.weight)
ggplot(atop.milsup, aes(x = max.democ.weight)) + geom_histogram()
ggplot(atop.milsup, aes(x = asinh(max.democ.weight))) + geom_histogram()


# plot depth over time 
depth.scatter <- ggplot(atop.milsup, aes(x = begyr, y = latent.depth.mean)) +
  geom_point() +
  labs(x = "Alliance Start Year", y = "Latent Measure of Alliance Depth") +
  ggtitle("Treaty Depth and Alliance Start Year") +
  theme_bw()
depth.scatter

# plot results from first paper
depth.res <- read.csv("data/depth-res.csv")
depth.res.min <- ggplot(depth.res, aes(x = latent.depth.mean, y = lambda)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth(method = "lm") + theme_bw() +
  labs(x = "Latent Treaty Depth", y = "Effect of Alliance Participation") +
  ggtitle("Treaty Depth and the Impact of Alliance on Non-Major Power Military Spending: 1816-2007")
depth.res.min
ggsave("figures/depth-motive.png", height = 6, width = 8)



# Run some correlations, t-tests and scatterplots 

# Uncoditional military support
table(atop.milsup$uncond.milsup)
t.test(atop.milsup$latent.depth.mean ~ atop.milsup$uncond.milsup)

# correlation between avg democracy and depth
cor.test(atop.milsup$avg.democ, atop.milsup$latent.depth.mean, na.rm = TRUE)

ggplot(atop.milsup, aes(x = avg.democ, y = latent.depth.mean)) + 
  geom_point() + geom_smooth(method = "lm") +
  theme_classic()

ggplot(atop.milsup, aes(x = avg.democ.weight, y = latent.depth.mean)) + 
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
             size = 2,
             aes(color = avg.democ)) +
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


# Start year, democracy and alliance size
ggplot(atop.milsup, aes(x = begyr, y = latent.depth.mean)) +
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.7,  # somewhat trasparent
             aes(color = avg.democ, size = num.mem)) +
  scale_colour_viridis_c(option = "plasma") + # change color scale
  theme_classic()

# Start year, avg democ and unconditional military support in bilateral treaties
atop.milsup %>% filter(bilat == 1) %>% 
ggplot(aes(x = begyr, y = latent.depth.mean)) +
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.7,  # somewhat trasparent
             aes(size = avg.democ, color = as.factor(uncond.milsup))) +
  theme_classic()


# Start year, avg democ and unconditional military support in multilateral treaties
atop.milsup %>% filter(bilat == 0) %>% 
  ggplot(aes(x = begyr, y = latent.depth.mean)) +
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.7,  # somewhat trasparent
             aes(color = avg.democ, size = as.factor(uncond.milsup))) +
  scale_colour_viridis_c(option = "plasma") + # change color scale
  theme_classic()



# Plot depth against weighted avg democracy, shape by uncond milsup
ggplot(atop.milsup, aes(x = avg.democ, y = latent.depth.mean)) +
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.7,  # somewhat trasparent,
             size = 2.5,
             aes(shape = as.factor(uncond.milsup))) +
  scale_shape_manual(values = c(16, 17),
                      labels = c("Conditional", "Unconditional")) +
  geom_smooth(method = "lm") +
  labs(x = "Average Democracy", y = "Latent Treaty Depth",
       shape = "Conditionality") +
  theme_bw()



# split by uncond milsup
ggplot(atop.milsup, aes(x = as.factor(uncond.milsup), y = latent.depth.mean)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitter(width = 0.1),  # jitter points to prevent overlap
             alpha = 0.7,  # somewhat trasparent,
             size = 2.5,
             aes(color = avg.democ))  +
  scale_colour_viridis_c(option = "plasma") + # change color scale
  labs(x = "Unconditional", y = "Latent Treaty Depth",
       shape = "Conditionality") +
  theme_bw()


ggplot(atop.milsup, aes(x = avg.democ)) +
  geom_histogram() +
  facet_wrap(vars(uncond.milsup, deep.alliance)) +
  labs(y = "Count", x = "Average Democracy") +
  theme_bw()

ggplot(atop.milsup, aes(x = dem.prop)) +
  geom_histogram() +
  facet_wrap(vars(uncond.milsup, deep.alliance)) +
  labs(y = "Count", x = "Proportion of Democracies") +
  theme_bw()



# summarize democracy by depth and uncond milsup 
atop.democ.group <- atop.milsup %>%
                    group_by(deep.alliance, uncond.milsup) %>%
                     summarize(
                       avg.democ.mean = mean(avg.democ, na.rm = TRUE),
                       avg.democ.sd = sd(avg.democ, na.rm = TRUE),
                       avg.democw.mean = mean(avg.democ.weight, na.rm = TRUE),
                       avg.democw.sd = sd(avg.democ.weight, na.rm = TRUE),
                       
                       max.democ.mean = mean(max.democ, na.rm = TRUE),
                       max.democ.sd = sd(max.democ, na.rm = TRUE),
                       max.democw.mean = mean(max.democ.weight, na.rm = TRUE),
                       max.democw.sd = sd(max.democ.weight, na.rm = TRUE),
                       
                       avg.dem.prop = mean(dem.prop, na.rm = TRUE),
                       avg.maxcap.dem = mean(maxcap.democ, na.rm = TRUE)
                      
                     )


# plot summary democracy scores
ggplot(data = atop.democ.group, 
                        aes(y = as.factor(deep.alliance), 
                            x = as.factor(uncond.milsup), 
                            fill = avg.democ.mean)) + 
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#FFFFFF", mid = "#999999", high = "#333333", 
                       space = "Lab", 
                       name = "Avg. Polity Score") +
  labs(y = "Treaty Depth", x = "Military Support") +
  scale_y_discrete(labels = c("Shallow Alliance", "Deep Alliance")) + 
  scale_x_discrete(labels = c("Conditional", "Unconditional")) +
  theme_bw() + coord_fixed() +
  geom_text(aes(y = as.factor(deep.alliance), x = as.factor(uncond.milsup), 
                label = round(avg.democ.mean, digits = 2)),
            color = "black", size = 6) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))
ggsave("figures/democ-combo.png", height = 6, width = 8)


# Same plot with average democratic proportion
ggplot(data = atop.democ.group, 
       aes(y = as.factor(deep.alliance), 
           x = as.factor(uncond.milsup), 
           fill = avg.dem.prop)) + 
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#FFFFFF", mid = "#999999", high = "#333333", 
                       space = "Lab", 
                       name = "Avg. Dem. Proportion") +
  labs(y = "Treaty Depth", x = "Military Support") +
  scale_y_discrete(labels = c("Shallow Alliance", "Deep Alliance")) + 
  scale_x_discrete(labels = c("Conditional", "Unconditional")) +
  theme_bw() + coord_fixed() +
  geom_text(aes(y = as.factor(deep.alliance), x = as.factor(uncond.milsup), 
                label = round(avg.dem.prop, digits = 2)),
            color = "black", size = 6) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))
ggsave("appendix/democ-prop-combo.png", height = 6, width = 8)



# Same plot with polity score of most capable member
ggplot(data = atop.democ.group, 
       aes(y = as.factor(deep.alliance), 
           x = as.factor(uncond.milsup), 
           fill = avg.maxcap.dem)) + 
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#FFFFFF", mid = "#999999", high = "#333333", 
                       space = "Lab", 
                       name = "Avg. Democ. of Most Capable State") +
  labs(y = "Treaty Depth", x = "Military Support") +
  scale_y_discrete(labels = c("Shallow Alliance", "Deep Alliance")) + 
  scale_x_discrete(labels = c("Conditional", "Unconditional")) +
  theme_bw() + coord_fixed() +
  geom_text(aes(y = as.factor(deep.alliance), x = as.factor(uncond.milsup), 
                label = round(avg.maxcap.dem, digits = 2)),
            color = "black", size = 6) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))
ggsave("appendix/democ-prop-combo.png", height = 6, width = 8)




# Break out a quick look at GWF data 
alliance.gwf.count <- filter(alliance.year, year >= 1946 & begyr == year) %>% 
  select(c(atopid, democ.count, 
           gwf.party.count, gwf.military.count,
           gwf.monarchy.count, gwf.personal.count)) %>% 
  left_join(atop.milsup) %>%
  filter(offense == 1 | defense == 1) %>% 
  select(atopid, deep.alliance, uncond.milsup, democ.count, 
           gwf.party.count, gwf.military.count,
           gwf.monarchy.count, gwf.personal.count)
glimpse(alliance.gwf.count)

# reshape long to get coutns 
alliance.gwf.count <- alliance.gwf.count %>%
                      group_by(deep.alliance, uncond.milsup) %>%
                      summarize(
                        democ.count = sum(democ.count, na.rm = TRUE), 
                        gwf.party.count = sum(gwf.party.count, na.rm = TRUE), 
                        gwf.military.count = sum(gwf.military.count, na.rm = TRUE),
                        gwf.monarchy.count = sum(gwf.monarchy.count, na.rm = TRUE), 
                        gwf.personal.count = sum(gwf.personal.count, na.rm = TRUE)
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
     labeller = label_both) +
  geom_bar(stat = "identity") +
  ggtitle("Count of Regime Type Members by Alliance Treaty Design")

#              labeller= labeller(uncond.milsup = c("Unconditional", "Conditional"),
# deep.alliance = c("Deep", "Shallow")



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
             labeller = label_both) +
  geom_bar(stat = "identity") +
  ggtitle("Average Proportion of Regime Types by Alliance Treaty Design")
