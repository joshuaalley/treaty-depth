# Joshua Alley
# Texas A&M University


# Load data
# alliance-member year data 
atop.cow.year <- read.csv("data/atop-cow-year.csv")
# geddes et al autocracy types
gwf.data <- read_dta("data/GWF_AllPoliticalRegimes.dta")


# Clean geddes et al data before merging
glimpse(gwf.data)
table(gwf.data$gwf_nonautocracy)
gwf.data <- select(gwf.data, cowcode, year, 
                   gwf_party, gwf_military,
                   gwf_monarchy, gwf_personal,
                   gwf_nonautocracy) %>%
             rename(ccode = cowcode)

# merge
atop.cow.year$ccode <- as.numeric(atop.cow.year$ccode)
atop.cow.year$year <- as.numeric(atop.cow.year$year)
atop.cow.year <- left_join(atop.cow.year, gwf.data)


# The full dataset can be used to create an alliance characteristics-year dataset
alliance.year <- atop.cow.year %>%
  filter(atopid > 0) %>%
  group_by(atopid, year) %>%
  mutate(
    most.cap = ifelse(cinc == max(cinc, na.rm = TRUE), 1, 0),
    maxcap.democ = ifelse(most.cap == 1, polity2, 0),
    cinc.share = cinc / sum(cinc, na.rm = TRUE),
    democ.weight = polity2 * cinc.share,
    democ = ifelse(polity2 > 5, 1, 0)
  ) %>% 
  summarize(
    avg.democ = mean(polity2, na.rm = TRUE),
    max.democ = max(polity2, na.rm = TRUE),
    min.democ = min(polity2, na.rm = TRUE),
    maxcap.democ.max = max(maxcap.democ, na.rm = TRUE),
    maxcap.democ.min = min(maxcap.democ, na.rm = TRUE),
    joint.democ = ifelse(min.democ > 5, 1, 0), 
    democ.count = sum(democ, na.rm = TRUE),
    
    total.cap = sum(cinc, na.rm = TRUE),
    total.expend = sum(ln.milex, na.rm = TRUE),
    total.gdp = sum(gdp, na.rm = TRUE),
    num.mem = n(),
    
    dem.prop = democ.count / num.mem,
    avg.democ.weight = mean(democ.weight, na.rm = TRUE),
    max.democ.weight = max(democ.weight, na.rm = TRUE),
    min.democ.weight = min(democ.weight, na.rm = TRUE),
    
    gwf.party.count = sum(gwf_party, na.rm = TRUE),
    party.prop = gwf.party.count / num.mem,
    gwf.military.count = sum(gwf_military, na.rm = TRUE),
    military.prop = gwf.military.count / num.mem,
    gwf.monarchy.count = sum(gwf_monarchy, na.rm = TRUE),
    monarchy.prop = gwf.monarchy.count / num.mem,
    gwf.personal.count = sum(gwf_personal, na.rm = TRUE),
    personal.prop = gwf.personal.count / num.mem,
    
    max.threat = max(lsthreat, na.rm = TRUE),
    min.threat = min(lsthreat, na.rm = TRUE),
    mean.threat = mean(lsthreat, na.rm = TRUE)
  )

alliance.year[order(alliance.year$atopid, alliance.year$year),] 


# Merge mean democracy into alliance characteristics data
# this is democracy at time of formation
alliance.year <- alliance.year %>% 
  group_by(atopid) %>%
  mutate(
    begyr = min(year)
  )

alliance.democ <- filter(alliance.year, begyr == year) %>% 
  select(c(atopid, dem.prop, joint.democ, avg.democ, max.democ, min.democ, 
           avg.democ.weight, max.democ.weight, min.democ.weight,
           max.threat, min.threat, mean.threat, 
           maxcap.democ.min, maxcap.democ.max))
write.csv(alliance.democ, "data/alliance-democ.csv",
          row.names = FALSE)



