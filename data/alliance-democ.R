# Joshua Alley
# Texas A&M University


# Load data
# alliance-member year data 
atop.cow.year <- read.csv("data/atop-cow-year.csv")
# raw polity data
polity <- read.csv("data/polity4v2015.csv")


# Dahl Polyarchy/contestation and Lexical Analysis of Democracy
# from democracyData package
polyarchy <- redownload_polyarchy_dimensions() %>%
       select(cown, year, CONTESTstd, INCLUSstd) %>%
          rename(
            ccode = cown,
          )
glimpse(polyarchy)

lied <- redownload_lied() %>%
         select(lied_cow, year,
                exselec, legselec, 
                male_suffrage, female_suffrage,
                opposition, competition,
                lexical_index_original) %>%
         rename(ccode = lied_cow)
glimpse(lied)

# Vdem from the vdem package
vdem.polyarchy <- read.csv("data/vdem-poly.csv") %>%
                    rename(ccode = COWcode)


# Clean the POLITY Data
# source of code here: https://github.com/brentonk/merge-cow-polity
# ensures state-year obs match atop-cow-year
data_polity <- polity %>%
  mutate_each(funs(ifelse(. %in% c(-66, -77, -88), NA, .)),
              -(ccode:year)) %>%
  mutate(country = str_trim(as.character(country))) %>%
  select(-cyear,
         -scode,
         -flag,
         -fragment,
         -durable,
         -(prior:regtrans))

## ----head-clean-polity---------------------------------------------------
head(data_polity)



## ----overlap-function----------------------------------------------------
find_overlap <- function(c1, c2) {
  overlap_years <- with(data_polity,
                        intersect(year[ccode == c1],
                                  year[ccode == c2]))
  data_polity %>% filter(year %in% overlap_years,
                         ccode %in% c(c1, c2))
}

## ----colombia-overlap----------------------------------------------------
find_overlap(99, 100)

## ----recode-colombia-----------------------------------------------------
data_polity <- data_polity %>%
  filter(ccode != 99 | year != 1832) %>%
  mutate(ccode = ifelse(ccode == 99, 100, ccode))

## ----sardinia-overlap----------------------------------------------------
find_overlap(324, 325)

## ----recode-sardinia-----------------------------------------------------
data_polity <- data_polity %>%
  filter(ccode != 324 | year != 1861) %>%
  mutate(ccode = ifelse(ccode == 324, 325, ccode))

## ----serbia-overlap------------------------------------------------------
find_overlap(342, 345)
find_overlap(342, 347)
find_overlap(345, 347)

## ----recode-serbia-------------------------------------------------------
data_polity <- data_polity %>%
  filter(ccode != 347 | !(year %in% c(1991, 2006))) %>%
  mutate(ccode = ifelse(ccode %in% c(342, 347), 345, ccode))

## ----recode-montenegro---------------------------------------------------
data_polity <- data_polity %>%
  mutate(ccode = ifelse(ccode == 341, 347, ccode),
         ccode = ifelse(ccode == 348, 341, ccode))

## ----ussr-overlap--------------------------------------------------------
find_overlap(364, 365)

## ----recode-ussr---------------------------------------------------------
data_polity <- data_polity %>%
  filter(ccode != 364 | year != 1922) %>%
  mutate(ccode = ifelse(ccode == 364, 365, ccode))

## ----sudan-overlap-------------------------------------------------------
find_overlap(625, 626)

## ----recode-sudan--------------------------------------------------------
data_polity <- data_polity %>%
  filter(ccode != 626 | year != 2011) %>%
  mutate(ccode = ifelse(ccode == 626, 625, ccode),
         ccode = ifelse(ccode == 525, 626, ccode))

## ----pakistan-overlap----------------------------------------------------
find_overlap(769, 770)

## ----recode-pakistan-----------------------------------------------------
data_polity <- data_polity %>%
  mutate(ccode = ifelse(ccode == 769, 770, ccode))

## ----vietnam-overlap-----------------------------------------------------
find_overlap(816, 818)

## ----recode-vietnam------------------------------------------------------
data_polity <- data_polity %>%
  filter(ccode != 818 | year != 1976) %>%
  mutate(ccode = ifelse(ccode == 818, 816, ccode))


# Create indicators and select key variables
data_polity <- data_polity %>%
                group_by(ccode, year) %>%
                select(ccode, year, 
                       exrec, polcomp, xconst) %>%
                mutate(
                  open.rec = ifelse(exrec == 8, 1, 0),
                  open.comp = ifelse(polcomp == 10, 1, 0),
                  exec.cons = ifelse(xconst == 7, 1, 0),
                  open.pol = open.rec + open.comp
                ) %>%
                left_join(polyarchy) %>%
                left_join(lied) %>%
                left_join(vdem.polyarchy) %>%
                mutate(
                 open.dum = ifelse(lexical_index_original >= 4, 1, 0)
                )
table(data_polity$open.pol)
glimpse(data_polity)


### Merge polity w/ atop-cow-year data
atop.cow.year <- left_join(atop.cow.year, data_polity)



###--Clean geddes et al data before merging------
# geddes et al autocracy types
gwf.data <- read_dta("data/GWF_AllPoliticalRegimes.dta")
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



### Create alliance-year data by summarizing key state chararaceristics

alliance.year <- atop.cow.year %>%
  group_by(atopid) %>%
  mutate(
    begyr = min(year)
  ) %>% # ID start year
  filter(atopid > 0) %>% # remove no alliance obs
  filter(begyr == year) %>% # retain start year
  group_by(atopid, year) %>% # group and summarize
  mutate(
    most.cap = ifelse(cinc == max(cinc, na.rm = TRUE), 1, 0),
    maxcap.democ = ifelse(most.cap == 1, polity2, 0),
    maxcap.rec = ifelse(most.cap == 1, open.rec, 0),
    maxcap.comp = ifelse(most.cap == 1, open.comp, 0),
    maxcap.cons = ifelse(most.cap == 1, exec.cons, 0),
    maxcap.open = ifelse(most.cap == 1, open.pol, 0),
    
    # polyarchy dim
    maxcap.cont.std = ifelse(most.cap == 1, CONTESTstd, 0),
    maxcap.inc.std = ifelse(most.cap == 1, INCLUSstd, 0),
    maxcap.poly = ifelse(most.cap == 1, v2x_polyarchy, 0),
    
    # lied dim
    maxcap.open.lied = ifelse(most.cap == 1, opposition, 0),
    maxcap.comp.lied = ifelse(most.cap == 1, competition, 0),
    maxcap.lied = ifelse(most.cap == 1, lexical_index_original, 0),
    
    # GWF coding 
    maxcap.party = ifelse(most.cap == 1, gwf_party, 0),
    maxcap.military = ifelse(most.cap == 1, gwf_military, 0),
    maxcap.personal = ifelse(most.cap == 1, gwf_personal, 0),
    maxcap.monarchy = ifelse(most.cap == 1, gwf_monarchy, 0),
    
    cinc.share = cinc / sum(cinc, na.rm = TRUE),
    democ.weight = polity2 * cinc.share,
    democ = ifelse(polity2 > 5, 1, 0),
    open = ifelse(open.pol >= 1, 1, 0)
  ) %>% 
  summarize(
    avg.democ = mean(polity2, na.rm = TRUE),
    max.democ = max(polity2, na.rm = TRUE),
    min.democ = min(polity2, na.rm = TRUE),
    maxcap.democ.max = max(maxcap.democ, na.rm = TRUE),
    maxcap.democ.min = min(maxcap.democ, na.rm = TRUE),
    joint.democ = ifelse(min.democ > 5, 1, 0), 
    democ.count = sum(democ, na.rm = TRUE),

    # means of dummies: functionally proportions    
    prop.comp = mean(open.comp, na.rm = TRUE),
    prop.rec = mean(open.rec, na.rm = TRUE),
    prop.cons = mean(exec.cons, na.rm = TRUE),
    prop.open = mean(open.dum, na.rm = TRUE),
    
    # POLITY summaries
    maxcap.rec = max(maxcap.rec, na.rm = TRUE),
    maxcap.comp = max(maxcap.comp, na.rm = TRUE),
    maxcap.cons = max(maxcap.cons, na.rm = TRUE),
    open.count = sum(open, na.rm = TRUE),
    maxcap.open = max(maxcap.open, na.rm = TRUE),
    
    # polyarchy dim
    maxcap.cont.std = sum(maxcap.cont.std, na.rm = TRUE),  # works as all others are zero
    maxcap.inc.std = sum(maxcap.inc.std, na.rm = TRUE),
    maxcap.poly = max(maxcap.poly, na.rm = TRUE),
    
    # lied dim
    maxcap.open.lied = max(maxcap.open.lied, na.rm = TRUE),
    maxcap.comp.lied = max(maxcap.comp.lied, na.rm = TRUE),
    maxcap.lied = max(maxcap.lied, na.rm = TRUE),
  
    total.cap = sum(cinc, na.rm = TRUE),
    total.expend = sum(ln.milex, na.rm = TRUE),
    total.gdp = sum(gdp, na.rm = TRUE),
    num.mem = n(),
    
    dem.prop = democ.count / num.mem,
    open.prop = open.count / num.mem,
    avg.democ.weight = mean(democ.weight, na.rm = TRUE),
    
    # types of autocracies 
    maxcap.party = max(maxcap.party, na.rm = TRUE),
    maxcap.military = max(maxcap.military, na.rm = TRUE),
    maxcap.personal = max(maxcap.personal, na.rm = TRUE),
    maxcap.monarchy = max(maxcap.monarchy, na.rm = TRUE),
    
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


# Create separate democracy data
alliance.democ <- alliance.year %>% 
  select(c(atopid, dem.prop, joint.democ, avg.democ, max.democ, min.democ, 
           avg.democ.weight, 
           max.threat, min.threat, mean.threat, 
           maxcap.democ.min, maxcap.democ.max,
           maxcap.cons, maxcap.rec, maxcap.comp, maxcap.open,
           prop.open, prop.cons,
           maxcap.cont.std, maxcap.inc.std,
           maxcap.comp.lied, maxcap.open.lied, maxcap.lied,
           maxcap.poly))
write.csv(alliance.democ, "data/alliance-democ.csv",
          row.names = FALSE)



