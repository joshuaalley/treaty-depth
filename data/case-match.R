# Joshua Alley  
# Texas A&M University
# Select cases for analysis by matching 


# create a case dataset
case.data <- select(atop.milsup, atopid, latent.depth.mean, avg.democ, econagg.dum, uncond.milsup, 
                    fp.conc.index, num.mem, wartime, asymm,
                    low.kap.sc, begyr, asymm.cap, non.maj.only) %>% 
                    drop_na()

# look at matches
case.data <- as.data.frame(case.data) # doesn't take tibble input 
match.res <- case.match(case.data, id.var = "atopid",
                   leaveout.vars = c("atopid", "latent.depth.mean", 
                                     "non.maj.only", "uncond.milsup", 
                                     "asymm.cap"),    
                    distance="mahalanobis", case.N = 2, 
                    number.of.matches.to.return = 5,
                   outcome.var = "latent.depth.mean", max.variance.outcome = TRUE,
                    treatment.var = "non.maj.only", max.variance = TRUE)
match.res

# preferred match w/ asymm cap 
filter(case.data, atopid == 2360 | atopid == 3300)
# preferred match w/o asymm cap
filter(case.data, atopid == 1310 | atopid == 3310)
