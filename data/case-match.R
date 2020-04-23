# Joshua Alley  
# Texas A&M University
# Select cases for analysis by matching 


# create a case dataset
case.data <- select(atop.milsup, atopid, latent.depth.mean, maxcap.democ, econagg.dum, uncond.milsup, 
                    fp.conc.index, num.mem, wartime, asymm,
                    low.kap.sc, begyr, asymm.cap) %>% 
                    drop_na()

# look at matches
case.data <- as.data.frame(case.data) # doesn't take tibble input 
match.res <- case.match(case.data, id.var = "atopid",
                   leaveout.vars = c("atopid", "maxcap.democ", "latent.depth.mean", 
                                     "uncond.milsup"),    
                    distance="mahalanobis", case.N = 2, 
                    number.of.matches.to.return = 5,
                   outcome.var = "latent.depth.mean", max.variance.outcome = TRUE,
                    treatment.var = "maxcap.democ")
match.res

filter(case.data, atopid == 3340 | atopid == 3350)
filter(case.data, atopid == 3410 | atopid == 3423)
# here's the best case for the mechanism 
filter(case.data, atopid == 3535 | atopid == 3750)
filter(case.data, atopid == 3322 | atopid == 3395)
filter(case.data, atopid == 3345 | atopid == 3350)
