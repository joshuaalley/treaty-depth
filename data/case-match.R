# Joshua Alley  
# Texas A&M University
# Select cases for analysis by matching 


# create a case dataset
case.data <- select(atop.milsup, atopid, latent.depth.mean, avg.democ, econagg.dum, uncond.milsup, 
                    fp.conc.index, num.mem, wartime, asymm,
                    low.kap.sc, begyr, asymm.cap) %>% 
                    drop_na()

# look at matches
case.data <- as.data.frame(case.data) # doesn't take tibble input 
match.res <- case.match(case.data, id.var = "atopid",
                   leaveout.vars = c("atopid", "avg.democ", "latent.depth.mean", 
                                     "uncond.milsup"),    
                    distance="mahalanobis", case.N = 2, 
                    number.of.matches.to.return = 5,
                   outcome.var = "latent.depth.mean", max.variance.outcome = TRUE,
                    treatment.var = "avg.democ")
match.res

# here's the best case for the mechanism 
filter(case.data, atopid == 3535 | atopid == 3750)
