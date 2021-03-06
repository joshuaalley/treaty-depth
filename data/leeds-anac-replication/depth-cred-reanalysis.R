# Joshua Alley
# check Leeds and Anac on institutionalization
# And examine berkemeier and fuhrmann 2018


# load data
la.rep <- read_dta("data/leeds-anac-replication/LeedsAnacIIrep.dta")

# replicate Leeds and Anac- focus on active support
la.rep <- filter(la.rep, active == 1)

la.logit <- glm(honor3 ~ milinst + formal + chcap2d +
                  dinstch2 + origtar1, 
                family = binomial(link = "logit"),
                data = la.rep)
summary(la.logit)

# some violate, some honor
la.logit2 <- glm(honor2 ~ milinst + formal + chcap2d +
                  dinstch2 + origtar1, 
                family = binomial(link = "logit"),
                data = la.rep)
summary(la.logit2)

# add data on depth
la.rep$atopid <- as.numeric(la.rep$atopid)
la.rep <- left_join(la.rep, select(atop.milsup, atopid, latent.depth.mean), 
                    by = "atopid")
summary(la.rep$latent.depth.mean)

# treaty depth instead
la.logit.depth <- glm(honor3 ~ latent.depth.mean + formal + chcap2d +
                  dinstch2 + origtar1, 
                family = binomial(link = "logit"),
                data = la.rep)
summary(la.logit.depth)

# some violate, some honor
la.logit2.depth <- glm(honor2 ~ latent.depth.mean + formal + chcap2d +
                   dinstch2 + origtar1, 
                 family = binomial(link = "logit"),
                 data = la.rep)
summary(la.logit2.depth)


### Berkemeier and Fuhrmann 2018 
# load berkemeier and fuhrmann data
bf.rep <- read_dta("data/leeds-anac-replication/BerkemeierFuhrmannRAP2018_0303.dta")
glimpse(bf.rep)
table(bf.rep$dnaLLM)

# replace NA with 0
bf.rep$dnaLLM[is.na(bf.rep$dnaLLM)] <- 0
bf.rep$honorLLM[is.na(bf.rep$honorLLM)] <- 0
bf.rep$violateLLM[is.na(bf.rep$violateLLM)] <- 0

bf.rep$sabroskyhonorllm[is.na(bf.rep$sabroskyhonorllm)] <- 0
table(bf.rep$sabroskyhonorllm)

# filter down
table(bf.rep$dnaLLM)
bf.rep <- filter(bf.rep, dnaLLM != 1)

table(bf.rep$honorLLM)
table(bf.rep$violateLLM)
table(bf.rep$honorLLM, bf.rep$violateLLM)

# filter to active alliances
bf.rep <- filter(bf.rep, offense == 1 | defense == 1)
table(bf.rep$honorLLM)

# add my data
bf.rep$atopid <- as.numeric(bf.rep$atopid)
bf.rep <- left_join(bf.rep, atop.milsup, by = "atopid")

# model honored
bf.logit.depth <- glm(honorLLM ~ latent.depth.mean +
                      non.maj.only + asymm.cap +
                        post45 +  prop.open + prop.cons +
                        num.mem + econagg.dum + uncond.milsup +
                        fp.conc.index,
                      family = binomial(link = "logit"),
                      data = bf.rep)
summary(bf.logit.depth)


# model honored w/ sabrosky coding
bf.logits.depth <- glm(sabroskyhonorllm ~ latent.depth.mean +
                        non.maj.only + asymm.cap +
                        post45 + prop.open + prop.cons +
                        num.mem + econagg.dum + uncond.milsup +
                        fp.conc.index,
                      family = binomial(link = "logit"),
                      data = bf.rep)
summary(bf.logits.depth)


stargazer(la.logit, la.logit.depth, bf.logit.depth,
          style = "all2",
          dep.var.labels=c("Leeds and Anac",
                           "Berkemeier and Fuhrmann"),
          order = c("milinst", "latent.depth.mean", "formal", "chcap2d",
                      "dinstch2", "origtar1", "non.maj.only", "asymm.cap",
                      "post45", "maxcap.open", "maxcap.cons",
                      "num.mem", "econagg.dum", "uncond.milsup",
                      "fp.conc.index"),
          covariate.labels=c(
            "Military Institutionalization", "Latent Depth",
            "Alliance Formality", "Capability Change", "Process Change",
            "Original Target",
            "Asymmetric Capability", "Non-Major Only", "Post 1945",
            "Share Electoral Democracy", "Share Executive Constraints", 
            "Number of Members", "Economic Issue Linkage", "Unconditional Support",
            "Foreign Policy Concessions"
          ),
          keep.stat = c("n","ll"), ci=TRUE, 
          star.char = c("", "", ""),
          notes = "95\\% Confidence Intervals in Parentheses.", 
          notes.append = FALSE,
          label = c("tab:depth-performance")
      )
