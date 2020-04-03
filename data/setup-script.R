# Joshua Alley
# Texas A&M University
# Script to load packages and manage conflicts in project for alliance treaty depth

# Run this at the start of every session


# load packages
library(MASS)
library(haven)
library(tidyverse)
library(coefplot)
library(betareg)
library(brms)
library(sjstats)
library(arm)
library(bfa)
library(coda)
library(stargazer)
library(xtable)
library(caseMatch)
library(GJRM)
library(gridExtra)
library(conflicted)


# Look at conflicts 
conflict_scout()

# Set preferences
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("ngrps", "brms")
conflict_prefer("chol2inv", "Matrix")
conflict_prefer("coefplot", "coefplot") 
conflict_prefer("coefplot.default", "coefplot") 
conflict_prefer("expand", "tidyr") 
conflict_prefer("lag", "dplyr")
conflict_prefer("Position", "ggplot2")
conflict_prefer("rcond", "Matrix")
conflict_prefer("pack", "tidyr")
conflict_prefer("unpack", "tidyr")
conflict_prefer("display", "xtable")
conflict_prefer("traceplot", "coda")
conflict_prefer("collapse", "dplyr")
conflict_prefer("t2", "mgcv")
conflict_prefer("mm", "brms")
conflict_prefer("lmList", "nlme")
conflict_prefer("s", "mgcv") 
conflict_prefer("combine", "dplyr")
conflict_prefer("ar", "brms")

# Set up RSTAN guidelines
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# Set seed
set.seed(12)




# Define multiplot function from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/ 
multiplot.ggplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
