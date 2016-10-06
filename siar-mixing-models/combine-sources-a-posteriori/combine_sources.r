# This script, illustrates how you can aggregate sources a posteriori 
# and make probability statements on the combined data.

# load the siar library
library(siar)

# remove any pre-existing and open graphics windows
graphics.off()

# ------------------------------------------------------------------------------
# After that bit of house-keeping. Read in the data files.
# ------------------------------------------------------------------------------

# make sure you set the current directory to the folder in which you put these
# downloaded files.
# setwd( "D:/Alternative My Documents/Andrews Documents/Work/SIAR/demos/siar_v4.2_combine_sources_a_posteriori")
# setwd("C:/SIARdemo")    # for example.

# read in you source data
sources <- read.csv("sourcesdemo.csv",header=TRUE)

# read in your consumer data
consumers <- read.csv("geese2demo.csv",header=TRUE)
consumers <- as.matrix(consumers) # line to get around the R v3 introduced bug

# alternative dataset comprising only a single group of consumers
# comment out the line above and replace with this if you want.
#consumers <- read.csv("geese1demo.csv",header=TRUE)

# read in your corrections data
corrections <- read.csv("correctionsdemo.csv",header=TRUE)


# NB if you dont have any corrections make them zero
# corrections <- 0

# read in the elemental concentration data
# see Phillips DL, Koch PL (2002) Incorporating concentration dependence in stable
# isotope mixing models. Oecologia 130: 114?125. for more details.
concs <- 0 # use this line if you dont want to run with concentration dependence
#concs <- read.csv("concdepdemo.csv", header=TRUE)


# ------------------------------------------------------------------------------
# Thats the data read in... now we can run the model and analyse the results
# ------------------------------------------------------------------------------

# this line calls the SIAR model for either multiple or single groups
# of consumers depending on the format of the consumers dataset
model1 <- siarmcmcdirichletv4(consumers, sources, corrections, concs)

# this line plots the raw isotope data for sources and consumers as a bi-plot.
# The trophic enrichment factors have been applied to the sources.
# You will be asked to position a legend on the screen by left clicking.
siarplotdata(model1,iso=c(2,1))

# run a matrix plot to convince yourself that the model is finding it difficult
# to distinguish between U.lactuca and Enteromorpha
siarmatrixplot(model1)

# Now we need to aggregate the appropriate columns of the posterior estimates
# by simply summing them. In this case, it turns out that Enteromorpha and 
# Ulva Lactuca are in fact the two phenotypes of the same species so its 
# probably far to aggregate them.
# http://en.wikipedia.org/wiki/Enteromorpha.
# Also, the matrix plots tells us that siar
# is finding it difficult to distinguish between their contribution to the 
# consumer as they lie close to one another in isotope space.
#
# we can do this manually or through a loop to go through each of the
# eight groups.

# manually
G1.entero.ulva <- rowSums(model1$output[,c("U.lactucaG1","EnteromorphaG1")])
G2.entero.ulva <- rowSums(model1$output[,c("U.lactucaG2","EnteromorphaG2")])

entero.plus.ulva <- matrix(0,nrow=nrow(model1$output),ncol=model1$numgroups,
                            dimnames=list(NULL,paste("EplusU.G",1:model1$numgroups,sep="")))

# via a loop
for ( i in 1: model1$numgroups){
  
  # create the text labels for the appropriate columns
  ulva <- paste("U.lactuca","G",i,sep="") #"U.lactucaGi"
  entero <- paste("Enteromorpha","G",i,sep="") #"EnteromorphaGi"
  
  # do the sum
  entero.plus.ulva[,i] <- rowSums(model1$output[,c(ulva,entero)])
  

}

# and plot the aggregated values
#dev.new()
siardensityplot(entero.plus.ulva)


# now, if you want to plot all the sources for group 1, you need to extract them
# and marry them to the entero.plus.ulva posterior we just created
G1.aggregated <-  cbind(model1$output[,c("ZosteraG1","GrassG1")],
                          entero.plus.ulva[,"EplusU.G1"])
                          
# and plot this
#dev.new()
siardensityplot(G1.aggregated)

# calculate the credible intervals for these a posteriori aggregations
# using hdrcde::hdr() by applying it repeatedly down each column (dim = 2)
# hence apply(X, 2, fun)
post.creds <- apply(G1.aggregated, 2, hdrcde::hdr)


# save your model to a file name of your choice
# save(model1,file="your_choice.rdata")

# For more details on the Bayesian method i suggest reading
# McCarthy, M.A. 2007. Bayesian methods for Ecology. Cambridge University Press.


