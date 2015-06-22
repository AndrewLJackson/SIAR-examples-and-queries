# this demo generates some random data for M consumers based on N samples and
# caculates the Bayesian estimated layman metrics

rm(list = ls())
graphics.off()

library(siar)

# ------------------------------------------------------------------------------
# ANDREW - REMOVE THESE LINES WHICH SHOULD BE REDUNDANT
# change this line
#setwd("c:/rtemp")
# ------------------------------------------------------------------------------



# read in some data
mydata <- read.table("example_layman_data.txt", header=T,sep="\t")
attach(mydata) # make the names of the columns available for direct calling

# calculate the Bayesian Layman metrics given data for Isotopes 1 and 2, 
# a grouping variable Group and a number of iterations to use to generate
# the results
metrics <- siber.hull.metrics(Iso1,Iso2,Group,R=10^4)




# ------------------------------------------------------------------------------
# Plot out some of the data and results
# ------------------------------------------------------------------------------

# Plot the raw [simulated in this case] data

xlabels <- attributes(metrics)$dimnames[[2]]

# Now lets calculate the convex hull as per the current method based
# simply on the means for each group
means.x <- aggregate(Iso1,list(Group),mean)$x
means.y <- aggregate(Iso2,list(Group),mean)$x
sample.hull <- convexhull(means.x,means.y)

M <- max(Group)

#dev.new()
par(mfrow=c(1,1))
plot(Iso1,Iso2,col=Group,xlab="Isotope 1", ylab="Isotope 2",
  pch=1,asp=1,xlim=c(min(Iso1)-2,max(Iso1)+2),ylim=c(min(Iso2)-2,max(Iso2)+2))
lines(sample.hull$xcoords,sample.hull$ycoords,lty=1,col=1,lwd=2)
legend("topright",
  legend=as.character(c(paste("Group ",1:M),"sample hull", "population hull")),
  pch=c(rep(1,M),NA, NA),col=c(1:M,1,1), lty=c(rep(NA,),1,3))

# in this example, I plot TA as a histogram seperately to the other
# metrics as it is usually on a scale so vastly different from the other 
# metrics.
#dev.new()
par(mfrow=c(1,2))
hist(metrics[,"TA"],freq=F,xlab="TA",ylab="Density",main="")
siardensityplot(metrics[,c(1,2,4,5,6)],
      xticklabels=xlabels[c(1,2,4,5,6)],
      ylims=c(0,25),ylab="units",xlab="Metric")


detach(mydata)