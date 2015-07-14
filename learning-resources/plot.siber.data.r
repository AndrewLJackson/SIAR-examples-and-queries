plot.siber.data <- function (fname, tt, add.hull = T, ...) {

require(siar)
  
#full.path <- paste(getwd(), fname , sep = "")

mydata <- read.csv(fname, header=T)

M <- max(mydata$Group)

with(mydata, {

# ------------------------------------------------------------------------------
# Plot out some of the data and results
# ------------------------------------------------------------------------------

# Plot the raw [simulated in this case] data

# Now lets calculate the convex hull as per the current method based
# simply on the means for each group
means.x <- aggregate(Iso1,list(Group),mean)$x
means.y <- aggregate(Iso2,list(Group),mean)$x
sample.hull <- convexhull(means.x,means.y)



#dev.new()
#par(mfrow=c(1,1))
plot(Iso1,Iso2,col=Group,
     xlab=expression({delta}^13*C~'\u2030'),
     ylab=expression({delta}^15*N~'\u2030'),
     pch=1,
     main = tt,
     asp = 1, 
     bty = "L",
     ...)

if (add.hull)
{lines(sample.hull$xcoords,sample.hull$ycoords,lty=1,col=1,lwd=2)}

#legend("topleft",
#  legend=as.character(c(paste("Group ",1:M),"sample hull")),
#  pch=c(rep(1,M),NA),col=c(1:M,1,1), lty=c(rep(NA,M),1),
#  bty="n")


}) # end of with(mydata, {...})

return(M) # needed by calling code chunk

} # end of function
