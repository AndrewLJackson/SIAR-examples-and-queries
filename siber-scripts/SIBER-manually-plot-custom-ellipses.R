
graphics.off()
rm(list=ls())
library(SIBER)
# ------------------------------------------------------------------------------
# Load some example data
data(demo.siber.data)

# create the siber object
my.siber.data <- createSiberObject(demo.siber.data)


# ------------------------------------------------------------------------------
# customise the colour order
# here i there are only 3 groups per community, but i am specifying 4 colours 
# as i dont like the lightest colour very much in plots.
palette(viridis::viridis(4))

# plot the isotope data, without any hulls or ellipses
plotSiberObject(my.siber.data,
                hulls = FALSE,
                ellipses = FALSE,
                group.hulls = FALSE)

# Call addEllipse directly on each group to customise the plot fully

# change c.id and g.id to select the group of data you want
# you could embed this in a loop easily enough if you wanted to 
# set up the order of lines and simply loop through them.
c.id <- 1 # specify the community ID
g.id <- 1 # specify the group ID within the community

# see help file for addEllipse for more information
# NB i am using the group identifier g.id to select the colour
# of the ellipse line.
addEllipse(my.siber.data$ML.mu[[c.id]][ , , g.id],
           my.siber.data$ML.cov[[c.id]][ , , g.id],
           m = NULL,
           n = 100,
           p.interval = NULL,
           ci.mean = FALSE,
           col = g.id,
           lty = 2,
           lwd = 1)

# ------------------------------------------------------------------------------
