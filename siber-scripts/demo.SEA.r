rm(list = ls()) # clear the memory of objects

# load the SIBER package of functions
library(SIBER)

# Optionally load the viridis package for colour-blind and 
# print-friendly colour palettes
library(viridis)

# create a palette of 4 colours. NB needs to be changed if you 
# have more than 4 groups.
palette(viridis(4))

# ------------------------------------------------------------------------------
# ANDREW - REMOVE THESE LINES WHICH SHOULD BE REDUNDANT
# change this line
#setwd("c:/rtemp")
#setwd("/Users/andrewjackson/Dropbox/siar/demo scripts and files/siber scripts")
#setwd( "D:/Alternative My Documents/Andrews Documents/Dropbox/siar/demo scripts and files/siber scripts")
# ------------------------------------------------------------------------------

# ---------------------------------------------------------------------
# Read in the data and set up for analysis
# ---------------------------------------------------------------------


# read in the data
mydata <- read.csv("example_layman_data.csv",
                   header=T)

# create the siber object
siber.example <- createSiberObject(mydata)

# ---------------------------------------------------------------------
# Plot the raw data and generate summary statistics
# ---------------------------------------------------------------------


# Create lists of plotting arguments to be passed onwards to each 
# of the three plotting functions.
community.hulls.args <- list(col = 1, lty = 1, lwd = 1)
group.ellipses.args  <- list(n = 100, p.interval = 0.95, 
                             lty = 1, lwd = 2)
group.hull.args      <- list(lty = 2, col = "grey20")


# ellipses and group.hulls are set to TRUE or T for short to force
# their plotting. 
par(mfrow=c(1,1))
plotSiberObject(siber.example,
                ax.pad = 2, 
                hulls = F, community.hulls.args, 
                ellipses = T, group.ellipses.args,
                group.hulls = T, group.hull.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~'\u2030'),
                ylab = expression({delta}^15*N~'\u2030')
)

# You can add more ellipses by directly calling plot.group.ellipses()
# Add an additional p.interval % prediction ellilpse
plotGroupEllipses(siber.example, n = 100, p.interval = 0.95,
                  lty = 1, lwd = 2)

# or you can add the XX% confidence interval around the bivariate means
# by specifying ci.mean = T along with whatever p.interval you want.
plotGroupEllipses(siber.example, n = 100, p.interval = 0.95, ci.mean = T,
                  lty = 1, lwd = 2)

# Calculate sumamry statistics for each group: TA, SEA and SEAc
group.ML <- groupMetricsML(siber.example)
print(group.ML)


# ---------------------------------------------------------------------
# FIT THE BAYESIAN MULTIVARIATE MODEL TO EACH GROUP
# ---------------------------------------------------------------------

# options for running jags
parms <- list()
parms$n.iter <- 2 * 10^4   # number of iterations to run the model for
parms$n.burnin <- 1 * 10^3 # discard the first set of values
parms$n.thin <- 10     # thin the posterior by this many
parms$n.chains <- 2        # run this many chains

# define the priors
priors <- list()
priors$R <- 1 * diag(2)
priors$k <- 2
priors$tau.mu <- 1.0E-3

# fit the ellipses which uses an Inverse Wishart prior
# on the covariance matrix Sigma, and a vague normal prior on the 
# means. Fitting is via the JAGS method.
ellipses.posterior <- siberMVN(siber.example, parms, priors)

# ---------------------------------------------------------------------
# Calculate and Plot the Bayesian Ellipses
# ---------------------------------------------------------------------


# The posterior estimates of the ellipses for each group can be used to
# calculate the SEA.B for each group.
SEA.B <- siberEllipses(ellipses.posterior)

siberDensityPlot(SEA.B, xticklabels = colnames(group.ML), 
                 xlab = c("Community | Group"),
                 ylab = expression("Standard Ellipse Area " ('\u2030' ^2) ),
                 bty = "L",
                 las = 1,
                 main = "SIBER ellipses on each group"
)

# Add red x's for the ML estimated SEA-c
points(1:ncol(SEA.B), group.ML[3,], col="red", pch = "x", lwd = 2)

# Calculate some credible intervals 
cr.p <- c(0.95, 0.99) # vector of quantiles

# call to hdrcde:hdr using lapply()
SEA.B.credibles <- lapply(
  as.data.frame(SEA.B), 
  function(x,...){tmp<-hdrcde::hdr(x)$hdr},
  prob = cr.p)

print(SEA.B.credibles)

# do similar to get the modes, taking care to pick up multimodal posterior
# distributions if present
SEA.B.modes <- lapply(
  as.data.frame(SEA.B), 
  function(x,...){tmp<-hdrcde::hdr(x)$mode},
  prob = cr.p, all.modes=T)

print(SEA.B.modes)


# ---------------------------------------------------------------------
# Compare the posterior distributions of SEA
# ---------------------------------------------------------------------


# Proportion, and hence probability that the SEA for Group 1
# is less than that for Group 2
Pg1.lt.g2 <- sum( SEA.B[,1] < SEA.B[,2] ) / nrow(SEA.B)
print(Pg1.lt.g2)

# Compare Group 1 and 3 similarly
Pg1.lt.g3 <- sum( SEA.B[,1] < SEA.B[,3] ) / nrow(SEA.B)
print(Pg1.lt.g3 )

# And the remaining pair-wise comparisons
Pg1.lt.g4 <- sum( SEA.B[,1] < SEA.B[,4] ) / nrow(SEA.B)
print(Pg1.lt.g4)

Pg2.lt.g3 <- sum( SEA.B[,2] < SEA.B[,3] ) / nrow(SEA.B)
print(Pg2.lt.g3)

Pg3.lt.g4 <- sum( SEA.B[,3] < SEA.B[,4] ) / nrow(SEA.B)
print(Pg3.lt.g4)


# ---------------------------------------------------------------------
# Calculate overlap between pairs of ellipses
# ---------------------------------------------------------------------

# We have to go back to the original data and split the isotope
# data based on the group identifier. NB, this bit of code wont
# work if you also have multiple communities. For now...
spx <- split(siber.example$original.data$iso1, 
             siber.example$original.data$group)
spy <- split(siber.example$original.data$iso2,
             siber.example$original.data$group)

# The overlap function still lives in the siar package so we 
# have to load that here to acheive this.
library(siar)

overlap.G2.G3 <- overlap(as.numeric(spx[[2]]), spy[[2]], 
                         spx[[3]], spy[[3]],
                         steps = 1)
print(overlap.G2.G3)

# You might want to present a proportion overlap: and you have some 
# options...

# Area of overlap between group 2 and 3 as a proportion of the size
# of ellipse for group 2
overlap.G2.G3$overlap / overlap.G2.G3$area1

# Area of overlap between group 2 and 3 as a proportion of the size
# of ellipse for group 3
overlap.G2.G3$overlap / overlap.G2.G3$area2

# Area of overlap between group 2 and 3 as a proportion of the size
# of both ellipses for group 2 and 3.
overlap.G2.G3$overlap / (overlap.G2.G3$area1 + overlap.G2.G3$area2)

