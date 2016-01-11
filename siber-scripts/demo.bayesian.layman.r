# this demo generates some random data for M consumers based on N samples and
# caculates the Bayesian estimated layman metrics

rm(list = ls())
graphics.off()

library(SIBER)

# optionally load the viridis colour package
library(viridis)

# and if loaded, designate a new colour palette of length = 4
palette(viridis(4))

# -----------------------------------------------------------------

# read in data from the current working directory
mydata <- read.csv("example_layman_data_all.csv", 
                   header = TRUE)

# create the siber object
siber.example <- createSiberObject(mydata)

# -----------------------------------------------------------------
# Plot the raw data
# -----------------------------------------------------------------

# Create lists of plotting arguments to be passed onwards to each 
# of the three plotting functions.
community.hulls.args <- list(col = 1, lty = 1, lwd = 1)
group.ellipses.args  <- list(n = 100, p.interval = 0.95, lty = 1, lwd = 2)
group.hull.args      <- list(lty = 2, col = "grey20")

# plot the raw data
par(mfrow=c(1,1))
plotSiberObject(siber.example,
                ax.pad = 2, 
                hulls = T, community.hulls.args, 
                ellipses = F, group.ellipses.args,
                group.hulls = F, group.hull.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~'\u2030'),
                ylab = expression({delta}^15*N~'\u2030')
)

# add the confidence interval of the means to help locate
# the centre of each data cluster
plotGroupEllipses(siber.example, n = 100, p.interval = 0.95,
                  ci.mean = T, lty = 1, lwd = 2)



# -----------------------------------------------------------------
# Fit the Bayesian models
# -----------------------------------------------------------------

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

# -----------------------------------------------------------------
# Calculate the layman metrics on these posteriors
# -----------------------------------------------------------------

# extract the posterior means
mu.post <- extractPosteriorMeans(siber.example, ellipses.posterior)

# calculate the corresponding distribution of layman metrics
layman.B <- bayesianLayman(mu.post)


# --------------------------------------
# Visualise the first community
# --------------------------------------

# drop the 3rd column of the posterior which is TA using -3.
siberDensityPlot(layman.B[[1]][ , -3], 
                 xticklabels = colnames(layman.B[[1]][ , -3]), 
                 bty="L", ylim = c(0,20))

# add the ML estimates (if you want). Extract the correct means 
# from the appropriate array held within the overall array of means.
comm1.layman.ml <- laymanMetrics(siber.example$ML.mu[[1]][1,1,],
                                 siber.example$ML.mu[[1]][1,2,]
)

# again drop the 3rd entry which relates to TA
points(1:5, comm1.layman.ml$metrics[-3], 
       col = "red", pch = "x", lwd = 2)


# --------------------------------------
# Visualise the second community
# --------------------------------------
siberDensityPlot(layman.B[[2]][ , -3], 
                 xticklabels = colnames(layman.B[[2]][ , -3]), 
                 bty="L", ylim = c(0,20))

# add the ML estimates. (if you want) Extract the correct means 
# from the appropriate array held within the overall array of means.
comm2.layman.ml <- laymanMetrics(siber.example$ML.mu[[2]][1,1,],
                                 siber.example$ML.mu[[2]][1,2,]
)
points(1:5, comm2.layman.ml$metrics[-3], 
       col = "red", pch = "x", lwd = 2)


# ------------------------------------------
# Plot both TAs from each community together
# ------------------------------------------

# go back to a 1x1 panel plot
par(mfrow=c(1,1))

# Now we only plot the TA data. We could address this as either
# layman.B[[1]][, "TA"]
# or
# layman.B[[1]][, 3]
siberDensityPlot(cbind(layman.B[[1]][ , "TA"], 
                       layman.B[[2]][ , "TA"]),
                 xticklabels = c("Community 1", "Community 2"), 
                 bty="L", ylim = c(0, 90),
                 las = 1,
                 ylab = "TA - Convex Hull Area",
                 xlab = "")

# ------------------------------------------
# Calcualte the probability that dN-range for 
# community 1 is less than that for 
# community 2
# ------------------------------------------

dNr1.lt.dNr2 <- sum(layman.B[[1]][,"dY_range"] < 
                      layman.B[[2]][,"dY_range"]) / 
  length(layman.B[[1]][,"TA"])

print(dNr1.lt.dNr2)



