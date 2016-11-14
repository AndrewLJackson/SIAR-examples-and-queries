set.seed(1)

library(SIBER)


# load in the included demonstration dataset
data("demo.siber.data")
#
# create the siber object
siber.example <- createSiberObject(demo.siber.data)


par(mfrow=c(1,1))

community.hulls.args <- list(col = 1, lty = 1, lwd = 1)
group.ellipses.args  <- list(n = 100, p.interval = 0.95, lty = 1, lwd = 2)
group.hull.args      <- list(lty = 2, col = "grey20")

# this time we will make the points a bit smaller by 
# cex = 0.5
plotSiberObject(siber.example,
                ax.pad = 2, 
                hulls = F, community.hulls.args, 
                ellipses = F, group.ellipses.args,
                group.hulls = F, group.hull.args,
                bty = "L",
                iso.order = c(1,2),
                xlab=expression({delta}^13*C~'\u2030'),
                ylab=expression({delta}^15*N~'\u2030'),
                cex = 0.5
)



# Calculate sumamry statistics for each group: TA, SEA and SEAc
group.ML <- groupMetricsML(siber.example)
print(group.ML)

# You can add more ellipses by directly calling plot.group.ellipses()
# Add an additional p.interval % prediction ellilpse
plotGroupEllipses(siber.example, n = 100, p.interval = 0.95,
                  lty = 1, lwd = 2)

# or you can add the XX% confidence interval around the bivariate means
# by specifying ci.mean = T along with whatever p.interval you want.
plotGroupEllipses(siber.example, n = 100, p.interval = 0.95, ci.mean = T,
                  lty = 1, lwd = 2)



# -----------------------------------------------------------------------
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

# -----------------------------------------------------------------------
# the fitted ellipses are held in ellipses.posterior, as a series of 
# list objects.

# this is the first 6 draws of the ellipse fitted to group 1.2
head(ellipses.posterior$'1.2')

# or if you wanted to call it in a loop, it would be easier to use
head(ellipses.posterior[[2]])

# the order of the groups is given by 
names(ellipses.posterior)

# this is the first 6 draws of the ellipse fitted to group 1.3
head(ellipses.posterior[[3]])

# -----------------------------------------------------------------------
# MAGIC STARTS HERE - this needs to be looped over group combinations and posterior draws
# -----------------------------------------------------------------------
# we can now calculate the x and y coordinates of 95% two ellipses
# NB, the do.plot option only works on the master version of SIBIER 
# hosted on github 
# devtools::install_github("andrewljackson/SIBER",
#                          build_vingettes = TRUE)
# library(SIBER)

# coordinates of ellipse for group [[1]] = 1.3 (red circles)

do.plot = T

# index for the posterior draw we want 
# (you can just compare ith draw with ith draw as they are uncorrelated)
draw.idx <- 1 

grp.idx <- 2  # index for the group we want

coords.1 <- addEllipse(ellipses.posterior[[grp.idx]][draw.idx, 5:6], 
                     matrix(ellipses.posterior[[grp.idx]][draw.idx , 1:4], nrow = 2, ncol = 2),
                     p.interval = 0.95,
                     do.plot = do.plot)

# coordinates of ellipse for group [[3]] = 1.3 (green circles)
grp.idx <- 3  # index for the group we want

coords.2 <- addEllipse(ellipses.posterior[[grp.idx]][draw.idx, 5:6], 
                     matrix(ellipses.posterior[[grp.idx]][draw.idx, 1:4], nrow = 2, ncol = 2),
                     p.interval = 0.95,
                     do.plot = do.plot)

# and then the overlap between the two
# and now we can use the function spatstat::overlap.xypolygon to calculate the 
# overlap, which is expressed in units, in this case permil squared.
overlap <- abs(spatstat::overlap.xypolygon(list(x = coords.1[,1],
                                                y = coords.2[,2]), 
                                           list(x = coords.2[,1],
                                                y = coords.2[,2]) ) )




