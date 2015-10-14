# query about calculating minimum distance from a point to a convex hull
# from https://twitter.com/ipnosimmia/status/654040246951084032

# ------------------------------------------------------------------------------
set.seed(1)
rm(list=ls())

# ------------------------------------------------------------------------------
# Define the function for calculating distance between a point (x0, y0) and a
# line that passes between two points P1=(y1, y1) and P2=(x2, y2) from
# https://en.m.wikipedia.org/wiki/Distance_from_a_point_to_a_line#Line_defined_by_two_points

# AJ - I need to adjust this to work on line segments, not just lines
# http://stackoverflow.com/questions/849211/shortest-distance-between-a-point-and-a-line-segment

distPoint2Points <- function(x0, y0, x1, y1, x2, y2){
  
  num <- abs( (y2 - y1) * x0 - (x2 - x1) * y0 + (x2 * y1) - (y2 * x1) )
  
  denom <- sqrt( (y2 - y1) ^2 + (x2 - x1) ^2 )
  
  d <- num / denom
  
}

# should check some examples to make sure this fucntion is working....
# here is one quick check.. should do more...

test <- distPoint2Points(5, 10, 0, 0, 10, 0 ) # should be = 10
print(test) # phew!

# ------------------------------------------------------------------------------
# create some data to illustrate

# a random cloud of data on which to calculate a convex hull
n <- 30 # number of data points (must be divisible by 2)
Z <- matrix(rnorm(n), n/2, 2)

# which points form vertices of the hull?
Z.hull <- chull(Z)

# extract these points as a sequence
# remembering to duplicate the first one since last joints first.
P <- Z[c(Z.hull, Z.hull[1]), ]

# a random point for reference which could be inside or outside the hull
x0 <- runif(1, 3, 10)
y0 <- runif(1, 3, 10)

# plot it
plot(Z[,1], Z[,2], pch = 19, col = "grey", xlim = c(-2.5, 10), ylim = c(-2.5, 10))
points(x0, y0, pch = "x", cex = 2, lwd = 2)
# add the hull
lines(P[,1], P[,2], lty = 2)

# ------------------------------------------------------------------------------
# calculate distance to each line segment of the hull
m <- nrow(P) # number of unique points in hull vertices set
d <- distPoint2Points(x0, y0, P[1:(m-1), 1], P[1:(m-1), 2], P[2:m, 1], P[2:m, 2])

min.index <- which.min(d)
min.d <- d[min.index]

print(min.d)

# indicate which segment is the nearest on the graph
points(P[min.index, 1], P[min.index, 2], pch = 19 )
points(P[ (min.index + 1), 1], P[ (min.index + 1), 2], pch = 19 )




