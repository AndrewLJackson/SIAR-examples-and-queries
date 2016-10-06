library(siar)


# sources
n <- 3

# reps
m <- 10^3

a <- rep(1, n) / (n)

p <- matrix(0, nrow = m, ncol = n)


for (i in 1:m){
  
  p[i,] <- rdirichlet(a)
  
}


## put histograms on the diagonal
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
pairs(p, diag.panel = panel.hist)

