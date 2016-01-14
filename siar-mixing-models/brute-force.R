# A basic isosoure model to test brian Fry's example

# source isotope values
A <- c(-5, -5)
B <- c(-5,  5)
C <- c( 5,  5)
D <- c( 5, -5)

# consumer
X <- c(0, 0)

# source proportions to try by brute force
pA <- seq(0, 0.5, by = 0.05)
pB <- pA
pC <- pA
pD <- pA

# results
results <- matrix(NA, nrow = length(pA)^4, ncol = 4)

ct <- 1

for (iA in pA) {
  
  for (iB in pB) {
    
    for (iC in pC) {
      
      for (iD in pD) { 
        
        if (sum(c(iA,iB,iC,iD)) == 1 ) {
          
          
          M1 <- A[1]*iA + B[1]*iB + C[1]*iC + D[1]*iD 
          M2 <- A[2]*iA + B[2]*iB + C[2]*iC + D[2]*iD
          
          if (sum(X - c(M1,M2)) <= 0.01) {
            results[ct,] <- c(iA, iB, iC, iD)
            ct <- ct + 1
          }
          
        }
        
      }
    }
  }
}

pairs(results,
      lower.panel = panel.smooth, 
      upper.panel = panel.cor, 
      diag.panel = panel.hist)

print(base::colMeans(results, na.rm=TRUE))
