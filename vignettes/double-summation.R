## ------------------------------------------------------------------------
# simulate data
smpl <- 30

n <- 4
a <- sample(1:20, n)
X <- lapply(rep(smpl, n), function(x) {rnorm(x)})

m <- 3
b <- sample(1:20, m)
Y <- lapply(rep(smpl, m), function(y) {rnorm(y)})

# left-hand side calculation
aX <- 0
for(i in 1:n) {
  aX <- aX + a[i]*X[[i]]
}

bY <- 0
for(j in 1:m) {
  bY <- bY + b[j]*Y[[j]]
}

lhs_cov <- cov(aX, bY)

# right hand side calculation
rhs_cov <- 0

for(i in 1:n) {
  for(j in 1:m) {
    rhs_cov <- rhs_cov + (a[i]*b[j]*cov(X[[i]], Y[[j]]))
  }
}

# should be equal
c(lhs_cov, rhs_cov)

