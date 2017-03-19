# number of samples, replications, and strata
n <- 10
M <- 200
k <- 8
m <- M / k


# range of interest: -1 to 1
a <- -1
b <- 1
s_width <- (b - a) / k # width of strata

# standard normal distribution
g_norm <- function(x) {
  x^2 / sqrt(2 * pi) * exp(-x^2 / 2)
}

# containers for estimates
strat <- numeric(8)
mc_entire <- numeric(0)
mc_strat <- numeric(0)

# perform monte carlo integration
for (i in 1:n) {
  # perform for entire range (a, b)
  mc_entire <- c(mc_entire, mean(g_norm(runif(M, a, b))))
  # perform for strata (a, b) / k
  for (j in 1:k) {
    samp <- g_norm(runif(m, a + (j - 1) * s_width, a + j * s_width))
    strat[j] <- mean(samp)
  }
  mc_strat <- c(mc_strat, mean(strat))
}

# get results
var(mc_entire)
var(mc_strat)
