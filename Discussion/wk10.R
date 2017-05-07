# cauchy normal
cauchy <- function(x) {
  return(1 / (pi * (1 + x^2)))
}

M <- 5000
u <- runif(M)

# get x0
x <- rchisq(1, df = 1)

# sample x values
for (i in 2:M) {
  # at t+1, get xt
  xt <- x[length(x)]
  # get y from normal distribution since cauchy is symmetrical
  y <- rnorm(1, mean = xt)
  # get ratio
  r <- (cauchy(y) * dnorm(xt, mean = y)) / (cauchy(xt) * dnorm(y, mean = xt))
  # append xt or y
  x <- c(x, ifelse(u[i] <=r, y, xt))
}

# plot estimates
x_trace <- data.frame(m = 1:M, x = x, b = c(rep('BurnIn', 1000), rep('Keep', 4000)))
library(ggplot2)
ggplot(x_trace, aes(x = m, y = x, col = b)) +
  geom_line(aes(col = b), show.legend = FALSE) +
  geom_hline(yintercept = 0, lty = 3) +
  labs(title = 'Estimation of x', x = NULL) +
  theme_minimal()

# plot histogram vs. density
library(fitdistrplus)
denscomp(fitdist(x, 'cauchy'), plotstyle = 'ggplot', legendtext = 'Cauchy',
         demp = TRUE, dempcol = 'black', fitlty = 1) +
  theme_minimal() + theme(legend.position = 'bottom')
