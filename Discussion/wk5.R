# create linear conguential function ####
lin_cong <- function(n, seed = 123457, a = 16807, c = 0, m = 2147483647) {
  # set up x0 and container for generated numbers
  x <- seed
  r <- numeric(0)
  for (i in 1:n) {
    # generate new x value
    x <- (a * x + c) %% m
    # get new r value and add to list
    r <- c(r, x / m)
  }
  return(r)
}

# generate results for examples ####
n <- 10000
# conditions for each example
conds <- list(
  c(27, 17, 43, 100),
  c(1, 13, 0, 64),
  c(123457, 16807, 0, 2147483647)
  )
# containers for results
rands <- vector('list', 3)

# generation of results
for (j in 1:3) {
  rands[[j]] <- lin_cong(n, conds[[j]][1], conds[[j]][2], conds[[j]][3], conds[[j]][4])
}

# create data frame
df_rand <- data.frame(i = 1:n, Example7.1 = rands[[1]], 
  Example7.2 = rands[[2]], Example7.3 = rands[[3]])
library(tidyr)
df_rand <- df_rand %>% gather(Example, r, -i)

# plot results
library(ggplot2)
ggplot(df_rand, aes(x = i, y = r, col = Example)) + geom_point(alpha = 0.25) +
  theme(legend.position = 'bottom', legend.title = element_blank()) +
  labs(title = 'Generation of random samples from textbook examples',
       subtitle = 'For n = 1000')