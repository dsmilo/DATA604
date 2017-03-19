# set up containers for means and mins
samp_mean <- numeric(0)
samp_min  <- numeric(0)

# generate 100 simulations
for (i in 1:100) {
  # sample the exponential distribtion 30 times
  samp_results <- rexp(30)
  # store mean and minimum results
  samp_mean <- c(samp_mean, mean(samp_results))
  samp_min  <- c(samp_min, min(samp_results))
}

# store results and tidy data for neater plotting
samp <- data.frame(mean = samp_mean, min = samp_min)
library(tidyr)
samp <- samp %>% gather(measure, value)

#plot results
library(ggplot2)
library(scales)
ggplot(samp, aes(x = value, y = (..count..) / sum(..count..))) + 
  geom_histogram(bins = 25, alpha = 0.5, col = 'black') + 
  facet_wrap(~measure, ncol = 1, scales = "free_y") + 
  scale_y_continuous('', labels = percent) + scale_x_continuous('') +
  labs(title = 'Distribution of Sample Means and Minimums',
       subtitle = '100 Simulations of 30 exponential distribution samples')
