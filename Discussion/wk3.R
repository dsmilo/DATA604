library(tidyverse)

# set up provided data
produce <- c('oats', 'peas', 'beans', 'barley')
wholesale <- c(1.05, 3.17, 1.99, 0.95)
retail <- c(1.29, 3.76, 2.23, 1.65)
min_sales <- rep(0, 4)
max_sales <- c(10, 8, 14, 11)
num_days <- 90

# create container list for simulation results
walther <- list()

# create simulation for each list
for (p in 1:length(produce)) {
  # get 90 simulated sales for produce item
  set.seed(42) # set random number generator seed for replicability
  sales <- sample(min_sales[p]:max_sales[p], num_days, replace = TRUE)
  # store day number and calculate revenue, cost, and profit
  df <- data.frame(day_no = 1:num_days, sales) %>%
    mutate(revenue = sales * retail[p],
           cost = sales * wholesale[p],
           profit = revenue - cost)
  # add item name
  df$item <- rep(produce[p], num_days)
  # store in list
  walther[[p]] <- df
}

# collapse list into single data frame (tbl_df)
walther <- tbl_df(bind_rows(walther))

# calculate daily and running sums
daily <- walther %>% 
  group_by(day_no) %>% 
  summarise(revenue = sum(revenue),
            cost = sum(cost),
            profit = sum(profit)) %>%
  mutate(
    running_revenue = cumsum(revenue),
    running_cost = cumsum(cost),
    running_profit = cumsum(profit)
  )

# calculate total revenue, cost, and profit
tot <- daily[nrow(daily), 5:7]
  # revenue: 4357.36
  # cost:    3500.76
  # profilt:  856.60

# get daily and average sales by item
item <- walther %>% 
  group_by(item) %>% 
  mutate(avg_sales = cummean(sales))

# plot results
ggplot(daily, aes(x = day_no)) +
  geom_line(aes(y = running_revenue, col = 'Revenue')) +
  geom_line(aes(y = running_cost, col = 'Cost')) +
  geom_line(aes(y = running_profit, col = 'Profit')) +
  scale_x_continuous('Day Number') + scale_y_continuous(NULL) +
  scale_color_discrete(NULL, breaks = c('Revenue', 'Cost', 'Profit')) +
  theme(legend.position = c(0.1, 0.9), legend.justification = c(0,1)) +
  labs(title = 'Running totals of revenue, profit, and cost',
       subtitle = 'Across all items over 90 days')

library(scales)
ggplot(item, aes(x = day_no)) +
  geom_col(aes(y = sales), alpha = 0.5) +
  geom_line(aes(y = avg_sales, col = item), show.legend = FALSE) +
  facet_wrap(~item, scales = 'free_y') +
  scale_y_continuous('Demand (lb)', breaks = pretty_breaks()) +
  scale_x_continuous('') +
  labs(title = 'Daily sales for each item available',
       caption = 'Bars = Daily Value; Lines = Average Value')
