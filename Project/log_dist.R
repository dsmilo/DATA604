region <- matrix(rep(NA, 100), nrow = 10)

region[4, 4] <- 'mill'
region[4, 6] <- 'mill'
region[6, 5] <- 'mill'
region[c(4, 4, 4), c(3, 5, 7)] <- 'bald'
region[c(3, 3), c(4, 6)] <- 'bald'
region[c(5, 5, 5), c(4, 5, 6)] <- 'bald'
region[c(6, 6), c(4, 6)] <- 'bald'
region[7, 5] <- 'bald'


cl = floor(which(region == 'mill') / ncol(region)) + 1
rw = which(region == 'mill') %% ncol(region)

dis <- numeric(0)
region_num <- region

for (i in 1:nrow(region)) {
  for (j in 1:ncol(region)) {
    if (is.na(region[i, j])) {
      closest <- min(sqrt((rw - i)^2 + (cl - j)^2)) * 10
      dis <- c(dis, closest)
      region_num[i, j] <- closest
    }
  }
}

p_tbl <- round(table(round(dis, 2)) / length(dis), 2)

p_df <- data.frame(p = names(p_tbl),
                   c = as.vector(unname(p_tbl)),
                   stringsAsFactors = FALSE)
p_df$p <- as.numeric(p_df$p)

simio_probs <- as.numeric(c(rbind(names(p_tbl), cumsum(unname(p_tbl)))))
paste(simio_probs, collapse = ', ')

e_p <- sum(p_df$p * p_df$c)

region_num <- ifelse(region_num == 'mill', 0,
                     ifelse(region_num == 'bald', NA, region_num))
region_num <- matrix(readr::parse_number(region_num), nrow = 10)

image(t(region_num), ylim = c(1, 0), col = viridis::viridis(nrow(p_df)),
      main = 'Distance from logging operations to nearest mill')
filled.contour(t(region_num), ylim = c(1, 0), color.palette = viridis::viridis)
