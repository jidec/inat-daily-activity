obs_data <- readRDS("D:/GitProjects/inat-daily-activity-analysis/_targets/objects/obs_data_gridded")

obs_data <- subset(obs_data, select = -c(geometry))
sscs <- obs_data %>% group_by(species,season,cell) %>% summarize(obs=list(local_hour),n=n())

library(ggplot2)

ggplot()
sscs$obs[[1]]


# Loading the required library
library(ggplot2)

# Generating example numeric data representing timestamps (in hours)
set.seed(123) # Setting seed for reproducibility
data <- rnorm(1000, mean = 12, sd = 3) # Mean at 12, standard deviation 3, 1000 data points

# Converting data to hours (assuming they are in a continuous range, like time of the day)
data_hours <- data %% 24

sscs$obs[[30]]

sscs$obs[[5124]]

hours <- sscs$obs[[31105]]
hours <- hours[hours>=8]
hours <- hours[hours <= 20]

quantile_10 <- quantile(hours, probs = 0.1)
quantile_90 <- quantile(hours, probs = 0.9)
median_val <- median(hours)

# Creating a density plot using ggplot2
plot <- ggplot() +
  geom_density(aes(x = hours), fill = "grey", alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 24, by = 1)) +
  geom_vline(aes(xintercept = quantile_10), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = quantile_90), color = "green", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median_val), color = "orange", linetype = "dashed", size = 1) +
  #geom_text(aes(x = quantile_10, label = "ONSET", y = 0.02), vjust = 0, hjust = 0, color = "red") +
  #geom_text(aes(x = median_val, label = "PEAK", y = 0.02), vjust = 0, hjust = 0, color = "orange") +
  #geom_text(aes(x = quantile_90, label = "OFFSET", y = 0.02), vjust = 0, hjust = 0, color = "green") +
  labs(title = "Density Plot of Numeric Data Binned into Hours",
       x = "Hour",
       y = "Density") +
  xlim(8,20) +
  theme_minimal()

# Displaying the plot
print(plot)

