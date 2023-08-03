# Load necessary libraries
library(dplyr)
library(ggplot2)

setwd("~/buckets/b2/recuperatorio_2/uba2022/")

# Read the data
exp_12_gains_015 <- read.csv("exp_12_gains_015.csv")
exp_12_gains_02 <- read.csv("exp_12_gains_02.csv")
exp_12_gains_03 <- read.csv("exp_12_gains_03.csv")

# Rename the columns to include split information
names(exp_12_gains_015) <- paste0(names(exp_12_gains_015), "_015")
names(exp_12_gains_02) <- paste0(names(exp_12_gains_02), "_02")
names(exp_12_gains_03) <- paste0(names(exp_12_gains_03), "_03")

# Rename fraction columns to 'fraction'
names(exp_12_gains_015)[1] <- 'fraction'
names(exp_12_gains_02)[1] <- 'fraction'
names(exp_12_gains_03)[1] <- 'fraction'

# Merge all datasets by 'fraction'
combined_data <- full_join(exp_12_gains_015, exp_12_gains_02, by = "fraction")
combined_data <- full_join(combined_data, exp_12_gains_03, by = "fraction")

# Create average columns
combined_data <- combined_data %>%
  mutate(avg_sum_wins = rowMeans(select(., starts_with("sum_of_wins")), na.rm = TRUE),
         avg_diff = rowMeans(select(., starts_with("diff")), na.rm = TRUE))

# Write the combined data into a csv file
write.csv(combined_data, file = "combined_data.csv")

# Analyze the data
max_avg_sum_wins <- which.max(combined_data$avg_sum_wins)
max_avg_diff <- which.max(combined_data$avg_diff)

# Print results
print(paste0("Max avg sum of wins at fraction: ", combined_data$fraction[max_avg_sum_wins]))
print(paste0("Max avg diff at fraction: ", combined_data$fraction[max_avg_diff]))

# Plotting
ggplot(combined_data, aes(x = fraction)) +
  geom_line(aes(y = avg_sum_wins), color = "blue") +
  geom_line(aes(y = avg_diff), color = "red") +
  labs(title = "Average Sum of Wins and Diff over Fractions", 
       x = "Fraction", y = "Average Value",
       color = "Metric") +
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("Average Sum of Wins", "Average Diff")) +
  theme_minimal()