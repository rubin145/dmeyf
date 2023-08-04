# Load necessary libraries
library(dplyr)
library(ggplot2)
library(scales)

setwd("~/buckets/b2/recuperatorio_2/uba2022/")

# Read the data
exp_12_015_gains <- read.csv("exp_12_015_gains.csv")
exp_12_02_gains <- read.csv("exp_12_02_gains.csv")
exp_12_03_gains <- read.csv("exp_12_03_gains.csv")

# Rename the columns to include split information
names(exp_12_015_gains) <- paste0(names(exp_12_015_gains), "_015")
names(exp_12_02_gains) <- paste0(names(exp_12_02_gains), "_02")
names(exp_12_03_gains) <- paste0(names(exp_12_03_gains), "_03")

# Rename fraction columns to 'fraction'
names(exp_12_015_gains)[1] <- 'fraction'
names(exp_12_02_gains)[1] <- 'fraction'
names(exp_12_03_gains)[1] <- 'fraction'

# Merge all datasets by 'fraction'
combined_data <- full_join(exp_12_015_gains, exp_12_02_gains, by = "fraction")
combined_data <- full_join(combined_data, exp_12_03_gains, by = "fraction")

# Create average columns
combined_data <- combined_data %>%
  mutate(avg_sum_wins = rowMeans(select(., starts_with("sum_of_wins")), na.rm = TRUE),
         avg_diff = rowMeans(select(., starts_with("diff")), na.rm = TRUE))

# Write the combined data into a csv file
write.csv(combined_data, file = "combined_data_exp_12_.csv")

# Analyze the data
max_avg_sum_wins <- which.max(combined_data$avg_sum_wins)
max_avg_diff <- which.max(combined_data$avg_diff)

# Print results
print(paste0("Max avg sum of wins at fraction: ", combined_data$fraction[max_avg_sum_wins]))
print(paste0("Max avg diff at fraction: ", combined_data$fraction[max_avg_diff]))

# Plotting
# Load necessary libraries
library(scales)

# Create a dual axis plot
ggplot() +
  geom_line(data = combined_data, aes(x = fraction, y = avg_sum_wins, color = "wins")) +
  geom_line(data = combined_data, aes(x = fraction, y = avg_diff / 1e6, color = "diff")) +
  scale_y_continuous(name = "Average Sum of Wins", 
                     limits = c(0, 100),
                     sec.axis = sec_axis(~ . * 1e6, name = "Average Diff", 
                                         labels = function(x) paste0(x/1e6, "M"))) +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "Average Sum of Wins and Diff over Fractions", 
       x = "Fraction", 
       color = "Metric") +
  theme_minimal()

ggplot(data = combined_data, aes(x = fraction, y = avg_sum_wins)) +
  geom_line(color = "blue") +
  labs(title = "Average Sum of Wins over Fractions", 
       x = "Fraction", 
       y = "Average Sum of Wins") +
  theme_minimal()

ggplot(data = combined_data, aes(x = fraction, y = avg_diff)) +
  geom_line(color = "red") +
  labs(title = "Average Sum of Wins over Fractions", 
       x = "Fraction", 
       y = "Average diff") +
  theme_minimal()

# Function to create plots for sum_of_wins and diff values
create_plots <- function(fn_or_ft) {
  # Load necessary libraries
  library(ggplot2)
  
  # Prepare data for plotting
  plot_data <- combined_data %>%
    select(fraction, starts_with(paste0("sum_of_wins_", fn_or_ft)), starts_with(paste0("diff_", fn_or_ft))) %>%
    tidyr::gather(key = "Variable", value = "Value", -fraction)
  
  # Create plot
ggplot() +
  geom_line(data = combined_data, aes(x = fraction, y = avg_sum_wins, color = "Average Sum of Wins")) +
  geom_line(data = combined_data, aes(x = fraction, y = avg_diff / 10000, color = "Average Diff")) +
  scale_y_continuous(name = "Average Sum of Wins", 
                     sec.axis = sec_axis(~ . * 10000, name = "Average Diff")) +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "Average Sum of Wins and Diff over Fractions", 
       x = "Fraction", 
       color = "Metric") +
  theme_minimal()
}

# Call the function for 'fn' and 'ft'
create_plots("fn")
create_plots("ft")


library(tidyr)

# Reshape the data
# Reshape the data
# Reshape the data
long_data <- combined_data %>%
  pivot_longer(cols = c(sum_of_wins_fn_015, sum_of_wins_fn_02, sum_of_wins_fn_03), 
               names_to = "split", 
               values_to = "wins")

# Convert the column names to a readable format for legend
long_data$split <- recode(long_data$split,
                          sum_of_wins_fn_015 = "0.15",
                          sum_of_wins_fn_02 = "0.2",
                          sum_of_wins_fn_03 = "0.3")

# Plot the data
ggplot(long_data, aes(x = fraction, y = wins, color = split)) +
  geom_line() +
  labs(title = "Sum of Wins over Fractions by Split", 
       x = "Fraction", 
       y = "Sum of Wins", 
       color = "Split") +
  scale_color_discrete(name = "Fraction split") +
  theme_minimal()




