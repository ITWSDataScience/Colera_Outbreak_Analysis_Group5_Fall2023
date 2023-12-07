# Load the necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(MASS) 

# Define the custom theme
Rohan_theme <- theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 0, vjust = 1)
  )

# Read and clean the datasets
outbreak_data <- read.csv("outbreak_data_2.csv")
gdp_data <- read.csv("gdp_data_2.csv")

# Merge the reshaped GDP data with the cholera outbreak data on the country code and year
merged_data <- merge(outbreak_data, gdp_data, by.x = c("country", "Year"), by.y = c("Country.Code", "Year"))

# Clean and convert GDP to numeric, handling NAs and non-numeric characters
merged_data$GDP <- as.numeric(gsub("[^0-9\\.]", "", merged_data$GDP))

# Remove rows with NA in the GDP column
merged_data <- merged_data %>% 
  filter(!is.na(GDP))

# Convert the 'Year' column to a Date type if necessary
merged_data$Year <- as.Date(as.character(merged_data$Year), format="%Y")

# Aggregate data by year for cholera cases and GDP
time_series_data <- merged_data %>%
  group_by(Year) %>%
  summarize(total_cases = sum(total_suspected_cases, na.rm = TRUE),
            average_GDP = mean(GDP, na.rm = TRUE))  # Ensure NAs are removed for the mean calculation

# Check for any remaining issues with the data
print(summary(time_series_data))

scale_factor <- max(time_series_data$total_cases) / max(time_series_data$average_GDP)

ggplot(time_series_data, aes(x = Year)) +
  geom_line(aes(y = total_cases, colour = "Cholera Cases")) +
  geom_line(aes(y = average_GDP * scale_factor, colour = "GDP")) +
  scale_y_continuous(
    name = "Total Cholera Cases",
    sec.axis = sec_axis(~ . / scale_factor, name="Average GDP (scaled)")
  ) +
  labs(title = "Cholera Cases and GDP Over Time",
       x = "Time",
       y = "Total Cholera Cases") +
  scale_colour_manual(
    values = c("Cholera Cases" = "blue", "GDP" = "green"),
    name = "",  # Hide the title of the legend
    labels = c("Cholera Cases", "GDP")
  ) +
  Rohan_theme +
  theme(legend.position = "bottom")  

# Shapiro-Wilk test for normality on total cases
shapiro.test(time_series_data$total_cases)

# Shapiro-Wilk test for normality on GDP
shapiro.test(time_series_data$average_GDP)

# Spearman correlation test since data may not be normally distributed
cor.test(time_series_data$average_GDP, time_series_data$total_cases, method = "spearman")

# Use robust linear regression to predict cholera cases based on GDP
rlm_model <- rlm(total_cases ~ average_GDP, data = time_series_data)

# Create a new data frame for GDP values to predict on
prediction_data <- data.frame(average_GDP = seq(min(time_series_data$average_GDP, na.rm = TRUE), 
                                                max(time_series_data$average_GDP, na.rm = TRUE), 
                                                length.out = 100))

# Predict cholera cases based on the model
prediction_data$predicted_cases <- predict(rlm_model, newdata = prediction_data)

# Plot the actual data and the predictions
ggplot(time_series_data, aes(x = average_GDP, y = total_cases)) +
  geom_point() +  # Actual data points
  geom_line(data = prediction_data, aes(x = average_GDP, y = predicted_cases), color = 'red') +  # Regression line
  labs(title = "Predicted Cholera Cases Based on GDP",
       x = "Total GDP (current US$)",
       y = "Predicted Total Cholera Cases") +
  Rohan_theme


